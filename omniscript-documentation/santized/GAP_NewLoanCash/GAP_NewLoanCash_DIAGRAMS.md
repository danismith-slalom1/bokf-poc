# GAP_NewLoanCash - Diagrams and Visualizations

**Program**: GAP_NewLoanCash.cbl  
**Last Updated**: February 4, 2026  
**Purpose**: Visual representations of program logic, data flow, and system interactions

---

## Table of Contents

1. [State Machine Diagram](#state-machine-diagram)
2. [Sequence Diagram](#sequence-diagram)
3. [Data Flow Diagram](#data-flow-diagram)
4. [Entity Relationship Diagram](#entity-relationship-diagram)

---

## State Machine Diagram

### Program Execution States

This diagram shows the lifecycle states of the program execution and position record processing.

```mermaid
stateDiagram-v2
    [*] --> Initializing: Program Start
    
    Initializing --> DateValidation: Variables Set
    DateValidation --> DateValid: RunDate Valid
    DateValidation --> DateFallback: RunDate Invalid
    DateValid --> QueryingDB: Dates Calculated
    DateFallback --> QueryingDB: Fallback Dates Set
    
    QueryingDB --> ProcessingRecords: poppobj_view() Success
    QueryingDB --> Error: Database Failure
    
    ProcessingRecords --> ReadingRecord: poppobj_next()
    ReadingRecord --> CheckingActivity: Record Data Loaded
    
    CheckingActivity --> ValidatingSSA: Secondary1Buys > 0
    CheckingActivity --> CheckingIdempotency: Secondary1Buys = 0
    
    ValidatingSSA --> QueryingSSA: Call CHECK.SSSA
    QueryingSSA --> CalculatingNet: SSSA Records Found
    QueryingSSA --> CheckingIdempotency: No SSSA Records
    
    CalculatingNet --> CheckingIdempotency: Net Amount Calculated
    
    CheckingIdempotency --> GeneratingC1: Not Processed Yet
    CheckingIdempotency --> ProcessingRecords: Already Processed
    
    GeneratingC1 --> WritingFile: C1 Record Built
    WritingFile --> UpdatingDB: File Write Success
    WritingFile --> Error: File Write Failure
    
    UpdatingDB --> ProcessingRecords: Position Updated
    UpdatingDB --> Error: Update Failure
    
    ProcessingRecords --> Completed: No More Records
    
    Completed --> [*]: Program End
    Error --> [*]: Abnormal Exit
    
    note right of ValidatingSSA
        Optimization: Skip SSSA
        query if no activity
    end note
    
    note right of CheckingIdempotency
        Idempotency: Skip if
        already processed
    end note
```

---

## Sequence Diagram

### Main Program Flow with CHECK.SSSA Interaction

This diagram shows the interaction between the main program, databases, and the CHECK.SSSA routine.

```mermaid
sequenceDiagram
    actor Scheduler
    participant Main as GAP_NewLoanCash<br/>(Main Program)
    participant CheckSSA as CHECK.SSSA<br/>(Routine)
    participant PoppDB as Position Objects<br/>(poppobj)
    participant SSADB as Settlement Activity<br/>(sssaobj)
    participant File as Output File<br/>(.DAT)
    
    Scheduler->>Main: Execute Program
    activate Main
    
    Main->>Main: Initialize (sd080, FileName)
    Main->>Main: Calculate Date Range
    Main->>File: Open Output File
    activate File
    
    Main->>PoppDB: poppobj_view(POOLLOAN3, date range)
    activate PoppDB
    
    loop For Each Position Record
        PoppDB-->>Main: poppobj_next() → Record Data
        Main->>Main: Read: RKPlan, TradeDate,<br/>Secondary1Buys, PriorCashApplied
        
        alt Secondary1Buys > 0
            Main->>CheckSSA: PERFORM 'CHECK.SSSA'
            activate CheckSSA
            
            CheckSSA->>CheckSSA: Validate Parameters
            CheckSSA->>CheckSSA: WK001 = 0
            
            CheckSSA->>SSADB: sssaobj_view(Plan, POOLLOAN3, Date)
            activate SSADB
            
            loop For Each SSSA Record
                SSADB-->>CheckSSA: sssaobj_next() → Transaction
                
                alt Transaction Type = 'XI'
                    alt Buy/Sell = 'B'
                        CheckSSA->>CheckSSA: WK001 += Amount (BUY)
                    else Buy/Sell = 'S'
                        CheckSSA->>CheckSSA: WK001 -= Amount (SELL)
                    end
                end
            end
            
            deactivate SSADB
            CheckSSA->>Main: Secondary1Buys = WK001
            deactivate CheckSSA
        end
        
        alt PriorCashApplied ≠ Secondary1Buys AND Secondary1Buys ≠ 0
            Main->>Main: NewLoanUnits = -Secondary1Buys
            Main->>Main: Build C1 Record
            Main->>File: Write C1 Record
            File-->>Main: Write Success
            
            Main->>PoppDB: poppobj_setde(877, Secondary1Buys)
            Main->>PoppDB: poppobj_update()
            PoppDB-->>Main: Update Success
        else Already Processed or Zero Amount
            Note over Main: Skip - No C1 Record Needed
        end
    end
    
    deactivate PoppDB
    Main->>File: Close (Implicit)
    deactivate File
    
    Main->>Scheduler: Exit (sd080)
    deactivate Main
```

---

## Data Flow Diagram

### Level 0: System Context

```mermaid
flowchart TB
    subgraph External Systems
        ENV[Environment<br/>Variables<br/>$XDAT, $RUN-DATE]
        TRUST[Trust Transaction<br/>System<br/>TRUSTTRANS.P1]
        SETTLE[Settlement<br/>System<br/>SSSA Source]
    end
    
    subgraph GAP_NewLoanCash Program
        PROG[GAP_NewLoanCash<br/>Batch Process]
    end
    
    subgraph Data Stores
        POPP[(Position Objects<br/>poppobj<br/>POOLLOAN3)]
        SSSA[(Secondary Settlement<br/>Activity<br/>sssaobj)]
    end
    
    subgraph Output Systems
        C1FILE[C1 Activity File<br/>*.DAT<br/>Cash Reconciliation]
        CASHREC[Cash Reconciliation<br/>System]
    end
    
    ENV -->|Configuration| PROG
    TRUST -->|Loads| POPP
    SETTLE -->|Loads| SSSA
    
    PROG -->|Query<br/>7-Day Window| POPP
    PROG -->|Validate<br/>Amounts| SSSA
    PROG -->|Update<br/>UDF1| POPP
    PROG -->|Generate<br/>C1 Records| C1FILE
    
    C1FILE -->|Batch Load| CASHREC
    
    style PROG fill:#4CAF50,color:#fff
    style POPP fill:#2196F3,color:#fff
    style SSSA fill:#2196F3,color:#fff
    style C1FILE fill:#FF9800,color:#fff
```

### Level 1: Detailed Data Flow

```mermaid
flowchart TB
    subgraph Inputs
        RUNDATE[/$RUN-DATE<br/>Environment/]
        XDAT[/$XDAT<br/>Directory/]
        POPPDATA[(Position Data<br/>POOLLOAN3<br/>7-Day Window)]
        SSADATA[(SSSA Data<br/>Loan Transactions)]
    end
    
    subgraph Process 1: Initialization
        P1A[Validate<br/>Run Date]
        P1B[Calculate<br/>Date Range]
        P1C[Build<br/>Filename]
        P1D[Open<br/>Output File]
    end
    
    subgraph Process 2: Query Positions
        P2A[Query<br/>poppobj]
        P2B[Read Position<br/>Fields]
    end
    
    subgraph Process 3: Validate Amount
        P3A{Secondary1Buys<br/>> 0?}
        P3B[Query<br/>sssaobj]
        P3C[Net<br/>BUYS - SELLS]
        P3D[Update<br/>Secondary1Buys]
    end
    
    subgraph Process 4: Generate C1
        P4A{Already<br/>Processed?}
        P4B[Calculate<br/>NewLoanUnits]
        P4C[Build<br/>C1 Record]
        P4D[Write<br/>to File]
    end
    
    subgraph Process 5: Mark Processed
        P5A[Set<br/>UDF1]
        P5B[Update<br/>Position]
    end
    
    subgraph Outputs
        C1OUT[C1 Activity File<br/>Fixed Format<br/>138 chars]
        POPPUPDATE[(Updated<br/>Position Objects<br/>UDF1 Marked)]
    end
    
    RUNDATE --> P1A
    P1A --> P1B
    P1B --> P2A
    XDAT --> P1C
    P1C --> P1D
    
    POPPDATA --> P2A
    P2A --> P2B
    P2B --> P3A
    
    P3A -->|Yes| P3B
    P3A -->|No| P4A
    SSADATA --> P3B
    P3B --> P3C
    P3C --> P3D
    P3D --> P4A
    
    P4A -->|Not Processed| P4B
    P4A -->|Already Done| P2A
    P4B --> P4C
    P4C --> P4D
    P4D --> P5A
    
    P5A --> P5B
    P5B --> POPPUPDATE
    
    P4D --> C1OUT
    
    style P3A fill:#FFF9C4
    style P4A fill:#FFF9C4
    style C1OUT fill:#C8E6C9
    style POPPUPDATE fill:#FFCCBC
```

---

## Entity Relationship Diagram

### Database Objects and Relationships

```mermaid
erDiagram
    POSITION-OBJECT ||--o{ SSSA-RECORD : "has settlement activity for"
    POSITION-OBJECT ||--o| C1-RECORD : "generates"
    
    POSITION-OBJECT {
        string RKPlan PK
        date TradeDate PK
        string SecurityID
        numeric Secondary1Buys "DE 741 - Initial loan amount"
        numeric PriorCashApplied "DE 877 - UDF1 idempotency flag"
        string TrustAccount "DE 01510 - Trust account ID"
    }
    
    SSSA-RECORD {
        string Plan FK
        string SecurityID FK
        date Date FK
        string TransactionType "DE 011 - XI for loans"
        string BuySellIndicator "DE 009 - B or S"
        numeric Amount "DE 235 - Transaction amount"
    }
    
    C1-RECORD {
        string RecordType "C100"
        string RKPlan FK
        date ProcessDate "LastBusiness"
        string TrustAccount
        numeric NewLoanUnits "Negative of net buys"
        string TransactionCode "00339"
        string Position92 "Value: 2"
    }
```

### Data Element Cross-Reference

```mermaid
flowchart LR
    subgraph Position Object
        DE030[DE 030<br/>RKPlan]
        DE008[DE 008<br/>TradeDate]
        DE741[DE 741<br/>Secondary1Buys]
        DE877[DE 877<br/>PriorCashApplied]
        DE1510[DE 01510<br/>TrustAccount]
    end
    
    subgraph SSSA Object
        DE011[DE 011<br/>Transaction Type]
        DE009[DE 009<br/>Buy/Sell]
        DE235[DE 235<br/>Amount]
    end
    
    subgraph C1 Record Fields
        C1TYPE[Pos 1-4<br/>C100]
        C1PLAN[Pos 5-10<br/>RKPlan]
        C1DATE[Pos 31-38<br/>LastBusiness]
        C1ACCT[Pos 40-71<br/>TrustAccount]
        C1POS92[Pos 73-92<br/>Position Indicator]
        C1UNITS[Pos 116-130<br/>NewLoanUnits]
        C1CODE[Pos 134-138<br/>00339]
    end
    
    DE030 -->|Read| C1PLAN
    DE008 -->|Filter SSSA| DE011
    DE741 -->|Initial Value| DE235
    DE235 -->|Net Calculation| C1UNITS
    DE877 -->|Idempotency Check| C1UNITS
    DE1510 -->|Copy| C1ACCT
    
    style DE741 fill:#FFEB3B
    style DE877 fill:#FF9800
    style C1UNITS fill:#4CAF50,color:#fff
```

---

## Business Process Diagram

### Loan Cash Reconciliation Workflow

```mermaid
flowchart TB
    START([Daily Batch<br/>Processing Begins]) --> TRUSTLOAD[Trust Transaction<br/>System Loads<br/>TRUSTTRANS.P1]
    
    TRUSTLOAD --> POSLOAD[Position Objects<br/>Updated with<br/>POOLLOAN3 Records]
    
    POSLOAD --> SSALOAD[Settlement System<br/>Loads SSSA Records<br/>with BUY/SELL Data]
    
    SSALOAD --> TRIGGER[Scheduler Triggers<br/>GAP_NewLoanCash]
    
    TRIGGER --> PROCESS[GAP_NewLoanCash<br/>Processes 7-Day Window]
    
    PROCESS --> C1GEN{C1 Records<br/>Generated?}
    
    C1GEN -->|Yes| C1FILE[C1 Activity File<br/>Created with Timestamp]
    C1GEN -->|No| NOACTIVITY[No New Loan Activity<br/>or Already Processed]
    
    C1FILE --> CASHLOAD[Cash Reconciliation<br/>System Loads C1 File]
    
    CASHLOAD --> RECON[Cash Reconciliation<br/>Processing<br/>Left Side: Trust Cash<br/>Right Side: C1 Activity]
    
    RECON --> BALANCED{Reconciliation<br/>Balanced?}
    
    BALANCED -->|Yes| SUCCESS([Reconciliation<br/>Complete])
    BALANCED -->|No| INVESTIGATE[Investigate<br/>Discrepancies]
    
    INVESTIGATE --> REVIEW[Review SSSA Data<br/>Check for Missing<br/>Transactions]
    
    REVIEW --> RERUN{Need to<br/>Re-run?}
    
    RERUN -->|Yes - Reset UDF1| PROCESS
    RERUN -->|No - Manual Adjustment| ADJUST[Manual Cash<br/>Adjustment Entry]
    
    ADJUST --> SUCCESS
    NOACTIVITY --> SUCCESS
    
    style PROCESS fill:#4CAF50,color:#fff
    style C1FILE fill:#FF9800,color:#fff
    style BALANCED fill:#FFF9C4
    style SUCCESS fill:#C8E6C9
```

---

## Integration Architecture

### System Integration Points

```mermaid
C4Context
    title System Context: GAP_NewLoanCash in Enterprise Architecture
    
    Person(scheduler, "Batch Scheduler", "Automated job scheduler")
    Person(analyst, "Cash Analyst", "Reviews reconciliation results")
    
    System(gapnewloan, "GAP_NewLoanCash", "Generates C1 activity records for loan cash reconciliation")
    
    System_Ext(trustsys, "Trust Transaction System", "Source of loan position data (TRUSTTRANS.P1)")
    System_Ext(settlesys, "Settlement System", "Source of secondary market buy/sell transactions")
    
    SystemDb(poppdb, "Position Objects", "poppobj - Plan position accounts")
    SystemDb(ssadb, "Secondary Settlement", "sssaobj - Settlement activity records")
    
    System_Ext(cashrecon, "Cash Reconciliation System", "Processes C1 activity for accounting")
    
    Rel(scheduler, gapnewloan, "Executes daily", "cron/scheduler")
    Rel(analyst, cashrecon, "Reviews reconciliation", "Web UI")
    
    Rel(trustsys, poppdb, "Loads position data", "Batch ETL")
    Rel(settlesys, ssadb, "Loads settlement data", "Batch ETL")
    
    Rel(gapnewloan, poppdb, "Queries & updates", "OMNISCRIPT DB API")
    Rel(gapnewloan, ssadb, "Queries", "OMNISCRIPT DB API")
    Rel(gapnewloan, cashrecon, "Writes C1 file", "Flat file")
    
    UpdateLayoutConfig($c4ShapeInRow="3", $c4BoundaryInRow="2")
```

---

## Performance Visualization

### Processing Volume by Phase

```mermaid
graph LR
    subgraph Input Volume
        A[Position Records<br/>in 7-Day Window<br/>~500-2000 records]
    end
    
    subgraph Filter 1: Security Type
        B[POOLLOAN3 Only<br/>~20-30%<br/>100-600 records]
    end
    
    subgraph Filter 2: Activity Check
        C[Secondary1Buys > 0<br/>~50-70%<br/>50-420 records]
    end
    
    subgraph SSSA Validation
        D[CHECK.SSSA Called<br/>~50-420 times<br/>DB Query per Record]
    end
    
    subgraph Filter 3: Idempotency
        E[Not Yet Processed<br/>First Run: 100%<br/>Re-run: ~0%<br/>50-420 records]
    end
    
    subgraph Output Volume
        F[C1 Records Generated<br/>First Run: 50-420<br/>Re-run: ~0]
    end
    
    A -->|Filter| B
    B -->|Check| C
    C -->|Validate| D
    D -->|Filter| E
    E -->|Generate| F
    
    style A fill:#E3F2FD
    style C fill:#FFF9C4
    style D fill:#FFCCBC
    style F fill:#C8E6C9
```

---

## Error State Diagram

### Error Handling State Transitions

```mermaid
stateDiagram-v2
    [*] --> Running: Program Start
    
    Running --> CheckRunDate: Validate $RUN-DATE
    CheckRunDate --> ValidDate: Date Valid
    CheckRunDate --> FallbackDate: Date Invalid
    
    ValidDate --> OpenFile: Calculate Dates
    FallbackDate --> OpenFile: Use Current Date
    
    OpenFile --> QueryDB: File Opened
    OpenFile --> FileError: Open Failed
    
    QueryDB --> Processing: Query Success
    QueryDB --> DBError: Query Failed
    
    Processing --> ReadRecord: Loop Active
    ReadRecord --> ProcessRecord: Record Read
    
    ProcessRecord --> WriteC1: Generate C1
    ProcessRecord --> SkipRecord: Already Processed
    
    WriteC1 --> UpdatePosition: Write Success
    WriteC1 --> WriteError: Write Failed
    
    UpdatePosition --> ReadRecord: Update Success
    UpdatePosition --> UpdateError: Update Failed
    
    SkipRecord --> ReadRecord: Continue Loop
    ReadRecord --> Completed: No More Records
    
    Completed --> [*]: Exit Success
    FileError --> [*]: Exit Failure
    DBError --> [*]: Exit Failure
    WriteError --> [*]: Exit Failure
    UpdateError --> [*]: Exit Failure
    
    note right of FallbackDate
        Recoverable Error
        Processing Continues
    end note
    
    note right of FileError
        Critical Error
        Cannot Continue
    end note
    
    note right of SkipRecord
        Normal Path
        Idempotency Working
    end note
```

---

## Notes

All diagrams follow Mermaid syntax and can be rendered in GitHub, GitLab, and most Markdown viewers with Mermaid support.

For diagram updates or modifications, edit the Mermaid code blocks directly and preview in a Mermaid-compatible viewer.

**Review Status**: Generated by automated analysis - diagrams represent inferred logic and should be validated by OmniScript experts

**Generated**: February 4, 2026
