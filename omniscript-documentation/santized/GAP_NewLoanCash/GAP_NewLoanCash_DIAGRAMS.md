# GAP_NewLoanCash Diagrams and Visualizations

## Program: GAP_NewLoanCash
**Purpose**: Visual representations of program flow, data flow, and architecture  
**Last Updated**: 2026-02-03

---

## 1. High-Level Process Flow

```mermaid
flowchart TD
    Start([Program Start]) --> Init[Initialize Environment<br/>- Set error context<br/>- Build output filename<br/>- Open file]
    Init --> GetDates[Calculate Date Range<br/>- Get RunDate from env<br/>- Calculate SevenDaysAgo<br/>- Calculate LastBusiness]
    GetDates --> QueryPos[Query POOLLOAN3 Positions<br/>Date Range: SevenDaysAgo to LastBusiness]
    QueryPos --> HasRec{More<br/>Records?}
    
    HasRec -->|No| EndProg([Program End])
    HasRec -->|Yes| Extract[Extract Position Data<br/>- RKPlan<br/>- TradeDate<br/>- Secondary1Buys<br/>- PriorCashApplied<br/>- TrustAccount]
    
    Extract --> CheckAmt{Secondary1Buys<br/>> 0?}
    CheckAmt -->|Yes| CallRev[Call CHECK.SSSA<br/>Reversal Detection]
    CheckAmt -->|No| CheckIdem
    CallRev --> CheckIdem{PriorCashApplied<br/>≠<br/>Secondary1Buys?}
    
    CheckIdem -->|No - Already Processed| HasRec
    CheckIdem -->|Yes - New Record| BuildC1[Build C1 Activity Record<br/>- Format fields<br/>- Negate amount]
    
    BuildC1 --> WriteFile[Write Record to File]
    WriteFile --> UpdateDB[Update Position<br/>Field 877 = Secondary1Buys]
    UpdateDB --> HasRec
    
    style Start fill:#e1f5e1
    style EndProg fill:#ffe1e1
    style CallRev fill:#fff4e1
    style CheckIdem fill:#e1f0ff
    style WriteFile fill:#f0e1ff
```

---

## 2. CHECK.SSSA Reversal Detection Flow

```mermaid
flowchart TD
    Entry([CHECK.SSSA Called]) --> ValidateParams{RKPlan ≠ ''<br/>AND<br/>TradeDate ≠ 0?}
    
    ValidateParams -->|No| ReturnEarly([GOBACK<br/>No Changes])
    ValidateParams -->|Yes| InitWK[Initialize WK001 = 0]
    
    InitWK --> QueryActivity[Query SSSA Activity<br/>Plan, Security, Date]
    QueryActivity --> NextRec{More<br/>Activity<br/>Records?}
    
    NextRec -->|No| AssignResult[Secondary1Buys = WK001]
    NextRec -->|Yes| CheckSource{Transaction<br/>Source = 'XI'?}
    
    CheckSource -->|No| NextRec
    CheckSource -->|Yes| CheckType{Transaction<br/>Type?}
    
    CheckType -->|'B' - Buy| AddAmount[WK001 += Amount<br/>Accumulate Buy]
    CheckType -->|'S' - Sell| SubAmount[WK001 -= Amount<br/>Reversal/Offset]
    CheckType -->|Other| NextRec
    
    AddAmount --> NextRec
    SubAmount --> NextRec
    
    AssignResult --> Return([GOBACK<br/>Secondary1Buys Updated])
    
    style Entry fill:#e1f5e1
    style Return fill:#ffe1e1
    style ReturnEarly fill:#ffe1e1
    style AddAmount fill:#d4edda
    style SubAmount fill:#f8d7da
```

---

## 3. Data Flow Diagram

```mermaid
flowchart LR
    subgraph Environment
        XDAT[($XDAT Directory)]
        RUNDATE[($RUN-DATE Variable)]
    end
    
    subgraph "Database Sources"
        POPP[(POPP Table<br/>Position Accounts)]
        SSSA[(SSSA Table<br/>Trust Transactions)]
    end
    
    subgraph "Program Processing"
        Main[Main Program<br/>Lines 13-57]
        CheckSSA[CHECK.SSSA<br/>Lines 59-75]
    end
    
    subgraph Outputs
        C1File[(C1 Activity File<br/>NEWLOANOFFSET.*.DAT)]
    end
    
    RUNDATE -->|RunDate| Main
    XDAT -->|File Path| Main
    POPP -->|Read Fields:<br/>030, 008, 741, 877, 1510| Main
    
    Main -->|RKPlan, TradeDate| CheckSSA
    SSSA -->|Transaction Data<br/>Fields 009, 011, 235| CheckSSA
    CheckSSA -->|Adjusted<br/>Secondary1Buys| Main
    
    Main -->|C1 Records| C1File
    Main -->|Update Field 877| POPP
    
    style Main fill:#e1f0ff
    style CheckSSA fill:#fff4e1
    style C1File fill:#d4edda
    style POPP fill:#f8d7da
    style SSSA fill:#f8d7da
```

---

## 4. State Machine: Record Processing States

```mermaid
stateDiagram-v2
    [*] --> Unprocessed: Position Record Found
    
    Unprocessed --> CheckingAmount: Extract Data
    CheckingAmount --> SkipZero: Secondary1Buys = 0
    CheckingAmount --> CheckingReversal: Secondary1Buys ≠ 0
    
    CheckingReversal --> NetCalculated: CHECK.SSSA Returns
    
    NetCalculated --> CheckingIdempotency: Compare to Prior
    CheckingIdempotency --> AlreadyProcessed: PriorCashApplied = Secondary1Buys
    CheckingIdempotency --> BuildingRecord: PriorCashApplied ≠ Secondary1Buys
    
    BuildingRecord --> WritingOutput: C1 Record Built
    WritingOutput --> UpdatingDatabase: File Written
    UpdatingDatabase --> Processed: Field 877 Updated
    
    SkipZero --> [*]: No Action
    AlreadyProcessed --> [*]: No Action
    Processed --> [*]: Complete
    
    note right of CheckingReversal
        Queries SSSA for
        Buy/Sell Activity
    end note
    
    note right of CheckingIdempotency
        Prevents duplicate
        C1 records
    end note
```

---

## 5. Sequence Diagram: Complete Processing Flow

```mermaid
sequenceDiagram
    participant Env as Environment
    participant Main as Main Program
    participant POPP as POPP Database
    participant CheckSSA as CHECK.SSSA
    participant SSSA as SSSA Database
    participant File as Output File
    
    Env->>Main: $RUN-DATE, $XDAT
    Main->>Main: Calculate Dates (SevenDaysAgo, LastBusiness)
    Main->>File: Open (FileName)
    
    Main->>POPP: Query Positions (POOLLOAN3, Date Range)
    
    loop For Each Position
        POPP-->>Main: Position Record
        Main->>Main: Extract Fields (RKPlan, TradeDate, etc.)
        
        alt Secondary1Buys > 0
            Main->>CheckSSA: PERFORM (RKPlan, TradeDate)
            CheckSSA->>SSSA: Query Activity (Plan, Security, Date)
            
            loop For Each Activity
                SSSA-->>CheckSSA: Activity Record
                CheckSSA->>CheckSSA: Accumulate Buys/Sells
            end
            
            CheckSSA-->>Main: GOBACK (Adjusted Secondary1Buys)
        end
        
        alt PriorCashApplied ≠ Secondary1Buys
            Main->>Main: Build C1 Record
            Main->>File: Write C1 Record
            Main->>POPP: Update Field 877
        else Already Processed
            Main->>Main: Skip Record
        end
    end
    
    Main->>File: Close (implicit)
    Main-->>Env: Exit
```

---

## 6. Entity Relationship: Database Schema

```mermaid
erDiagram
    POPP {
        string Field_030 "RKPlan (Plan ID)"
        numeric Field_008 "TradeDate"
        numeric Field_741 "Secondary1Buys (UDF)"
        numeric Field_877 "PriorCashApplied (UDF1)"
        string Field_1510 "TrustAccount"
        string SecurityID "POOLLOAN3"
    }
    
    SSSA {
        string Plan "Plan ID"
        string SecurityID "POOLLOAN3"
        numeric Date "Trade Date"
        string Field_009 "Transaction Type (B/S)"
        string Field_011 "Transaction Source (XI)"
        numeric Field_235 "Transaction Amount"
    }
    
    C1_Activity {
        string RecordType "C100"
        string PlanID "From POPP.Field_030"
        numeric EffectiveDate "LastBusiness"
        string TrustAccount "From POPP.Field_1510"
        string TransactionCode "000000000000000    2"
        numeric Amount "Negated Secondary1Buys"
        string ActivityCode "00339"
    }
    
    POPP ||--o{ SSSA : "matched by Plan+Security+Date"
    POPP ||--o{ C1_Activity : "generates when unprocessed"
    SSSA ||--o{ C1_Activity : "influences via reversal netting"
```

---

## 7. Component Interaction Diagram

```mermaid
graph TB
    subgraph "Runtime Environment"
        ENV[Environment Variables<br/>$XDAT, $RUN-DATE]
    end
    
    subgraph "GAP_NewLoanCash Program"
        INIT[Initialization<br/>Lines 13-19]
        DATE[Date Calculation<br/>Lines 21-29]
        MAIN[Main Loop<br/>Lines 31-57]
        ROUT[CHECK.SSSA Routine<br/>Lines 59-75]
    end
    
    subgraph "Data Sources"
        POPPDB[(POPP Database)]
        SSSADB[(SSSA Database)]
    end
    
    subgraph "Output Systems"
        OUTFILE[C1 Activity File]
        RECON[Cash Reconciliation<br/>System]
    end
    
    ENV --> INIT
    ENV --> DATE
    INIT --> MAIN
    DATE --> MAIN
    
    POPPDB <-->|Read/Update| MAIN
    MAIN -->|Call| ROUT
    ROUT <-->|Query| SSSADB
    ROUT -->|Return| MAIN
    
    MAIN -->|Write| OUTFILE
    OUTFILE -->|Consume| RECON
    
    style MAIN fill:#e1f0ff
    style ROUT fill:#fff4e1
    style OUTFILE fill:#d4edda
```

---

## 8. C1 Record Layout Diagram

```mermaid
block-beta
    columns 10
    
    block:header
        columns 1
        H["C1 Activity Record Layout<br/>(Fixed Length)"]
    end
    
    space
    
    block:RecType
        RT["Positions 1-4<br/>Record Type<br/>'C100'"]
    end
    
    block:Plan
        PL["Positions 5-10<br/>Plan ID<br/>(RKPlan)"]
    end
    
    block:Spacer1
        SP1["Positions 11-30<br/>Unused<br/>(20 bytes)"]
    end
    
    block:Date
        DT["Positions 31-38<br/>Effective Date<br/>(LastBusiness)"]
    end
    
    block:Spacer2
        SP2["Position 39<br/>Unused"]
    end
    
    block:Account
        AC["Positions 40-71<br/>Trust Account<br/>(32 bytes)"]
    end
    
    block:TxCode
        TX["Positions 73-92<br/>Transaction Code<br/>'000000000000000    2'"]
    end
    
    block:Spacer3
        SP3["Positions 93-114<br/>Unused<br/>(22 bytes)"]
    end
    
    block:Sign
        SG["Position 115<br/>Sign<br/>'0'"]
    end
    
    block:Amount
        AM["Positions 116-130<br/>Amount<br/>(NewLoanUnits)<br/>Format: Z,12V2-"]
    end
    
    block:Spacer4
        SP4["Positions 131-133<br/>Unused"]
    end
    
    block:ActCode
        CD["Positions 134-138<br/>Activity Code<br/>'00339'"]
    end
    
    style RT fill:#d4edda
    style PL fill:#d4edda
    style DT fill:#d4edda
    style AC fill:#d4edda
    style TX fill:#d4edda
    style AM fill:#f8d7da
    style CD fill:#d4edda
```

---

## 9. Timeline: Processing Sequence

```mermaid
timeline
    title GAP_NewLoanCash Execution Timeline
    
    section Initialization
        Line 13 : Set sd080 error context
        Line 14 : Define local variables
        Line 16-17 : Build filename with timestamp
        Line 19 : Open output file
    
    section Date Setup
        Line 21 : Get RunDate from environment
        Line 22 : Validate date
        Line 23-27 : Calculate SevenDaysAgo and LastBusiness
        Line 29 : Display date values
    
    section Data Processing
        Line 31 : Query poppobj for POOLLOAN3 positions
        Line 32-57 : Loop through position records
        Line 33-36 : Extract position data
        Line 37-39 : Call CHECK.SSSA if Secondary1Buys > 0
        Line 40-56 : Process unprocessed records
        Line 53 : Write C1 record to file
        Line 54-55 : Update position field 877
    
    section Completion
        Line 57 : End loop
        Implicit : Close file and exit
```

---

## 10. Decision Tree: Processing Logic

```mermaid
graph TD
    Start([Position Record]) --> Q1{Secondary1Buys<br/>= 0?}
    
    Q1 -->|Yes| Skip1[Skip Record<br/>No loan activity]
    Q1 -->|No| CallCheck[Call CHECK.SSSA<br/>Calculate Net Activity]
    
    CallCheck --> Q2{PriorCashApplied<br/>= Secondary1Buys?}
    
    Q2 -->|Yes| Skip2[Skip Record<br/>Already Processed]
    Q2 -->|No| Process[Process Record]
    
    Process --> ExtractData[Extract Trust Account<br/>and Other Fields]
    ExtractData --> CalcAmount[Calculate NewLoanUnits<br/>= 0 - Secondary1Buys]
    CalcAmount --> BuildRecord[Build C1 Record<br/>Using OcText_Set]
    BuildRecord --> WriteOutput[Write to Output File]
    WriteOutput --> UpdateField[Update Field 877<br/>= Secondary1Buys]
    UpdateField --> Complete([Record Complete])
    
    Skip1 --> End([Continue to Next])
    Skip2 --> End
    Complete --> End
    
    style Process fill:#d4edda
    style Skip1 fill:#f8d7da
    style Skip2 fill:#f8d7da
    style Complete fill:#e1f5e1
```

---

## 11. System Context Diagram

```mermaid
C4Context
    title System Context - GAP_NewLoanCash in Cash Reconciliation Ecosystem
    
    Person(ops, "Batch Operator", "Initiates nightly processing")
    
    System(gap, "GAP_NewLoanCash", "Generates C1 activity for loan purchases")
    
    System_Ext(positionsys, "Position Management System", "Maintains POPP table with daily positions")
    System_Ext(trustsys, "Trust Transaction System", "Maintains SSSA table with activity")
    System_Ext(reconcile, "Cash Reconciliation System", "Consumes C1 activity files")
    System_Ext(audit, "Audit System", "Archives transaction files")
    
    Rel(ops, gap, "Executes via batch scheduler", "Shell/Cron")
    Rel(gap, positionsys, "Queries and updates", "Database API")
    Rel(gap, trustsys, "Queries for reversals", "Database API")
    Rel(gap, reconcile, "Generates C1 files", "File System")
    Rel(gap, audit, "Archives output", "File System")
    
    UpdateLayoutConfig($c4ShapeInRow="3", $c4BoundaryInRow="1")
```

---

## 12. Deployment Diagram

```mermaid
graph TB
    subgraph "Batch Server Environment"
        subgraph "OMNISCRIPT Runtime"
            PROG[GAP_NewLoanCash.cbl<br/>OMNISCRIPT Program]
        end
        
        subgraph "File System"
            XDAT[/$XDAT Directory<br/>Output Files/]
            LOGS[/Log Files/]
        end
        
        subgraph "Environment Config"
            ENV[Environment Variables<br/>$RUN-DATE, $XDAT]
        end
    end
    
    subgraph "Database Server"
        DBMS[(Database System)]
        POPP_TBL[(POPP Table)]
        SSSA_TBL[(SSSA Table)]
    end
    
    subgraph "Downstream Systems"
        RECON[Cash Reconciliation<br/>Application]
        ARCHIVE[Archive Storage]
    end
    
    ENV --> PROG
    PROG -->|Query/Update| DBMS
    DBMS --- POPP_TBL
    DBMS --- SSSA_TBL
    PROG -->|Write| XDAT
    PROG -->|Write| LOGS
    XDAT -->|Read| RECON
    XDAT -->|Copy| ARCHIVE
    
    style PROG fill:#e1f0ff
    style XDAT fill:#d4edda
```

---

## Diagram Usage Guide

### Quick Reference

| Diagram | Best For | Use Case |
|---------|----------|----------|
| 1. Process Flow | Understanding overall logic | Initial code review, training |
| 2. Reversal Detection | Understanding CHECK.SSSA | Debugging reversal issues |
| 3. Data Flow | Understanding data movement | Integration analysis |
| 4. State Machine | Understanding record states | Troubleshooting status issues |
| 5. Sequence Diagram | Understanding timing/order | Performance analysis |
| 6. Entity Relationship | Understanding data structure | Database schema review |
| 7. Component Interaction | Understanding system integration | Architecture documentation |
| 8. Record Layout | Understanding C1 format | File format debugging |
| 9. Timeline | Understanding execution order | Step-by-step debugging |
| 10. Decision Tree | Understanding conditional logic | Business rule validation |
| 11. System Context | Understanding ecosystem | System architecture |
| 12. Deployment | Understanding infrastructure | Operations setup |

---

*These diagrams were created using Mermaid syntax and can be rendered in any Mermaid-compatible viewer.*  
*Last Generated*: 2026-02-03
