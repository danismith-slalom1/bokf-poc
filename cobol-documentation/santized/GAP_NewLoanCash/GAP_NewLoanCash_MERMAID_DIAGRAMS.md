# GAP_NewLoanCash Visual Diagrams (Mermaid)

This document contains mandatory Mermaid diagrams for the GAP_NewLoanCash OmniScript program. These visualizations aid in understanding program flow, dependencies, and data transformations for modernization and knowledge transfer efforts.

---

## 1. Program Flow Diagram

This flowchart shows the main processing logic from program start to termination.

```mermaid
flowchart TD
    Start([Program Start]) --> Init1[Set sd080 = 99999999]
    Init1 --> Init2[Declare Variables]
    Init2 --> Init3[Construct FileName with Timestamp]
    Init3 --> Init4[Display FileName]
    Init4 --> Init5[Open Output File]
    Init5 --> Init6[Read $RUN-DATE Environment Variable]
    Init6 --> DateValid{Is RunDate Valid?}
    
    DateValid -->|YES| Calc1[SevenDaysAgo = RunDate - 7 days<br/>LastBusiness = RunDate - 1 business day]
    DateValid -->|NO| Calc2[SevenDaysAgo = Current - 7 days<br/>LastBusiness = Current - 1 business day]
    
    Calc1 --> Display1[Display Dates]
    Calc2 --> Display1
    
    Display1 --> QueryPOPP[Query POPP Database:<br/>POOLLOAN3 positions<br/>Date Range: SevenDaysAgo to LastBusiness]
    
    QueryPOPP --> LoopStart{More POPP<br/>Records?}
    
    LoopStart -->|NO| EndProgram([End Program])
    
    LoopStart -->|YES| ReadPOPP[Read Position Data:<br/>RKPlan, TradeDate<br/>Secondary1Buys, PriorCashApplied<br/>TrustAccount]
    
    ReadPOPP --> CheckZero{Secondary1Buys<br/>≠ 0?}
    
    CheckZero -->|YES| CallSSSA[PERFORM CHECK.SSSA<br/>Recalculate Secondary1Buys]
    CheckZero -->|NO| CheckIdem
    
    CallSSSA --> CheckIdem{PriorCashApplied<br/>≠ Secondary1Buys<br/>AND<br/>Secondary1Buys ≠ 0?}
    
    CheckIdem -->|NO<br/>Already Processed| LoopStart
    
    CheckIdem -->|YES<br/>New Activity| ReRead[Re-read Position Data]
    ReRead --> CalcUnits[Calculate:<br/>NewLoanUnits = 0 - Secondary1Buys]
    CalcUnits --> BuildRecord[Build C1 Activity Record:<br/>Type='C100', Plan, Date,<br/>Account, Amount, Code='00339']
    BuildRecord --> WriteFile[Write Record to Output File]
    WriteFile --> UpdatePOPP[Update POPP DE 877 = Secondary1Buys<br/>Mark as Processed]
    UpdatePOPP --> LoopStart
    
    style Start fill:#90EE90
    style EndProgram fill:#FFB6C1
    style CallSSSA fill:#ADD8E6
    style CheckIdem fill:#FFD700
    style WriteFile fill:#FFA500
    style UpdatePOPP fill:#FFA500
```

**Key Insights**: 
- Main processing loop handles records until EOF
- Two validation stages: zero check and idempotency check
- File write and DB update are sequential (not atomic - risk of duplicate records on failure)

---

## 2. PERFORM Hierarchy (Call Graph)

This graph shows all routine call relationships in the program.

```mermaid
graph TD
    Main[MAIN PROGRAM<br/>Lines 11-52] --> Init[INITIALIZATION<br/>Lines 11-26]
    Main --> Loop[MAIN PROCESSING LOOP<br/>Lines 28-52]
    
    Init --> Env1[Read Environment Variables<br/>$XDAT, $RUN-DATE]
    Init --> File1[Open Output File<br/>OcFile1_Open]
    Init --> Date1[Calculate Date Range<br/>SevenDaysAgo, LastBusiness]
    
    Loop --> Query1[Query POPP Database<br/>poppobj_view]
    Loop --> Cond1{IF Secondary1Buys ≠ 0}
    
    Cond1 -->|YES| CheckSSSA[CHECK.SSSA Routine<br/>Lines 54-70]
    Cond1 -->|NO| Cond2
    
    CheckSSSA --> Validate[Validate RKPlan & TradeDate]
    CheckSSSA --> InitWK[Initialize WK001 = 0]
    CheckSSSA --> QuerySSA[Query SSSA Database<br/>sssaobj_view]
    CheckSSSA --> LoopSSA[Loop Through SSSA Records<br/>Net Buys - Sells]
    CheckSSSA --> UpdateVar[Update Secondary1Buys = WK001]
    CheckSSSA --> Return[GOBACK to Main]
    
    Return --> Cond2{IF Unprocessed<br/>& Non-Zero}
    
    Cond2 -->|YES| GenC1[Generate C1 Activity]
    Cond2 -->|NO| NextRec[Continue to Next Record]
    
    GenC1 --> Build[Build C1 Record<br/>OcText_Set]
    GenC1 --> Write[Write to File<br/>OcFile1_Write]
    GenC1 --> Update[Update POPP<br/>poppobj_update]
    
    Update --> NextRec
    
    classDef mainClass fill:#ADD8E6,stroke:#000,stroke-width:2px
    classDef subClass fill:#90EE90,stroke:#000,stroke-width:2px
    classDef decisionClass fill:#FFD700,stroke:#000,stroke-width:2px
    classDef ioClass fill:#FFA500,stroke:#000,stroke-width:2px
    
    class Main,Loop mainClass
    class CheckSSSA,Init subClass
    class Cond1,Cond2 decisionClass
    class Query1,QuerySSA,Write,Update ioClass
```

**Key Insights**:
- Simple two-level hierarchy: Main → CHECK.SSSA
- CHECK.SSSA called conditionally (only for non-zero Secondary1Buys)
- No recursion, maximum call depth of 2

---

## 3. Data Flow Diagram

This diagram shows how data moves and transforms through the program.

```mermaid
flowchart LR
    subgraph Inputs
        ENV1[$XDAT<br/>Environment]
        ENV2[$RUN-DATE<br/>Environment]
        DB1[(POPP Database<br/>Position Records)]
        DB2[(SSSA Database<br/>Settlement Activity)]
    end
    
    subgraph Initialization
        ENV1 --> FileName[FileName<br/>Timestamped Path]
        ENV2 --> RunDate[RunDate<br/>Validated]
        RunDate --> DateCalc[Date Calculation]
        DateCalc --> SevenDays[SevenDaysAgo]
        DateCalc --> LastBus[LastBusiness]
    end
    
    subgraph Main Processing
        SevenDays --> Query1[POPP Query]
        LastBus --> Query1
        DB1 --> Query1
        Query1 --> POPPData[Position Data:<br/>RKPlan, TradeDate<br/>Secondary1Buys<br/>PriorCashApplied<br/>TrustAccount]
    end
    
    subgraph SSSA Processing
        POPPData --> RKPlan[RKPlan]
        POPPData --> TradeDate[TradeDate]
        RKPlan --> Query2[SSSA Query]
        TradeDate --> Query2
        DB2 --> Query2
        Query2 --> SSSAData[SSSA Activity:<br/>Buy/Sell Amounts]
        SSSAData --> NetCalc[Net Calculation:<br/>WK001 = ΣBuys - ΣSells]
        NetCalc --> NewSecondary[Updated<br/>Secondary1Buys]
    end
    
    subgraph C1 Generation
        NewSecondary --> Comparison{Compare with<br/>PriorCashApplied}
        Comparison -->|Different & Non-Zero| Transform[Transform:<br/>NewLoanUnits =<br/>0 - Secondary1Buys]
        POPPData --> TrustAcct[TrustAccount]
        LastBus --> EffDate[Effective Date]
        Transform --> BuildC1[Build C1 Record]
        TrustAcct --> BuildC1
        EffDate --> BuildC1
        RKPlan --> BuildC1
    end
    
    subgraph Outputs
        FileName --> File[Output File]
        BuildC1 --> File
        NewSecondary --> UpdateDB[(POPP Database<br/>Update DE 877)]
    end
    
    style ENV1 fill:#E6F3FF
    style ENV2 fill:#E6F3FF
    style DB1 fill:#FFE6E6
    style DB2 fill:#FFE6E6
    style File fill:#E6FFE6
    style UpdateDB fill:#FFE6E6
    style NetCalc fill:#FFFFCC
    style Transform fill:#FFFFCC
```

**Key Insights**:
- Environment variables drive file naming and date calculations
- POPP provides primary position data
- SSSA provides adjustment/reversal data
- Secondary1Buys is the critical variable that flows through entire process
- Outputs are: C1 file records + POPP database updates

---

## 4. Copybook Dependencies

This program does not use traditional COBOL COPY statements. However, it has implicit dependencies on OmniScript runtime libraries and database schemas.

```mermaid
graph LR
    Program[GAP_NewLoanCash.cbl] --> Runtime[OmniScript Runtime Library]
    
    Runtime --> File[File I/O Functions:<br/>OcFile1_Open<br/>OcFile1_Write]
    Runtime --> Text[Text Functions:<br/>OcText_string<br/>OcText_Set<br/>OCTEXT_GETENV]
    Runtime --> Date[Date Functions:<br/>OcDate_Valid<br/>OcDate_AddDays<br/>OcDate_AddBusDays<br/>OcDate_Current]
    Runtime --> Format[Format Functions:<br/>OcFmt<br/>OcTime_Current]
    Runtime --> Display[Display Function:<br/>OcShow]
    
    Program --> POPP[POPP Database Schema]
    Program --> SSSA[SSSA Database Schema]
    
    POPP --> POPPDE[Data Elements:<br/>DE 030 - RKPlan<br/>DE 008 - TradeDate<br/>DE 741 - Secondary1Buys<br/>DE 877 - PriorCashApplied<br/>DE 01510 - TrustAccount]
    
    SSSA --> SSSADE[Data Elements:<br/>DE 009 - Transaction Type<br/>DE 011 - Transaction Indicator<br/>DE 235 - Amount]
    
    classDef programClass fill:#ADD8E6,stroke:#000,stroke-width:2px
    classDef libraryClass fill:#90EE90,stroke:#000,stroke-width:2px
    classDef dbClass fill:#FFB6C1,stroke:#000,stroke-width:2px
    
    class Program programClass
    class Runtime,File,Text,Date,Format,Display libraryClass
    class POPP,SSSA,POPPDE,SSSADE dbClass
```

**Key Insights**:
- No traditional COPY books (native OmniScript program)
- Heavy dependency on OmniScript runtime library
- Direct coupling to POPP and SSSA database schemas
- Any schema changes to DE 030, 741, 877, 01510 in POPP would require program changes
- Any schema changes to DE 009, 011, 235 in SSSA would require program changes

---

## 5. File I/O Operations Timeline

This sequence diagram shows the order of file and database operations during execution.

```mermaid
sequenceDiagram
    participant Env as Environment
    participant Prog as Program
    participant File as Output File
    participant POPP as POPP Database
    participant SSSA as SSSA Database
    
    Note over Prog: INITIALIZATION PHASE
    Prog->>Env: Read $XDAT
    Env-->>Prog: Directory path
    Prog->>Env: Read $RUN-DATE
    Env-->>Prog: Run date (or fallback)
    Prog->>Prog: Calculate SevenDaysAgo, LastBusiness
    Prog->>File: OcFile1_Open(FileName, OUTPUT)
    File-->>Prog: File handle
    
    Note over Prog: MAIN PROCESSING PHASE
    Prog->>POPP: poppobj_view(POOLLOAN3, SevenDaysAgo, LastBusiness)
    POPP-->>Prog: Query cursor positioned
    
    loop For Each POPP Record
        Prog->>POPP: poppobj_next()
        POPP-->>Prog: Position record
        Prog->>POPP: Read DE 030, 008, 741, 877, 01510
        POPP-->>Prog: RKPlan, TradeDate, Secondary1Buys, etc.
        
        alt Secondary1Buys ≠ 0
            Note over Prog: CHECK.SSSA Routine
            Prog->>SSSA: sssaobj_view(RKPlan, POOLLOAN3, TradeDate)
            SSSA-->>Prog: Query cursor positioned
            
            loop For Each SSSA Record
                Prog->>SSSA: sssaobj_next()
                SSSA-->>Prog: Activity record
                Prog->>SSSA: Read DE 009, 011, 235
                SSSA-->>Prog: Type, Indicator, Amount
                Prog->>Prog: Accumulate: WK001 += or -= Amount
            end
            
            Prog->>Prog: Update Secondary1Buys = WK001
        end
        
        alt PriorCashApplied ≠ Secondary1Buys AND Secondary1Buys ≠ 0
            Prog->>Prog: Build C1 activity record
            Prog->>File: OcFile1_Write(Line)
            File-->>Prog: Write confirmation
            Prog->>POPP: poppobj_setde(877, Secondary1Buys)
            Prog->>POPP: poppobj_update()
            POPP-->>Prog: Update confirmation
        end
    end
    
    Note over Prog: TERMINATION PHASE
    Prog->>File: Implicit close (end of program)
    File-->>Prog: File closed
```

**Key Insights**:
- File opened once at start, written multiple times during loop
- POPP query executed once, cursor iterated for all records
- SSSA query executed once per POPP record with non-zero Secondary1Buys (nested query pattern)
- File write and POPP update are sequential (not transactional - risk of inconsistency)
- No explicit file close (relies on program termination)

---

## 6. Variable Lifecycle State Diagrams

### Secondary1Buys Variable Lifecycle

This state diagram shows the critical transformations of the Secondary1Buys variable.

```mermaid
stateDiagram-v2
    [*] --> Uninitialized
    
    Uninitialized --> ReadFromPOPP: Line 32<br/>poppobj_numde(741)
    
    state "Read from POPP" as ReadFromPOPP {
        [*] --> POPPValue: Secondary1Buys =<br/>Original position amount
    }
    
    ReadFromPOPP --> ZeroCheck: Line 34<br/>IF Secondary1Buys ≠ 0
    
    state ZeroCheck <<choice>>
    ZeroCheck --> SkipSSSA: Secondary1Buys = 0<br/>No SSSA check needed
    ZeroCheck --> CallSSSA: Secondary1Buys ≠ 0<br/>Check for reversals
    
    state "CHECK.SSSA Processing" as CallSSSA {
        [*] --> QuerySSSA: Query SSSA database
        QuerySSSA --> Accumulate: Loop through records
        Accumulate --> NetCalculation: WK001 = ΣBuys - ΣSells
        NetCalculation --> UpdateValue: Line 69<br/>Secondary1Buys = WK001
        UpdateValue --> [*]
    }
    
    CallSSSA --> IdempotencyCheck: Return from CHECK.SSSA
    SkipSSSA --> IdempotencyCheck
    
    state "Idempotency Check" as IdempotencyCheck {
        [*] --> Compare: Compare with<br/>PriorCashApplied
    }
    
    IdempotencyCheck --> Decision: Line 38
    
    state Decision <<choice>>
    Decision --> SkipProcessing: Already processed<br/>OR zero amount
    Decision --> GenerateC1: Unprocessed & non-zero
    
    state "Generate C1 Activity" as GenerateC1 {
        [*] --> Negate: Line 41<br/>NewLoanUnits = 0 - Secondary1Buys
        Negate --> BuildRecord: Build C1 record
        BuildRecord --> WriteFile: Write to file
        WriteFile --> UpdateDB: Line 50<br/>POPP DE 877 = Secondary1Buys
        UpdateDB --> [*]
    }
    
    GenerateC1 --> NextRecord: Continue to next
    SkipProcessing --> NextRecord: Continue to next
    
    NextRecord --> [*]: Loop to next POPP record
    
    note right of ReadFromPOPP
        Initial state from database
        May be zero or positive amount
    end note
    
    note right of CallSSSA
        CRITICAL MUTATION POINT
        Value adjusted based on
        SSSA buy/sell activity
    end note
    
    note right of GenerateC1
        Final value persisted
        to POPP for future runs
    end note
```

**State Descriptions**:
1. **Uninitialized**: Variable declared but not assigned
2. **ReadFromPOPP**: Initial value from POPP DE 741
3. **ZeroCheck**: Decision point - check SSSA or skip?
4. **CallSSSA**: Recalculation phase (may modify value)
5. **IdempotencyCheck**: Compare with previous run
6. **Decision**: Generate C1 or skip?
7. **GenerateC1**: Use in output and persist
8. **NextRecord**: Ready for next iteration

**State Transitions**:
- **Normal Path**: Uninitialized → ReadFromPOPP → CallSSSA → GenerateC1 → NextRecord
- **Zero Amount Path**: Uninitialized → ReadFromPOPP → SkipSSSA → SkipProcessing → NextRecord
- **Already Processed Path**: Uninitialized → ReadFromPOPP → CallSSSA → SkipProcessing → NextRecord

**Key Insights**:
- Secondary1Buys has one critical mutation point (CHECK.SSSA)
- Final state is persisted to database for future idempotency checks
- Value can transition from non-zero → zero (full reversal case)
- No validation of negative values (potential data integrity risk)

---

### WK001 Accumulator Lifecycle

```mermaid
stateDiagram-v2
    [*] --> Uninitialized: CHECK.SSSA Entry
    
    Uninitialized --> Initialized: Line 58<br/>WK001 = 0
    
    Initialized --> QuerySSA: sssaobj_view()
    
    QuerySSA --> LoopStart: SSSA cursor positioned
    
    state LoopStart <<choice>>
    LoopStart --> ProcessRecord: More records exist
    LoopStart --> LoopComplete: No more records
    
    state "Process SSSA Record" as ProcessRecord {
        [*] --> CheckIndicator: Read DE 011
        CheckIndicator --> CheckType: If 'XI'
        CheckType --> AddBuy: If DE 009 = 'B'
        CheckType --> SubtractSell: If DE 009 = 'S'
        AddBuy --> [*]: WK001 += Amount
        SubtractSell --> [*]: WK001 -= Amount
        CheckType --> [*]: Other type (ignored)
    }
    
    ProcessRecord --> LoopStart: Next record
    
    LoopComplete --> Transfer: Line 69<br/>Secondary1Buys = WK001
    
    Transfer --> [*]: Return to caller
    
    note right of Initialized
        Accumulator reset to zero
        at start of each CHECK.SSSA call
    end note
    
    note right of ProcessRecord
        Accumulate net amount:
        + Buy transactions
        - Sell/Reversal transactions
    end note
    
    note right of Transfer
        Final value transferred
        to global Secondary1Buys
    end note
```

**Key Insights**:
- WK001 is local to CHECK.SSSA routine, reset each call
- Accumulation pattern: sum of buys minus sum of sells
- Final value replaces Secondary1Buys (critical mutation)
- No bounds checking (risk of overflow with extreme values)

---

## Diagram Usage Guide

### For New Developers
**Recommended Order**:
1. **Program Flow Diagram** - Understand overall processing logic
2. **Data Flow Diagram** - See how data moves through the system
3. **Variable Lifecycle (Secondary1Buys)** - Understand critical variable mutations
4. **File I/O Timeline** - Grasp database and file interaction patterns

### For Maintenance Developers
**Focus Areas**:
1. **PERFORM Hierarchy** - Understand call relationships before modifying routines
2. **File I/O Timeline** - Identify transaction boundaries and error handling gaps
3. **Variable Lifecycle** - Trace mutation points when debugging data issues

### For Modernization Teams
**Key Diagrams**:
1. **Copybook Dependencies** - Identify external dependencies for migration
2. **Data Flow Diagram** - Map data transformations for new architecture
3. **Program Flow Diagram** - Identify refactoring opportunities

### For QA/Testing
**Testing Paths**:
1. **Program Flow Diagram** - Identify test scenarios (zero amounts, full reversals, etc.)
2. **Variable Lifecycle** - Create test cases for each state transition
3. **File I/O Timeline** - Design integration tests for database and file operations

---

## Mermaid Rendering Notes

These diagrams are written in Mermaid syntax and will render automatically in:
- GitHub/GitLab Markdown viewers
- VS Code with Mermaid Preview extension
- Mermaid Live Editor: https://mermaid.live
- Most modern documentation platforms (Confluence, Notion, etc.)

To validate or modify diagrams, copy the Mermaid code blocks to https://mermaid.live

---

*AI-Generated Documentation - Review with OmniScript/COBOL experts for accuracy. Diagrams represent interpreted program logic based on source code analysis.*
