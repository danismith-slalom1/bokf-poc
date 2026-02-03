# GAP_NewLoanCash Diagrams

## Data Flow Diagram

```mermaid
flowchart LR
    subgraph Inputs
        ENV1[$RUN-DATE<br/>Environment]
        ENV2[$XDAT<br/>Environment]
        POPP[(POPP<br/>Database)]
        SSSA[(SSSA<br/>Database)]
    end
    
    subgraph Program["GAP_NewLoanCash"]
        Init[Initialize<br/>Variables]
        DateCalc[Calculate<br/>Date Range]
        Query[Query<br/>POPP]
        Process[Process<br/>Records]
        CheckSSA[CHECK.SSSA<br/>Net Calc]
        BuildC1[Build<br/>C1 Record]
        Update[Update<br/>UDF1]
    end
    
    subgraph Outputs
        C1File[C1 Activity<br/>File]
        POPPOut[(POPP<br/>Updated)]
    end
    
    ENV1 -->|Run Date| DateCalc
    ENV2 -->|Output Path| Init
    POPP -->|Position Records| Query
    Query --> Process
    SSSA -->|Buy/Sell Trans| CheckSSA
    Process -->|Non-zero Buys| CheckSSA
    CheckSSA -->|Net Amount| BuildC1
    BuildC1 -->|C1 Record| C1File
    Process -->|Mark Processed| Update
    Update -->|UDF1 Updated| POPPOut
    
    style Program fill:#E8F4F8
    style Inputs fill:#FFF4E6
    style Outputs fill:#E8F5E9
```

---

## State Transition Diagram

```mermaid
stateDiagram-v2
    [*] --> Initializing: Program Start
    
    Initializing --> QueryingRecords: File Opened,<br/>Dates Calculated
    
    QueryingRecords --> ProcessingRecord: Record Found
    QueryingRecords --> Complete: No More Records
    
    ProcessingRecord --> CheckingReversal: Secondary1Buys > 0
    ProcessingRecord --> ProcessingRecord: Secondary1Buys = 0<br/>(Skip to Next)
    
    CheckingReversal --> CalculatingNet: Valid Plan/Date
    CheckingReversal --> CheckingDuplicate: Invalid Plan/Date<br/>(Amount Unchanged)
    
    CalculatingNet --> CheckingDuplicate: Net Calculated
    
    CheckingDuplicate --> ProcessingRecord: Already Processed<br/>(Skip to Next)
    CheckingDuplicate --> GeneratingC1: Not Processed<br/>AND Amount > 0
    CheckingDuplicate --> ProcessingRecord: Amount = 0<br/>(Full Reversal)
    
    GeneratingC1 --> UpdatingDatabase: C1 Written
    
    UpdatingDatabase --> ProcessingRecord: UDF1 Updated<br/>(Next Record)
    
    Complete --> [*]: Program End
    
    note right of CheckingReversal
        CHECK.SSSA queries
        SSSA for buy/sell
        transactions
    end note
    
    note right of CheckingDuplicate
        Compare PriorCashApplied
        with Secondary1Buys
    end note
    
    note right of GeneratingC1
        Build 138-byte
        fixed-format record
    end note
```

---

## Sequence Diagram: Main Processing Flow

```mermaid
sequenceDiagram
    participant Main as Main Program
    participant POPP as POPP Database
    participant CheckSSA as CHECK.SSSA
    participant SSSA as SSSA Database
    participant File as C1 Output File
    
    Main->>Main: Initialize Variables
    Main->>File: Open Output File
    Main->>Main: Calculate Date Range
    
    Main->>POPP: Query POOLLOAN3<br/>(SevenDaysAgo to LastBusiness)
    
    loop For Each Position Record
        POPP-->>Main: Return Record
        Main->>POPP: Fetch Fields<br/>(RKPlan, TradeDate,<br/>Secondary1Buys, PriorCashApplied)
        
        alt Secondary1Buys > 0
            Main->>CheckSSA: PERFORM CHECK.SSSA
            
            alt Valid RKPlan and TradeDate
                CheckSSA->>SSSA: Query SSSA<br/>(Plan, POOLLOAN3, Date)
                
                loop For Each SSSA Record
                    SSSA-->>CheckSSA: Return Transaction
                    
                    alt Activity = 'XI' AND Type = 'B'
                        CheckSSA->>CheckSSA: WK001 += Amount
                    else Activity = 'XI' AND Type = 'S'
                        CheckSSA->>CheckSSA: WK001 -= Amount
                    end
                end
                
                CheckSSA->>CheckSSA: Secondary1Buys = WK001
            end
            
            CheckSSA-->>Main: GOBACK (Net Calculated)
        end
        
        alt PriorCashApplied ≠ Secondary1Buys<br/>AND Secondary1Buys > 0
            Main->>Main: Build C1 Record
            Main->>File: Write C1 Record
            Main->>POPP: Update UDF1 = Secondary1Buys
            POPP-->>Main: Update Confirmed
        else Already Processed or Zero Amount
            Note over Main: Skip to Next Record
        end
    end
    
    Main->>Main: End Program
```

---

## Entity Relationship Diagram

```mermaid
erDiagram
    POPP ||--o{ SSSA : "has activity"
    POPP {
        string DE008_TradeDate
        string DE030_RKPlan PK
        numeric DE741_Secondary1Buys
        numeric DE877_UDF1_PriorCashApplied
        string DE01510_TrustAccount
    }
    
    SSSA {
        string Plan FK
        string SecurityID
        date Date
        string DE009_TransactionType
        string DE011_ActivityCode
        numeric DE235_Amount
    }
    
    C1_OUTPUT {
        string RecordType
        string RKPlan
        date ActivityDate
        string TrustAccount
        numeric NewLoanUnits
        string ActivityCode
    }
    
    POPP ||--o{ C1_OUTPUT : "generates"
    
    ENVIRONMENT {
        string RUN_DATE
        string XDAT
    }
    
    ENVIRONMENT ||--|| POPP : "controls query"
    ENVIRONMENT ||--|| C1_OUTPUT : "defines path"
```

---

## Decision Tree: Record Processing Logic

```mermaid
flowchart TD
    Start([Fetch Position Record]) --> CheckZero{Secondary1Buys<br/><> 0?}
    
    CheckZero -->|No| Skip1[Skip Record]
    CheckZero -->|Yes| CallCheck[Call CHECK.SSSA]
    
    CallCheck --> ValidParams{Valid RKPlan<br/>AND<br/>TradeDate <> 0?}
    
    ValidParams -->|No| CheckDup1{PriorCash =<br/>Secondary1Buys?}
    ValidParams -->|Yes| QuerySSA[Query SSSA<br/>Calculate Net]
    
    QuerySSA --> CheckDup2{PriorCash =<br/>Secondary1Buys?}
    
    CheckDup1 -->|Yes| Skip2[Skip Record<br/>Already Processed]
    CheckDup1 -->|No| CheckStillNonZero1{Secondary1Buys<br/><> 0?}
    
    CheckDup2 -->|Yes| Skip3[Skip Record<br/>Already Processed]
    CheckDup2 -->|No| CheckStillNonZero2{Secondary1Buys<br/><> 0?}
    
    CheckStillNonZero1 -->|No| Skip4[Skip Record<br/>Zero Amount]
    CheckStillNonZero1 -->|Yes| Generate1[Generate C1<br/>Update UDF1]
    
    CheckStillNonZero2 -->|No| Skip5[Skip Record<br/>Full Reversal]
    CheckStillNonZero2 -->|Yes| Generate2[Generate C1<br/>Update UDF1]
    
    Skip1 --> Next([Next Record])
    Skip2 --> Next
    Skip3 --> Next
    Skip4 --> Next
    Skip5 --> Next
    Generate1 --> Next
    Generate2 --> Next
    
    style Generate1 fill:#90EE90
    style Generate2 fill:#90EE90
    style Skip1 fill:#FFE4B5
    style Skip2 fill:#FFE4B5
    style Skip3 fill:#FFE4B5
    style Skip4 fill:#FFE4B5
    style Skip5 fill:#FFE4B5
```

---

## Component Interaction Diagram

```mermaid
graph TB
    subgraph External Systems
        ENV[Environment<br/>Variables]
        POPP_DB[(POPP<br/>Database)]
        SSSA_DB[(SSSA<br/>Database)]
        FileSystem[File System<br/>$XDAT]
    end
    
    subgraph GAP_NewLoanCash Program
        Init[Initialization<br/>Module]
        DateCalc[Date<br/>Calculator]
        MainLoop[Main<br/>Loop]
        CheckSSA[CHECK.SSSA<br/>Routine]
        C1Builder[C1 Record<br/>Builder]
        DBUpdate[Database<br/>Updater]
    end
    
    subgraph OmniScript Runtime
        DateFuncs[Date<br/>Functions]
        FileFuncs[File I/O<br/>Functions]
        TextFuncs[Text<br/>Functions]
        DBFuncs[Database<br/>Functions]
    end
    
    ENV -->|$RUN-DATE| Init
    ENV -->|$XDAT| Init
    
    Init -->|Calculate Dates| DateCalc
    DateCalc -->|Use| DateFuncs
    DateCalc -->|SevenDaysAgo,<br/>LastBusiness| MainLoop
    
    Init -->|Open File| FileFuncs
    FileFuncs -->|Access| FileSystem
    
    MainLoop -->|Query| DBFuncs
    DBFuncs -->|Read| POPP_DB
    
    MainLoop -->|Call| CheckSSA
    CheckSSA -->|Query| DBFuncs
    DBFuncs -->|Read| SSSA_DB
    CheckSSA -->|Net Amount| MainLoop
    
    MainLoop -->|Build Record| C1Builder
    C1Builder -->|Format| TextFuncs
    C1Builder -->|Write| FileFuncs
    
    MainLoop -->|Update UDF1| DBUpdate
    DBUpdate -->|Update| DBFuncs
    DBFuncs -->|Write| POPP_DB
    
    style GAP_NewLoanCash Program fill:#E8F4F8
    style OmniScript Runtime fill:#FFF4E6
    style External Systems fill:#FFE4E1
```

---

## Timeline: Processing Sequence

```mermaid
gantt
    title GAP_NewLoanCash Execution Timeline
    dateFormat X
    axisFormat %L ms
    
    section Initialization
    Set Variables           :0, 10
    Build Filename          :10, 20
    Open Output File        :20, 30
    
    section Date Calc
    Get Run Date            :30, 40
    Validate Date           :40, 50
    Calculate Ranges        :50, 80
    
    section Main Loop
    Query POPP              :80, 100
    
    Record 1 Processing     :100, 150
    CHECK.SSSA Call 1       :120, 140
    C1 Write 1              :145, 150
    UDF1 Update 1           :150, 155
    
    Record 2 Processing     :155, 180
    CHECK.SSSA Call 2       :165, 175
    Skip (Duplicate)        :180, 180
    
    Record 3 Processing     :180, 230
    CHECK.SSSA Call 3       :190, 220
    C1 Write 3              :225, 230
    UDF1 Update 3           :230, 235
    
    section Completion
    End Loop                :235, 240
    Close Resources         :240, 250
```

---

## Activity Diagram: CHECK.SSSA Routine

```mermaid
stateDiagram-v2
    [*] --> ValidateParams: PERFORM CHECK.SSSA
    
    ValidateParams --> InitAccumulator: RKPlan valid AND<br/>TradeDate <> 0
    ValidateParams --> ReturnZero: Invalid params
    
    InitAccumulator --> QuerySSA: WK001 = 0
    
    QuerySSA --> FetchRecord: sssaobj_view()
    
    FetchRecord --> CheckMore: sssaobj_next()
    
    CheckMore --> CheckActivity: Record Found
    CheckMore --> AssignResult: No More Records
    
    CheckActivity --> CheckType: Activity = 'XI'
    CheckActivity --> FetchRecord: Activity <> 'XI'
    
    CheckType --> AddAmount: Type = 'B'
    CheckType --> SubAmount: Type = 'S'
    CheckType --> FetchRecord: Type = Other
    
    AddAmount --> FetchRecord: WK001 += Amount
    SubAmount --> FetchRecord: WK001 -= Amount
    
    AssignResult --> ReturnNet: Secondary1Buys = WK001
    
    ReturnZero --> [*]: GOBACK
    ReturnNet --> [*]: GOBACK
    
    note right of ValidateParams
        Guard against
        invalid queries
    end note
    
    note right of CheckActivity
        Filter for
        'XI' = External In
    end note
    
    note right of CheckType
        'B' = Buy: Add<br/>'S' = Sell: Subtract
    end note
```

---

## Data Transformation Flow

```mermaid
flowchart LR
    subgraph Input Data
        RD[$RUN-DATE<br/>or Current Date]
        PF[POPP Fields:<br/>DE 008, 030, 741,<br/>877, 01510]
        SF[SSSA Fields:<br/>DE 009, 011, 235]
    end
    
    subgraph Transformations
        T1[Date Arithmetic:<br/>-7 days<br/>-1 bus day]
        T2[Net Calculation:<br/>Buys - Sells]
        T3[Negation:<br/>0 - Amount]
        T4[Formatting:<br/>Fixed-width<br/>positions]
    end
    
    subgraph Output Data
        DR[SevenDaysAgo<br/>LastBusiness]
        NA[Net<br/>Secondary1Buys]
        C1[138-byte<br/>C1 Record]
        U1[Updated<br/>UDF1 Field]
    end
    
    RD --> T1
    T1 --> DR
    
    PF --> T2
    SF --> T2
    T2 --> NA
    
    NA --> T3
    T3 --> T4
    PF --> T4
    DR --> T4
    T4 --> C1
    
    NA --> U1
    
    style Transformations fill:#FFE4E6
    style Input Data fill:#E8F5E9
    style Output Data fill:#FFF9C4
```

---

## Error Handling Flow

```mermaid
flowchart TD
    Start([Program Start]) --> CheckEnv{$XDAT<br/>exists?}
    
    CheckEnv -->|No| Error1[ERROR:<br/>Invalid Environment]
    CheckEnv -->|Yes| OpenFile{File Open<br/>Success?}
    
    OpenFile -->|No| Error2[ERROR:<br/>File Open Failed]
    OpenFile -->|Yes| ValidateDate{$RUN-DATE<br/>Valid?}
    
    ValidateDate -->|No| Fallback1[Fallback to<br/>Current Date]
    ValidateDate -->|Yes| CalcDates1[Calculate<br/>Date Range]
    
    Fallback1 --> CalcDates2[Calculate<br/>Date Range]
    CalcDates1 --> QueryDB{Database<br/>Connection?}
    CalcDates2 --> QueryDB
    
    QueryDB -->|No| Error3[ERROR:<br/>DB Connection Failed]
    QueryDB -->|Yes| ProcessLoop[Process Records]
    
    ProcessLoop --> CheckZero{Secondary1Buys<br/><> 0?}
    
    CheckZero -->|No| Skip1[Skip Record]
    CheckZero -->|Yes| CheckValid{Valid<br/>Plan/Date?}
    
    CheckValid -->|No| UseOriginal[Use Original<br/>Amount]
    CheckValid -->|Yes| QuerySSA{SSSA Query<br/>Success?}
    
    QuerySSA -->|No| Error4[ERROR:<br/>SSSA Query Failed]
    QuerySSA -->|Yes| CalcNet[Calculate<br/>Net Amount]
    
    CalcNet --> CheckDup{Already<br/>Processed?}
    UseOriginal --> CheckDup
    
    CheckDup -->|Yes| Skip2[Skip Record]
    CheckDup -->|No| WriteFile{File Write<br/>Success?}
    
    WriteFile -->|No| Error5[ERROR:<br/>Write Failed]
    WriteFile -->|Yes| UpdateDB{DB Update<br/>Success?}
    
    UpdateDB -->|No| Error6[ERROR:<br/>Update Failed]
    UpdateDB -->|Yes| NextRecord{More<br/>Records?}
    
    Skip1 --> NextRecord
    Skip2 --> NextRecord
    
    NextRecord -->|Yes| ProcessLoop
    NextRecord -->|No| Complete([Complete])
    
    Error1 --> Abort([Abort])
    Error2 --> Abort
    Error3 --> Abort
    Error4 --> HandleError[Log Error,<br/>Continue?]
    Error5 --> HandleError
    Error6 --> HandleError
    
    HandleError --> NextRecord
    
    style Error1 fill:#FF6B6B
    style Error2 fill:#FF6B6B
    style Error3 fill:#FF6B6B
    style Error4 fill:#FFA07A
    style Error5 fill:#FFA07A
    style Error6 fill:#FFA07A
    style Complete fill:#90EE90
    style Abort fill:#FF4444
```

---

## C1 Record Format Diagram

```mermaid
graph LR
    subgraph C1 Record Structure - 138 Bytes
        direction TB
        
        P1["Pos 1-4<br/>'C100'<br/>(Type)"]
        P2["Pos 5-10<br/>RKPlan<br/>(6 chars)"]
        P3["Pos 11-30<br/>(Unused)"]
        P4["Pos 31-38<br/>Date<br/>(YYYYMMDD)"]
        P5["Pos 39<br/>(Unused)"]
        P6["Pos 40-71<br/>TrustAccount<br/>(32 chars)"]
        P7["Pos 72<br/>(Unused)"]
        P8["Pos 73-92<br/>Type/Pos<br/>'...2'<br/>(20 chars)"]
        P9["Pos 93-114<br/>(Unused)"]
        P10["Pos 115<br/>Sign<br/>'0'"]
        P11["Pos 116-130<br/>Amount<br/>Z,12V2-<br/>(15 chars)"]
        P12["Pos 131-133<br/>(Unused)"]
        P13["Pos 134-138<br/>Activity<br/>'00339'<br/>(5 chars)"]
    end
    
    style P1 fill:#FFE4E6
    style P2 fill:#E8F5E9
    style P4 fill:#E8F5E9
    style P6 fill:#E8F5E9
    style P8 fill:#FFF9C4
    style P10 fill:#FFE4E6
    style P11 fill:#E8F5E9
    style P13 fill:#FFF9C4
```

---

## Related Documentation
- [GAP_NewLoanCash Overview](GAP_NewLoanCash_OVERVIEW.md)
- [GAP_NewLoanCash Call Graph](GAP_NewLoanCash_CALL_GRAPH.md)
- [GAP_NewLoanCash Data Dictionary](GAP_NewLoanCash_DATA_DICTIONARY.md)
- [CHECK.SSSA Procedure](procedures/CHECK.SSSA.md)
