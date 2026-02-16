# GAP_NEWLOANCASH Visual Diagrams

## Overview

This document contains visual representations of the GAP_NewLoanCash program structure, flow, and dependencies generated using Mermaid diagrams. Core diagrams (Program Flow, Call Hierarchy, Data Flow) are also embedded in the [Program Overview](./GAP_NEWLOANCASH_OVERVIEW.md). This file contains the complete set including complex/detailed diagrams.

---

## 1. Program Flow Diagram

This flowchart shows the main processing logic from program start to end, including all decision points and the CHECK.SSSA subroutine.

```mermaid
flowchart TD
    Start([Program Start]):::startEnd --> Init[Set sd080 = 99999999<br/>Declare Variables]:::initFlow
    Init --> BuildFile[Construct Output Filename<br/>from XDAT + Date + Time]:::initFlow
    BuildFile --> OpenFile[OcFile1_Open<br/>OUTPUT mode]:::ioFlow
    OpenFile --> ReadDate[Read RUN-DATE<br/>from Environment]:::initFlow
    ReadDate --> ValidDate{OcDate_Valid<br/>RunDate?}

    ValidDate -->|Valid| CalcFromRun[SevenDaysAgo = RunDate - 7 days<br/>LastBusiness = RunDate - 1 biz day]:::initFlow
    ValidDate -->|Invalid| CalcFromCurr[SevenDaysAgo = Current - 7 days<br/>LastBusiness = Current - 1 biz day]:::initFlow

    CalcFromRun --> ShowDates[OcShow Dates]
    CalcFromCurr --> ShowDates

    ShowDates --> POPPView[poppobj_view<br/>POOLLOAN3<br/>SevenDaysAgo..LastBusiness]:::dbFlow
    POPPView --> NextRec{poppobj_next?}

    NextRec -->|No More Records| EndProg([Program End]):::startEnd
    NextRec -->|Record Found| Extract[Extract: RKPlan<br/>TradeDate, Secondary1Buys<br/>PriorCashApplied]:::mainFlow

    Extract --> ChkBuys{Secondary1Buys<br/> ne 0?}
    ChkBuys -->|Yes| CallSSA[PERFORM CHECK.SSSA]:::routineFlow
    ChkBuys -->|No / Zero| ChkIdem

    CallSSA --> ChkIdem{PriorCashApplied ne<br/>Secondary1Buys<br/>AND Secondary1Buys ne 0?}

    ChkIdem -->|No - Already Processed<br/>or Zero| NextRec
    ChkIdem -->|Yes - New Activity| BuildRec[Calculate NewLoanUnits<br/>Build C1 Record in Line]:::mainFlow

    BuildRec --> WriteRec[OcFile1_Write Line]:::ioFlow
    WriteRec --> UpdatePOPP[poppobj_setde 877<br/>poppobj_update]:::dbFlow
    UpdatePOPP --> NextRec

    classDef startEnd fill:#90EE90,stroke:#333,stroke-width:2px
    classDef initFlow fill:#E8E8E8,stroke:#333,stroke-width:1px
    classDef mainFlow fill:#87CEEB,stroke:#333,stroke-width:2px
    classDef dbFlow fill:#FFD700,stroke:#333,stroke-width:1px
    classDef ioFlow fill:#FFA500,stroke:#333,stroke-width:1px
    classDef routineFlow fill:#DDA0DD,stroke:#333,stroke-width:2px
```

**Key Insights**:
- The main loop processes POPP records until exhausted
- Two guard conditions filter records: non-zero buys and idempotency check
- CHECK.SSSA adjusts Secondary1Buys before the idempotency comparison
- Records that pass both checks get a C1 record written and POPP updated

---

## 2. Call Hierarchy

Shows all procedure call relationships and their conditions.

```mermaid
graph TD
    Main[Main Program<br/>Lines 13-57]:::mainFlow --> Init[Initialization<br/>Lines 13-19]:::initFlow
    Main --> DateCalc[Date Calculation<br/>Lines 21-29]:::initFlow
    Main --> Loop[POPP Processing Loop<br/>Lines 31-57]:::mainFlow

    Loop -->|Secondary1Buys ne 0| CheckSSA[CHECK.SSSA<br/>Lines 59-75]:::routineFlow
    Loop --> Output[Build and Write C1 Record<br/>Lines 40-56]:::ioFlow

    CheckSSA --> SSSAView[sssaobj_view<br/>sssaobj_next loop]:::dbFlow
    Loop --> POPPOps[poppobj_view<br/>poppobj_next loop]:::dbFlow
    Output --> FileWrite[OcFile1_Write]:::ioFlow
    Output --> POPPUpdate[poppobj_setde + update]:::dbFlow

    classDef mainFlow fill:#87CEEB,stroke:#333,stroke-width:2px
    classDef initFlow fill:#E8E8E8,stroke:#333,stroke-width:1px
    classDef routineFlow fill:#DDA0DD,stroke:#333,stroke-width:2px
    classDef dbFlow fill:#FFD700,stroke:#333,stroke-width:1px
    classDef ioFlow fill:#FFA500,stroke:#333,stroke-width:1px
```

**Key Insights**:
- Simple two-level hierarchy: Main Program → CHECK.SSSA
- CHECK.SSSA is the only subroutine, called conditionally
- Database operations span both levels (POPP in main, SSSA in subroutine)

---

## 3. Data Flow Diagram

Shows how data moves from inputs through processing to outputs.

```mermaid
flowchart LR
    subgraph Inputs
        ENV1[/"$XDAT"/]:::envFlow
        ENV2[/"$RUN-DATE"/]:::envFlow
        POPP[(POPP Database<br/>POOLLOAN3)]:::dbFlow
        SSSA[(SSSA Database<br/>XI Transactions)]:::dbFlow
    end

    subgraph Processing
        DateCalc[Date Range<br/>Calculation]:::mainFlow
        Extract[Extract Position<br/>Fields]:::mainFlow
        NetCalc[Net Buy/Sell<br/>Calculation]:::routineFlow
        Idem[Idempotency<br/>Check]:::mainFlow
        BuildRec[Build C1<br/>Record]:::mainFlow
    end

    subgraph Outputs
        File[/"C1 Activity File<br/>NEWLOANOFFSET.DAT"/]:::ioFlow
        POPPUpd[(POPP Database<br/>UDF1 Updated)]:::dbFlow
    end

    ENV1 --> BuildRec
    ENV2 --> DateCalc
    DateCalc --> Extract
    POPP --> Extract
    Extract --> NetCalc
    SSSA --> NetCalc
    NetCalc --> Idem
    Idem -->|New Activity| BuildRec
    BuildRec --> File
    Idem -->|Update Marker| POPPUpd

    classDef envFlow fill:#B0E0E6,stroke:#333,stroke-width:1px
    classDef dbFlow fill:#FFD700,stroke:#333,stroke-width:1px
    classDef mainFlow fill:#87CEEB,stroke:#333,stroke-width:2px
    classDef routineFlow fill:#DDA0DD,stroke:#333,stroke-width:2px
    classDef ioFlow fill:#FFA500,stroke:#333,stroke-width:1px
```

**Key Insights**:
- Two database sources feed the processing (POPP positions, SSSA transactions)
- Two outputs: C1 activity file (new data) and POPP database update (idempotency marker)
- Environment variables control output path and processing date window

---

## 4. File I/O Operations Timeline

Shows all file and database operations in execution order.

```mermaid
sequenceDiagram
    participant P as Program
    participant E as Environment
    participant F as Output File
    participant POPP as POPP Database
    participant SSSA as SSSA Database

    P->>E: Read $XDAT
    P->>E: Read $RUN-DATE
    P->>F: OcFile1_Open (OUTPUT)

    P->>POPP: poppobj_view(POOLLOAN3, date range)

    loop For Each POPP Record
        POPP-->>P: poppobj_next() → record data
        P->>P: Extract RKPlan, TradeDate, Secondary1Buys, PriorCashApplied

        alt Secondary1Buys != 0
            P->>SSSA: sssaobj_view(Plan, POOLLOAN3, Date)
            loop For Each SSSA Record
                SSSA-->>P: sssaobj_next() → transaction data
                P->>P: Accumulate Buy/Sell amounts
            end
            P->>P: Secondary1Buys = Net Amount
        end

        alt New Activity (not yet processed)
            P->>P: Build C1 Record
            P->>F: OcFile1_Write(Line)
            P->>POPP: poppobj_setde(877, value)
            P->>POPP: poppobj_update()
        end
    end

    Note over P,F: File implicitly closed on program exit
```

**Key Insights**:
- Output file is opened once at start, written to multiple times during processing
- SSSA views are opened/closed for each qualifying POPP record (nested I/O)
- POPP updates happen inline within the main POPP read loop
- No explicit file close operation

---

## 5. Variable Lifecycle State Diagram - Secondary1Buys

The most complex variable in the program, with mutations across the main loop and CHECK.SSSA.

```mermaid
stateDiagram-v2
    [*] --> Declared: OcLVar_Define (Line 14)
    Declared --> LoadedFromPOPP: poppobj_numde(741) - Line 35

    LoadedFromPOPP --> CheckZero: Evaluate Secondary1Buys

    CheckZero --> ZeroBuys: Value = 0
    CheckZero --> NonZeroBuys: Value != 0

    ZeroBuys --> [*]: Skip record (Line 40 fails)

    NonZeroBuys --> SSSACheck: PERFORM CHECK.SSSA (Line 38)

    SSSACheck --> SSSANetted: WK001 calculated from<br/>SSSA Buy/Sell transactions
    SSSANetted --> Replaced: Secondary1Buys = WK001 (Line 73)

    Replaced --> IdempotencyCheck: Compare vs PriorCashApplied

    IdempotencyCheck --> AlreadyProcessed: Values match
    IdempotencyCheck --> NewActivity: Values differ AND != 0

    AlreadyProcessed --> LoadedFromPOPP: Next POPP record
    NewActivity --> UsedForOutput: NewLoanUnits = 0 - Secondary1Buys
    UsedForOutput --> WrittenToUDF1: poppobj_setde(877) - Line 54
    WrittenToUDF1 --> LoadedFromPOPP: Next POPP record

    LoadedFromPOPP --> [*]: No more records
```

**Key Insights**:
- Secondary1Buys undergoes a two-stage transformation: initial load from POPP, then potential replacement by SSSA net amount
- The value flows through three business checks: zero check, SSSA netting, idempotency check
- After processing, the value is persisted back to POPP as the idempotency marker

---

## 6. Variable Lifecycle State Diagram - Line (Output Buffer)

```mermaid
stateDiagram-v2
    [*] --> Declared: OcLVar_Define (Line 14)
    Declared --> Empty: Uninitialized buffer

    Empty --> Pos1_4: OcText_Set pos 1 = C100
    Pos1_4 --> Pos5_10: OcText_Set pos 5 = RKPlan
    Pos5_10 --> Pos31_38: OcText_Set pos 31 = LastBusiness
    Pos31_38 --> Pos40_71: OcText_Set pos 40 = TrustAccount
    Pos40_71 --> Pos73_92: OcText_Set pos 73 = Control Flags
    Pos73_92 --> Pos115: OcText_Set pos 115 = Processing Flag
    Pos115 --> Pos116_130: OcText_Set pos 116 = NewLoanUnits
    Pos116_130 --> Pos134_138: OcText_Set pos 134 = Transaction Code

    Pos134_138 --> Written: OcFile1_Write(Line) - Line 53
    Written --> Empty: Next qualifying record
    Written --> [*]: No more records
```

**Key Insights**:
- Line buffer is populated incrementally using positional OcText_Set calls
- Eight fields are set in sequence before each write
- Buffer is reused for each qualifying record (not explicitly cleared between writes)

---

## Tools and Resources

- [Mermaid Live Editor](https://mermaid.live) - Validate and preview diagrams
- [Mermaid Documentation](https://mermaid.js.org/intro/) - Syntax reference
- VS Code Extension: Mermaid Preview - Real-time preview in editor
