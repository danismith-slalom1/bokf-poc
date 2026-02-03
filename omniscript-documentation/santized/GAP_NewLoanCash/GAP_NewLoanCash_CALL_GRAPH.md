# GAP_NewLoanCash Call Graph

## Program: GAP_NewLoanCash
**Purpose**: Process Plan Position Accounts for loan cash reconciliation  
**Last Updated**: 2026-02-03

---

## Call Graph Overview

This program has a simple call structure with one main routine and one subroutine.

```
MAIN PROGRAM (Lines 13-57)
│
└─→ CHECK.SSSA (Lines 59-75)
    └─→ (performs reversal detection and calculation)
```

---

## Main Program Flow

### Entry Point: Line 13
**Purpose**: Initialize and process POOLLOAN3 position records for cash reconciliation

**Execution Sequence**:

1. **Initialization** (Lines 13-19)
   - Set error context (`sd080 = 99999999`)
   - Define local variables
   - Construct output filename with timestamp
   - Open output file for writing

2. **Date Calculation** (Lines 21-29)
   - Retrieve `RunDate` from environment variable `$RUN-DATE`
   - Calculate `SevenDaysAgo` and `LastBusiness` dates
   - Validate dates and use fallback to current date if needed
   - Display date values for logging

3. **Position Processing Loop** (Lines 31-57)
   - Query `poppobj` for POOLLOAN3 positions in date range
   - **For each position record**:
     - Extract plan, trade date, amounts, and account info
     - **[CALL]** `CHECK.SSSA` routine if Secondary1Buys ≠ 0
     - Check idempotency: skip if already processed
     - Build C1 activity record
     - Write record to output file
     - Update position record with applied cash amount

4. **Program Termination**
   - Loop completes
   - File automatically closed
   - Program ends

---

## Routine: CHECK.SSSA

### Location: Lines 59-75
**Called From**: Main program line 37-39 (PERFORM statement)
**Purpose**: Detect loan reversals and calculate net loan activity

**Parameters** (via global variables):
- **Input**: `RKPlan`, `TradeDate`
- **Output**: `Secondary1Buys` (modified)

**Logic Flow**:

1. **Validation** (Line 60)
   - Check if `RKPlan` is not empty AND `TradeDate` is not zero
   - Exit immediately if validation fails

2. **Initialize Accumulator** (Line 61)
   - Set work variable `WK001 = 0`

3. **Query Security Activity** (Line 62)
   - View `sssaobj` for matching plan, security, and date
   - Filter: `PLAN:RKPlan`, `SECURITYID:'POOLLOAN3'`, `DATE:TradeDate`

4. **Process Activity Records** (Lines 63-72)
   - **For each activity record**:
     - Check if transaction source = 'XI' (line 64)
     - **If Buy ('B')**: Add amount to WK001 (line 65)
     - **If Sell ('S')**: Subtract amount from WK001 (line 68) [Reversal]
   - Calculate net position after reversals

5. **Update and Return** (Lines 73-75)
   - Assign net amount to `Secondary1Buys`
   - GOBACK (return to caller)

**Side Effects**:
- Modifies global variable `Secondary1Buys`
- Reads from database object `sssaobj`

**Business Logic**:
- Handles scenarios where loan purchases are reversed by sales
- Ensures cash reconciliation reflects net activity, not gross
- Prevents double-counting when buys and sells occur on same date

---

## Database Call Hierarchy

### poppobj (Plan Position Object)
**Operations**:
1. `poppobj_view()` - Line 31
   - Filter: `securityid:'POOLLOAN3'`, `datelo:SevenDaysAgo`, `datehi:LastBusiness`
   - Returns: Position records for processing window
2. `poppobj_next()` - Line 32 (loop iterator)
3. `poppobj_de()` / `poppobj_numde()` - Lines 33-36, 41-44 (field extractors)
4. `poppobj_setde()` - Line 54 (field updater)
5. `poppobj_update()` - Line 55 (commit changes)

### sssaobj (Security Activity Object)
**Operations** (within CHECK.SSSA routine):
1. `sssaobj_view()` - Line 62
   - Filter: `PLAN:RKPlan`, `SECURITYID:'POOLLOAN3'`, `DATE:TradeDate`
   - Returns: Activity records for reversal detection
2. `sssaobj_next()` - Line 63 (loop iterator)
3. `sssaobj_de()` / `sssaobj_numde()` - Lines 64-65, 67-68 (field extractors)

---

## Built-In Function Call Map

### Initialization Phase
- `OcText_string()` - Line 16 (construct filename)
- `OCTEXT_GETENV()` - Line 16 (get $XDAT path)
- `OcDate_Current()` - Line 17 (get current date)
- `OcTime_Current()` - Line 17 (get current time)
- `OCFMT()` - Line 17 (format date/time)
- `OcShow()` - Lines 18, 29 (display values)
- `OcFile1_Open()` - Line 19 (open output file)

### Date Calculation Phase
- `octext_getenv()` - Line 21 (get $RUN-DATE)
- `octext_tonum()` - Line 21 (convert to numeric)
- `OcDate_Valid()` - Line 22 (validate date)
- `OcDate_AddDays()` - Lines 23, 26 (calculate SevenDaysAgo)
- `OcDate_AddBusDays()` - Lines 24, 27 (calculate LastBusiness)

### Record Construction Phase
- `OcText_Set()` - Lines 45-52 (build C1 record fields)
- `OcFmt()` - Lines 47, 50 (format date and amount)

### Output Phase
- `OcFile1_Write()` - Line 53 (write C1 record)

---

## Control Flow Diagram

```
┌─────────────────────────────────────────────────────┐
│ START                                               │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│ Initialize: Set sd080, Define vars, Build filename │
│ Open output file                                    │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│ Get RunDate from environment                        │
│ Validate and calculate date range                   │
│ (SevenDaysAgo, LastBusiness)                        │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│ Query poppobj for POOLLOAN3 positions               │
│ Date range: [SevenDaysAgo, LastBusiness]           │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
         ┌─────────────────┐
         │ poppobj_next()  │◄──────────────────┐
         └────────┬────────┘                   │
                  │                             │
                  │ [Has Record]                │
                  ▼                             │
    ┌─────────────────────────────┐            │
    │ Extract Position Data:      │            │
    │ - RKPlan                    │            │
    │ - TradeDate                 │            │
    │ - Secondary1Buys            │            │
    │ - PriorCashApplied          │            │
    └─────────────┬───────────────┘            │
                  │                             │
                  ▼                             │
        ┌──────────────────┐                   │
        │ Secondary1Buys   │                   │
        │ <> 0?            │                   │
        └──────┬───────────┘                   │
               │                                │
          [YES]│                                │
               ▼                                │
    ┌──────────────────────┐                   │
    │ PERFORM CHECK.SSSA   │                   │
    │ (Reversal Detection) │                   │
    └──────────┬───────────┘                   │
               │                                │
               ▼                                │
    ┌──────────────────────────────────┐       │
    │ PriorCashApplied <>              │       │
    │ Secondary1Buys?                  │       │
    └──────┬───────────────────────────┘       │
           │                                    │
      [YES]│                                    │
           ▼                                    │
  ┌─────────────────────────────┐              │
  │ Extract: TrustAccount       │              │
  │ Calculate: NewLoanUnits     │              │
  │ Build C1 Record (OcText_Set)│              │
  │ Write to file               │              │
  │ Update poppobj field 877    │              │
  └─────────────┬───────────────┘              │
                │                               │
                └───────────────────────────────┘
                │
                │ [No More Records]
                ▼
         ┌──────────────┐
         │ END          │
         └──────────────┘


┌──────────────────────────────────────────────┐
│ CHECK.SSSA SUBROUTINE                        │
│ (Lines 59-75)                                │
└──────────────┬───────────────────────────────┘
               │
               ▼
    ┌──────────────────────────┐
    │ RKPlan <> '' AND         │
    │ TradeDate <> 0?          │
    └──────┬───────────────────┘
           │
      [YES]│
           ▼
    ┌──────────────────────────┐
    │ WK001 = 0                │
    └──────┬───────────────────┘
           │
           ▼
    ┌──────────────────────────────────┐
    │ Query sssaobj for:               │
    │ PLAN, SECURITYID, DATE           │
    └──────┬───────────────────────────┘
           │
           ▼
    ┌─────────────────┐
    │ sssaobj_next()  │◄──────────┐
    └────────┬────────┘            │
             │                     │
        [Has]│                     │
             ▼                     │
   ┌──────────────────┐            │
   │ Transaction      │            │
   │ Source = 'XI'?   │            │
   └──────┬───────────┘            │
          │                        │
     [YES]│                        │
          ▼                        │
   ┌─────────────────┐             │
   │ Type = 'B'?     │             │
   └──────┬──────────┘             │
          │                        │
     [YES]│                        │
          ▼                        │
   ┌───────────────────┐           │
   │ WK001 += Amount   │           │
   └───────┬───────────┘           │
           │                       │
           ▼                       │
   ┌─────────────────┐             │
   │ Type = 'S'?     │             │
   └──────┬──────────┘             │
          │                        │
     [YES]│                        │
          ▼                        │
   ┌───────────────────┐           │
   │ WK001 -= Amount   │           │
   │ (Reversal)        │           │
   └───────┬───────────┘           │
           │                       │
           └───────────────────────┘
           │
           │ [No More]
           ▼
   ┌───────────────────────┐
   │ Secondary1Buys = WK001│
   └───────────┬───────────┘
               │
               ▼
         ┌──────────┐
         │ GOBACK   │
         └──────────┘
```

---

## Execution Metrics

### Program Statistics
- **Total Lines**: 75
- **Main Logic**: Lines 13-57 (45 lines)
- **Subroutine Logic**: Lines 59-75 (17 lines)
- **Database Calls**: Up to 2 views per position record (poppobj always, sssaobj conditionally)
- **File I/O**: 1 write per unprocessed position

### Performance Considerations
- **Loop Iterations**: Based on POOLLOAN3 positions in 7-day window
- **Conditional Subroutine Call**: Only when Secondary1Buys ≠ 0
- **Database Updates**: Only when PriorCashApplied ≠ Secondary1Buys
- **Idempotency**: Prevents redundant processing via field 877 check

---

## Call Dependencies Summary

### External Dependencies
- **Environment Variables**: `$XDAT`, `$RUN-DATE`
- **Database Objects**: `poppobj`, `sssaobj`
- **File System**: Write access to `$XDAT` directory

### Internal Dependencies
- **Main → CHECK.SSSA**: Conditional call for reversal detection
- **Shared Variables**: Global variables passed between routines

### No External Program Calls
This program is self-contained and does not call other programs or scripts.

---

*This call graph was generated from OMNISCRIPT Grammar Parser analysis and program flow inspection.*
