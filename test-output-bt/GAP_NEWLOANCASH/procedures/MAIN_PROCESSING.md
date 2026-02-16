# MAIN_PROCESSING - Main Program Flow

## Source Location
- **Lines**: 13-57
- **File**: GAP_NewLoanCash.txt

## Purpose

The main program flow initializes the environment, determines the processing date range, queries Plan Position (POPP) records for POOLLOAN3 security, evaluates each record for new loan cash activity, and generates C1 activity offset records for the cash reconciliation process.

## Business Context

This is a **cash reconciliation offset program**. When new loan activity (Secondary1 Buys) occurs in the POOLLOAN3 pool, a corresponding C1 activity record must be created on the "right side" (AC) of the cash reconciliation to balance the entry. The program automates this offset creation and includes idempotency protection to prevent duplicate processing.

---

## Processing Steps

### Step 1: Environment Initialization (Lines 13-19)

1. **Set sd080 = 99999999** (Line 13) - Configure system maximum record limit
2. **Declare variables** (Lines 14-15) - Define all program variables via `OcLVar_Define`
3. **Construct output filename** (Lines 16-17):
   - Base directory from `$XDAT` environment variable
   - Fixed prefix: `OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.`
   - Date stamp: current date formatted as `Z8` (YYYYMMDD)
   - Time stamp: current time formatted as `Z6` (HHMMSS)
   - Extension: `.DAT`
4. **Display filename** (Line 18) - Log via `OcShow`
5. **Open output file** (Line 19) - `OcFile1_Open` in OUTPUT mode

### Step 2: Date Range Calculation (Lines 21-29)

1. **Read run date** (Line 21) - Convert `$RUN-DATE` environment variable to numeric
2. **Validate run date** (Line 22) - `OcDate_Valid(RunDate)`
3. **If valid**:
   - `SevenDaysAgo` = RunDate minus 7 calendar days (Line 23)
   - `LastBusiness` = RunDate minus 1 business day (Line 24)
4. **If invalid** (fallback):
   - `SevenDaysAgo` = Current date minus 7 calendar days (Line 26)
   - `LastBusiness` = Current date minus 1 business day (Line 27)
5. **Display dates** (Line 29) - Log RunDate, SevenDaysAgo, LastBusiness via `OcShow`

### Step 3: POPP Record Query (Line 31)

Open a database view on the POPP object:
- **Security ID**: `POOLLOAN3`
- **Date range**: `SevenDaysAgo` to `LastBusiness`
- Returns all position records matching the criteria

### Step 4: Record Processing Loop (Lines 32-57)

For each POPP record returned:

1. **Extract fields** (Lines 33-36):
   - `RKPlan` = Plan identifier (DE 030)
   - `TradeDate` = Trade date (DE 008)
   - `Secondary1Buys` = Buy amount (DE 741)
   - `PriorCashApplied` = Previously applied value (DE 877/UDF1)

2. **Check for reversal activity** (Lines 37-39):
   - If `Secondary1Buys <> 0`, call `CHECK.SSSA` routine
   - This adjusts Secondary1Buys to reflect net buy/sell activity

3. **Idempotency and output check** (Lines 40-56):
   - Condition: `PriorCashApplied <> Secondary1Buys AND Secondary1Buys <> 0`
   - If already processed (values match) or zero activity: **skip**
   - Otherwise:
     a. Re-read RKPlan and TradeDate from POPP record (Lines 41-42)
     b. Calculate `NewLoanUnits = 0 - Secondary1Buys` (Line 43)
     c. Read `TrustAccount` from DE 01510 (Line 44)
     d. Build C1 output record in `Line` buffer (Lines 45-52)
     e. Write record to output file (Line 53)
     f. Update POPP DE 877 (UDF1) with Secondary1Buys (Line 54) - marks as processed
     g. Persist POPP update (Line 55)

---

## Variables Read

| Variable | Source | Purpose |
|---|---|---|
| RunDate | Environment `$RUN-DATE` | Determine processing date range |
| SevenDaysAgo | Calculated | Lower bound of POPP query |
| LastBusiness | Calculated | Upper bound of POPP query, activity date |
| RKPlan | POPP DE 030 | Plan identifier for SSSA lookup and output |
| TradeDate | POPP DE 008 | Date for SSSA lookup |
| Secondary1Buys | POPP DE 741 | Buy amount (adjusted by CHECK.SSSA) |
| PriorCashApplied | POPP DE 877 | Idempotency check value |
| TrustAccount | POPP DE 01510 | Trust account for output record |

## Variables Modified

| Variable | Line(s) | Modification |
|---|---|---|
| FileName | 16 | Constructed from environment + date/time |
| RunDate | 21 | Read from environment |
| SevenDaysAgo | 23 or 26 | Calculated from RunDate or current date |
| LastBusiness | 24 or 27 | Calculated from RunDate or current date |
| RKPlan | 33, 41 | Read from POPP record |
| TradeDate | 34, 42 | Read from POPP record |
| Secondary1Buys | 35 | Read from POPP record (also modified by CHECK.SSSA) |
| PriorCashApplied | 36 | Read from POPP record |
| NewLoanUnits | 43 | Negation of Secondary1Buys |
| TrustAccount | 44 | Read from POPP record |
| Line | 45-52 | Built incrementally for output |

## Dependencies

| Dependency | Type | Description |
|---|---|---|
| CHECK.SSSA | Routine call | Adjusts Secondary1Buys for reversals |
| poppobj | Database object | POPP position records |
| OcFile1 | File I/O | Output file for C1 records |
| $XDAT | Environment variable | Output directory path |
| $RUN-DATE | Environment variable | Processing run date |

## Error Handling

- **RunDate validation**: Falls back to current date if `$RUN-DATE` is invalid (Line 22-28)
- **No explicit error handling** for: file open failure, database view failure, file write failure, POPP update failure
- **Idempotency protection**: Prevents duplicate processing via PriorCashApplied check

## Performance Characteristics

- **Loop complexity**: O(N) where N = number of POPP records for POOLLOAN3 in 7-day window
- **Nested DB calls**: Each qualifying record triggers a CHECK.SSSA SSSA view (additional DB I/O)
- **File I/O**: One write per qualifying record
- **Database updates**: One setde + update per qualifying record
