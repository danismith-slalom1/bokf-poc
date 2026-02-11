# GAP_NEWLOANCASH Data Dictionary

## Overview

This data dictionary documents all variables declared and used in the GAP_NewLoanCash OmniScript program. Variables are grouped by functional purpose and include data type, usage patterns, modification points, and business significance.

---

## 1. System/Environment Variables

### sd080

| Attribute | Value |
|---|---|
| **Data Type** | Numeric |
| **Initial Value** | `99999999` |
| **Declared** | Line 13 |
| **Assigned** | Line 13 |
| **Read By** | System/environment (not explicitly referenced in code) |
| **Purpose** | System control variable; likely sets maximum database record limit or processing ceiling for OmniScript environment |
| **Business Significance** | Ensures the program can process up to the maximum expected volume of records |
| **Notes** | Set at program start but not explicitly referenced in logic. This is a common OmniScript environment setting (`sd080 = 99999999` typically controls the maximum number of records returned by database views). |

### RunDate

| Attribute | Value |
|---|---|
| **Data Type** | Numeric (date in YYYYMMDD format) |
| **Initial Value** | From environment variable `$RUN-DATE` |
| **Declared** | Line 21 |
| **Assigned** | Line 21 |
| **Read By** | Date range calculation (Lines 22-28) |
| **Purpose** | Externally supplied run date that controls the processing window |
| **Business Significance** | Allows the program to be run for a specific date context (e.g., batch scheduling) rather than relying solely on the current system date |
| **Validation** | Validated with `OcDate_Valid()` on Line 22; fallback to current date if invalid |

---

## 2. Date Range Variables

### SevenDaysAgo

| Attribute | Value |
|---|---|
| **Data Type** | Numeric (date in YYYYMMDD format) |
| **Initial Value** | Computed: `RunDate - 7 calendar days` or `CurrentDate - 7 calendar days` |
| **Declared** | Line 23 |
| **Assigned** | Lines 23, 26 |
| **Read By** | Database view filter (Line 31: `datelo:SevenDaysAgo`) |
| **Purpose** | Lower bound of the 7-day lookback window for POPP record queries |
| **Business Significance** | Defines how far back the program searches for new loan position records; ensures recent activity is captured |
| **Notes** | Assignment depends on `RunDate` validation - Line 23 when RunDate is valid, Line 26 otherwise |

### LastBusiness

| Attribute | Value |
|---|---|
| **Data Type** | Numeric (date in YYYYMMDD format) |
| **Initial Value** | Computed: `RunDate - 1 business day` or `CurrentDate - 1 business day` |
| **Declared** | Line 24 |
| **Assigned** | Lines 24, 27 |
| **Read By** | Database view filter (Line 31: `datehi:LastBusiness`), Output record (Line 47) |
| **Purpose** | Upper bound of the lookback window and the effective date written to C1 activity records |
| **Business Significance** | Ensures processing uses the last business day (skipping weekends/holidays) as the activity date for cash reconciliation records |
| **Notes** | Uses `OcDate_AddBusDays` which accounts for business day calendar |

---

## 3. Database Record Variables (POPP)

### RKPlan

| Attribute | Value |
|---|---|
| **Data Type** | String (6 characters) |
| **Initial Value** | From POPP data element 030 |
| **Declared** | Line 33 |
| **Assigned** | Lines 33, 41 |
| **Read By** | CHECK.SSSA routine (Line 60, 62), Output record (Line 46) |
| **Purpose** | Plan identifier from the position record |
| **Business Significance** | Identifies the specific investment plan associated with the loan position; used as key field in SSSA lookup and in output C1 records |
| **DB Source** | `poppobj_de(030)` |

### TradeDate

| Attribute | Value |
|---|---|
| **Data Type** | Numeric (date in YYYYMMDD format) |
| **Initial Value** | From POPP data element 008 |
| **Declared** | Line 34 |
| **Assigned** | Lines 34, 42 |
| **Read By** | CHECK.SSSA routine (Line 60, 62) |
| **Purpose** | Trade date of the position record |
| **Business Significance** | Used to correlate POPP positions with SSSA transaction records on the same date |
| **DB Source** | `poppobj_numde(008)` |

### Secondary1Buys

| Attribute | Value |
|---|---|
| **Data Type** | Numeric |
| **Initial Value** | From POPP data element 741 |
| **Declared** | Line 35 |
| **Assigned** | Lines 35, 73 |
| **Read By** | Comparison checks (Lines 37, 40), NewLoanUnits calculation (Line 43), POPP update (Line 54) |
| **Purpose** | Secondary 1 buy amount from the position record; may be adjusted by CHECK.SSSA to reflect net buy/sell activity |
| **Business Significance** | Core business value - represents the new loan cash amount that needs to be offset in the cash reconciliation. The CHECK.SSSA routine adjusts this value to account for reversals. |
| **DB Source** | `poppobj_numde(741)` |
| **Mutation Pattern** | Initially loaded from POPP (Line 35), potentially overwritten by CHECK.SSSA (Line 73) with net buy/sell amount |

### Secondary1Sells

| Attribute | Value |
|---|---|
| **Data Type** | Numeric |
| **Initial Value** | None (declared but unused) |
| **Declared** | Line 14 (in `OcLVar_Define`) |
| **Assigned** | Never |
| **Read By** | Never |
| **Purpose** | Declared but not used in current program logic |
| **Business Significance** | May have been used in a previous version or reserved for future use |
| **Notes** | Dead variable - declared in `OcLVar_Define` but never assigned or read |

### PriorCashApplied

| Attribute | Value |
|---|---|
| **Data Type** | Numeric |
| **Initial Value** | From POPP data element 877 (UDF1) |
| **Declared** | Line 36 |
| **Assigned** | Line 36 |
| **Read By** | Idempotency check (Line 40) |
| **Purpose** | Previously applied cash amount stored in POPP UDF1 field; acts as an idempotency marker |
| **Business Significance** | Prevents duplicate processing - if PriorCashApplied already equals Secondary1Buys, the record has already been processed and is skipped |
| **DB Source** | `poppobj_numde(877)` |

### TrustAccount

| Attribute | Value |
|---|---|
| **Data Type** | String (up to 32 characters) |
| **Initial Value** | From POPP data element 01510 |
| **Declared** | Line 44 |
| **Assigned** | Line 44 |
| **Read By** | Output record (Line 48) |
| **Purpose** | Trust account identifier from the position record |
| **Business Significance** | Identifies the trust account for the C1 activity record; required for cash reconciliation matching |
| **DB Source** | `poppobj_de(01510)` |

---

## 4. Calculation Variables

### NewLoanUnits

| Attribute | Value |
|---|---|
| **Data Type** | Numeric (signed, 12 digits with 2 decimal places) |
| **Initial Value** | `0 - Secondary1Buys` (negation of buy amount) |
| **Declared** | Line 43 |
| **Assigned** | Line 43 |
| **Read By** | Output record (Line 51) |
| **Purpose** | Negated buy amount representing the cash offset for new loan activity |
| **Business Significance** | The C1 activity represents the "right side" (AC) of cash reconciliation, so the buy amount is negated to create the offsetting entry |
| **Format** | Written as `Z,12V2-` (12 digits, 2 decimal places, trailing negative sign) |

---

## 5. Output/File Variables

### FileName

| Attribute | Value |
|---|---|
| **Data Type** | String |
| **Initial Value** | Constructed path: `$XDAT\OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.{date}.{time}.DAT` |
| **Declared** | Line 16 |
| **Assigned** | Line 16 |
| **Read By** | File open (Line 19), Display (Line 18) |
| **Purpose** | Full path to the output C1 activity file |
| **Business Significance** | Output file follows naming convention with date/time stamp ensuring unique file names per run |
| **Components** | `$XDAT` (environment data directory) + fixed prefix + current date (Z8) + current time (Z6) + `.DAT` |

### Line

| Attribute | Value |
|---|---|
| **Data Type** | String (buffer, at least 138 characters) |
| **Initial Value** | Built incrementally via `OcText_Set` calls |
| **Declared** | Line 14 (in `OcLVar_Define`) |
| **Assigned** | Lines 45-52 (positional field setting) |
| **Read By** | File write (Line 53) |
| **Purpose** | Output record buffer for C1 activity file; populated using positional field-setting |
| **Business Significance** | Represents a complete C1 activity record for cash reconciliation |
| **Buffer Layout** | See Output Record Format section below |

---

## 6. Routine-Local Variables

### WK001 (CHECK.SSSA)

| Attribute | Value |
|---|---|
| **Data Type** | Numeric |
| **Scope** | Local to CHECK.SSSA routine |
| **Initial Value** | `0` (Line 61) |
| **Assigned** | Lines 61, 66, 69 |
| **Read By** | Accumulation (Lines 66, 69), Final assignment (Line 73) |
| **Purpose** | Working accumulator for net buy/sell activity from SSSA records |
| **Business Significance** | Aggregates all buy transactions and subtracts sell (reversal) transactions to determine true net new loan activity |
| **Mutation Pattern** | Initialized to 0, incremented for buys (+ amount), decremented for sells (- amount), final value assigned to Secondary1Buys |

---

## 7. Output Record Format (Line Buffer)

The `Line` variable is populated as a fixed-width C1 activity record:

| Position | Length | Field | Source | Value/Format |
|---|---|---|---|---|
| 1-4 | 4 | Record Type | Hardcoded | `C100` |
| 5-10 | 6 | Plan ID | `RKPlan` | String |
| 31-38 | 8 | Activity Date | `LastBusiness` | `Z8` (YYYYMMDD) |
| 40-71 | 32 | Trust Account | `TrustAccount` | String |
| 73-92 | 20 | Control Flags | Hardcoded | `000000000000000    2` |
| 115 | 1 | Processing Flag | Hardcoded | `0` |
| 116-130 | 15 | Offset Amount | `NewLoanUnits` | `Z,12V2-` (signed decimal) |
| 134-138 | 5 | Transaction Code | Hardcoded | `00339` |

**Notes**:
- Position 92 contains value `2` (corrected from `1` per GPD-1704, 06/27/2024)
- Positions 11-30, 39, 72, 93-114, 131-133 are implicitly space-filled
- The `Z,12V2-` format produces a 12-digit number with 2 implied decimals and trailing sign

---

## 8. Variable Mutation Patterns

### Variables Modified in Multiple Locations

#### Secondary1Buys (Modified in 2 locations)

| # | Location | Line | Trigger | Value |
|---|---|---|---|---|
| 1 | Main loop | 35 | POPP record read | Raw value from `poppobj_numde(741)` |
| 2 | CHECK.SSSA routine | 73 | After SSSA accumulation | Net buy/sell amount (WK001) |

**Business Logic**: Secondary1Buys starts as the raw buy amount from the position record. If non-zero, CHECK.SSSA recalculates it by examining all SSSA transactions for the same plan/security/date, netting buys against sells (reversals). This ensures the C1 offset reflects true net activity, not just gross buys.

**Concern**: The mutation in CHECK.SSSA overwrites the POPP-sourced value entirely. If CHECK.SSSA finds no matching SSSA records (sssaobj_next returns false on first call), WK001 stays 0 and Secondary1Buys becomes 0, potentially skipping a valid record. This behavior may be intentional (SSSA is the authoritative source) or could be a risk if SSSA data lags POPP data.

#### RKPlan (Modified in 2 locations)

| # | Location | Line | Trigger | Value |
|---|---|---|---|---|
| 1 | Main loop (pre-check) | 33 | POPP record read | `poppobj_de(030)` |
| 2 | Main loop (post-check) | 41 | Inside output condition | `poppobj_de(030)` (re-read) |

**Business Logic**: RKPlan is read twice from the same POPP record - once at the start of the loop for CHECK.SSSA, and again inside the output condition block. Both reads are from the same data element of the same record, so the value is identical. The re-read at Line 41 appears redundant.

#### TradeDate (Modified in 2 locations)

| # | Location | Line | Trigger | Value |
|---|---|---|---|---|
| 1 | Main loop (pre-check) | 34 | POPP record read | `poppobj_numde(008)` |
| 2 | Main loop (post-check) | 42 | Inside output condition | `poppobj_numde(008)` (re-read) |

**Business Logic**: Same pattern as RKPlan - read twice from the same record. The re-read at Line 42 appears redundant. Both assignments produce the same value since the POPP cursor hasn't moved.

#### LastBusiness (Modified in 2 locations)

| # | Location | Line | Trigger | Value |
|---|---|---|---|---|
| 1 | Date initialization (valid RunDate) | 24 | RunDate is valid | `OcDate_AddBusDays(RunDate, -1)` |
| 2 | Date initialization (fallback) | 27 | RunDate is invalid | `OcDate_AddBusDays(OcDate_Current(), -1)` |

**Business Logic**: Mutually exclusive assignment - exactly one of the two lines executes based on RunDate validation. Not a true multi-mutation concern.

#### SevenDaysAgo (Modified in 2 locations)

| # | Location | Line | Trigger | Value |
|---|---|---|---|---|
| 1 | Date initialization (valid RunDate) | 23 | RunDate is valid | `OcDate_AddDays(RunDate, -7)` |
| 2 | Date initialization (fallback) | 26 | RunDate is invalid | `OcDate_AddDays(OcDate_Current(), -7)` |

**Business Logic**: Same mutually exclusive pattern as LastBusiness. Not a true multi-mutation concern.
