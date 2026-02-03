# GAP_NewLoanCash Data Dictionary

## Program: GAP_NewLoanCash
**Purpose**: Process Plan Position Accounts for loan cash reconciliation  
**Last Updated**: 2026-02-03

---

## Global Variables

### sd080
- **Data Type**: Numeric (String representation)
- **Initial Value**: 99999999
- **Purpose**: Special error code or status indicator used in OMNISCRIPT environment
- **Declared**: Line 13
- **Assignments**: Line 13 (initialization only)
- **Usage Pattern**: Set once at program initialization
- **Business Logic**: Standard OMNISCRIPT system variable for error handling context

---

### FileName
- **Data Type**: String (Dynamic path)
- **Purpose**: Stores the constructed output file path for C1 activity records
- **Declared**: Line 16
- **Assignments**: Line 16 (constructed from environment variables and timestamps)
- **Usage Pattern**: 
  - Built using `$XDAT` environment variable
  - Includes date (`OcDate_Current()`) and time (`OcTime_Current()`) in filename
  - Format: `OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.YYYYMMDD.HHMMSS.DAT`
- **Business Logic**: Unique output file for each program execution, preventing overwrites
- **Related Variables**: None
- **File Operations**: Used in `OcFile1_Open()` at line 19

---

### RunDate
- **Data Type**: Numeric (Date format YYYYMMDD)
- **Purpose**: Execution date for the batch process, sourced from environment
- **Declared**: Line 21
- **Assignments**: Line 21 (from `$RUN-DATE` environment variable)
- **Usage Pattern**: 
  - Retrieved from environment variable `$RUN-DATE`
  - Validated using `OcDate_Valid()` function
  - Used to calculate date ranges for data processing
- **Business Logic**: Controls the effective date for position account processing
- **Related Variables**: `SevenDaysAgo`, `LastBusiness`
- **Validation**: Must be valid date format; falls back to current date if invalid

---

### SevenDaysAgo
- **Data Type**: Numeric (Date format YYYYMMDD)
- **Purpose**: Start date for the 7-day lookback window for position records
- **Declared**: Line 23
- **Assignments**: 
  - Line 23 (conditional on valid RunDate)
  - Line 26 (fallback if RunDate invalid)
- **Usage Pattern**: 
  - Calculated as `RunDate - 7 calendar days` OR `Current Date - 7 calendar days`
  - Used as lower bound in `poppobj_view()` query
- **Business Logic**: Defines the beginning of the processing window for plan positions
- **Related Variables**: `RunDate`, `LastBusiness`
- **Database Usage**: Used in `poppobj_view(datelo:SevenDaysAgo datehi:LastBusiness)` at line 31

---

### LastBusiness
- **Data Type**: Numeric (Date format YYYYMMDD)
- **Purpose**: Last business day (excluding weekends/holidays) for position processing
- **Declared**: Line 24
- **Assignments**: 
  - Line 24 (conditional on valid RunDate)
  - Line 27 (fallback if RunDate invalid)
- **Usage Pattern**: 
  - Calculated as `RunDate - 1 business day` OR `Current Date - 1 business day`
  - Used as upper bound in `poppobj_view()` query
  - Used as effective date in C1 activity records (line 47)
- **Business Logic**: Ensures processing only includes completed business days
- **Related Variables**: `RunDate`, `SevenDaysAgo`
- **Database Usage**: Upper date filter in position view query

---

### RKPlan
- **Data Type**: String (Plan identifier, 6 characters)
- **Purpose**: Retirement plan identifier from position records
- **Declared**: Line 33
- **Assignments**: 
  - Line 33 (from `poppobj_de(030)` - first retrieval)
  - Line 41 (from `poppobj_de(030)` - second retrieval inside conditional)
- **Usage Pattern**: 
  - Retrieved from position object field 030
  - Used in C1 activity record construction (positions 5-10)
  - Used as filter in `sssaobj_view()` for reversal checks
- **Business Logic**: Identifies which retirement plan the transaction applies to
- **Related Variables**: `TradeDate`, `TrustAccount`
- **Database Usage**: 
  - Read from `poppobj` at lines 33, 41
  - Used in `sssaobj_view(PLAN:RKPlan ...)` at line 62
- **C1 Record Position**: Positions 5-10 (6 bytes)

---

### TradeDate
- **Data Type**: Numeric (Date format YYYYMMDD)
- **Purpose**: Trade date of the position record transaction
- **Declared**: Line 34
- **Assignments**: 
  - Line 34 (from `poppobj_numde(008)` - first retrieval)
  - Line 42 (from `poppobj_numde(008)` - second retrieval)
- **Usage Pattern**: 
  - Retrieved from position object numeric field 008
  - Used as date filter in `sssaobj_view()` for matching activity
- **Business Logic**: Identifies when the position transaction occurred
- **Related Variables**: `RKPlan`, `Secondary1Buys`
- **Database Usage**: 
  - Read from `poppobj` at lines 34, 42
  - Used in `sssaobj_view(DATE:TradeDate)` at line 62

---

### Secondary1Buys
- **Data Type**: Numeric (Dollar amount with decimal precision)
- **Purpose**: Dollar amount of secondary market purchases (loan activity)
- **Declared**: Line 35
- **Assignments**: 
  - Line 35 (from `poppobj_numde(741)` - POPP field 741)
  - Line 73 (calculated from `WK001` after reversal check)
- **Usage Pattern**: 
  - Retrieved from position object field 741
  - Checked for reversal activity in `CHECK.SSSA` routine
  - Compared against `PriorCashApplied` to determine if update needed
  - Negated and written to C1 record as `NewLoanUnits`
- **Business Logic**: 
  - Represents loan purchase amount that needs cash offset
  - May be adjusted for reversals (sells that offset buys)
  - Zero values are skipped (no activity)
- **Related Variables**: `PriorCashApplied`, `NewLoanUnits`, `WK001`
- **Database Usage**: 
  - Read from `poppobj` field 741 at line 35
  - Written back to `poppobj` field 877 at line 54
- **Validation**: Must be non-zero to process

---

### PriorCashApplied
- **Data Type**: Numeric (Dollar amount)
- **Purpose**: Previously recorded cash amount in POPP UDF1 field to prevent duplicate processing
- **Declared**: Line 36
- **Assignments**: Line 36 (from `poppobj_numde(877)` - POPP field 877)
- **Usage Pattern**: 
  - Retrieved from position object field 877 (UDF1)
  - Compared to `Secondary1Buys` to check if already processed
  - If equal, record is skipped (already updated)
- **Business Logic**: Idempotency control - prevents reprocessing the same position record
- **Related Variables**: `Secondary1Buys`
- **Database Usage**: Read from `poppobj` field 877 at line 36
- **Validation Logic**: `IF (PriorCashApplied <> Secondary1Buys)` determines processing

---

### NewLoanUnits
- **Data Type**: Numeric (Dollar amount with sign, negative)
- **Purpose**: Calculated negative value of loan purchases for C1 activity record
- **Declared**: Line 43
- **Assignments**: Line 43 (calculated as `0 - Secondary1Buys`)
- **Usage Pattern**: 
  - Calculated by negating `Secondary1Buys`
  - Formatted as `Z,12V2-` (12 digits with 2 decimals, signed)
  - Written to C1 record positions 116-130
- **Business Logic**: Represents cash offset (negative) for loan purchases in reconciliation
- **Related Variables**: `Secondary1Buys`
- **C1 Record Position**: Positions 116-130 (15 bytes, formatted with sign)

---

### TrustAccount
- **Data Type**: String (Account number, 32 characters)
- **Purpose**: Trust account identifier for the C1 activity record
- **Declared**: Line 44
- **Assignments**: Line 44 (from `poppobj_de(01510)` - POPP field 1510)
- **Usage Pattern**: 
  - Retrieved from position object field 1510
  - Written directly to C1 record positions 40-71
- **Business Logic**: Identifies which trust account the cash activity applies to
- **Related Variables**: `RKPlan`
- **Database Usage**: Read from `poppobj` field 1510 at line 44
- **C1 Record Position**: Positions 40-71 (32 bytes)

---

### Line
- **Data Type**: String (Fixed-length record, 134+ bytes)
- **Purpose**: Formatted C1 activity record for output file
- **Declared**: Line 14 (via `OcLVar_Define`)
- **Assignments**: Lines 45-52 (built incrementally using `OcText_Set`)
- **Usage Pattern**: 
  - Constructed using multiple `OcText_Set()` calls
  - Written to output file using `OcFile1_Write()` at line 53
- **Business Logic**: Contains the complete C1 transaction record for cash reconciliation
- **Related Variables**: All output variables contribute to this
- **File Operations**: Written via `OcFile1_Write(Line)` at line 53

#### Line Field Layout:
| Position | Length | Field | Source Variable |
|----------|--------|-------|-----------------|
| 1-4      | 4      | Record Type | 'C100' (constant) |
| 5-10     | 6      | Plan ID | RKPlan |
| 31-38    | 8      | Effective Date | LastBusiness (formatted Z8) |
| 40-71    | 32     | Trust Account | TrustAccount |
| 73-92    | 20     | Transaction Code | '000000000000000    2' (constant) |
| 115      | 1      | Sign | '0' (constant) |
| 116-130  | 15     | Amount | NewLoanUnits (formatted Z,12V2-) |
| 134-138  | 5      | Activity Code | '00339' (constant) |

---

## Local Variables (Routine-Specific)

### WK001
- **Data Type**: Numeric (Dollar amount)
- **Purpose**: Work variable to calculate net loan activity accounting for reversals
- **Scope**: Local to `CHECK.SSSA` routine
- **Declared**: Implicitly (first use at line 61)
- **Assignments**: 
  - Line 61 (initialized to 0)
  - Line 65 (accumulated for Buy transactions)
  - Line 68 (decremented for Sell/reversal transactions)
  - Line 73 (assigned back to `Secondary1Buys`)
- **Usage Pattern**: 
  - Accumulator for calculating net position after reversals
  - Adds buy amounts, subtracts sell amounts
  - Final net value replaces `Secondary1Buys`
- **Business Logic**: 
  - Handles loan reversal scenarios where sells offset buys
  - Ensures accurate net loan activity for cash reconciliation
- **Related Variables**: `Secondary1Buys`
- **Database Usage**: Accumulates values from `sssaobj_numde(235)` at lines 65, 68

---

## Variable Mutation Analysis

### High-Frequency Mutations (3+ Locations)

#### Secondary1Buys
- **Initial Assignment**: Line 35 (from database field 741)
- **Conditional Check**: Line 37 (reversal check trigger)
- **Comparison**: Line 40 (idempotency check)
- **Calculation**: Line 43 (negated for C1 record)
- **Database Write**: Line 54 (written to field 877)
- **Modification**: Line 73 (updated from reversal calculation)
- **Mutation Count**: 6 references, 3 modifications
- **Risk Assessment**: HIGH - Critical for accurate cash reconciliation
- **Validation**: Must ensure reversal logic correctly nets buys/sells

---

## Database Object Reference Guide

### poppobj (Plan Position Object)
- **Purpose**: Plan position accounts table/view
- **Query**: `poppobj_view(securityid:'POOLLOAN3' datelo:SevenDaysAgo datehi:LastBusiness)`
- **Operations**: View, Next, SetDE, Update
- **Fields Used**:
  - Field 008: TradeDate (numeric)
  - Field 030: RKPlan (string)
  - Field 741: Secondary1Buys (numeric) - UDF custom field
  - Field 877: PriorCashApplied (numeric) - UDF1 field
  - Field 1510: TrustAccount (string)
- **Update Pattern**: Field 877 updated with Secondary1Buys value for idempotency

### sssaobj (Secondary Security Activity Object)
- **Purpose**: TRUSTTRANS.P1 security activity records
- **Query**: `sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate)`
- **Operations**: View, Next
- **Fields Used**:
  - Field 009: Transaction Type (B=Buy, S=Sell)
  - Field 011: Transaction Source ('XI')
  - Field 235: Transaction Amount (numeric)
- **Business Logic**: Used to detect and calculate loan reversals

---

## Built-In Function Reference

### Date/Time Functions
- **OcDate_Current()**: Returns current system date (YYYYMMDD)
- **OcTime_Current()**: Returns current system time (HHMMSS)
- **OcDate_Valid(date)**: Validates date format and value
- **OcDate_AddDays(date, days)**: Adds calendar days to date
- **OcDate_AddBusDays(date, days)**: Adds business days to date (skips weekends/holidays)

### String/Formatting Functions
- **OcFMT(value, format)**: Formats numeric values
  - 'Z8': Zero-filled 8-digit format (dates)
  - 'Z6': Zero-filled 6-digit format (times)
  - 'Z,12V2-': 12 digits with 2 decimals, comma-separated, signed
- **OcText_string()**: Concatenates text strings
- **OcText_Set(Line, position, value, length)**: Sets substring in fixed-length record
- **OcText_GetEnv(variable)**: Retrieves environment variable value
- **OcText_ToNum(string)**: Converts string to numeric

### File I/O Functions
- **OcFile1_Open(name:, mode:)**: Opens file for reading/writing
- **OcFile1_Write(record)**: Writes record to file

### Display Functions
- **OcShow(variables...)**: Displays variable values (debugging/logging)

---

## Summary Statistics

- **Total Global Variables**: 11
- **Total Local Variables**: 1
- **Database Objects Referenced**: 2 (poppobj, sssaobj)
- **Database Operations**: 6 distinct operations
- **Built-In Functions Used**: 13 distinct functions
- **Routines/Procedures**: 1 (CHECK.SSSA)
- **Control Structures**: 5 IF statements, 2 LOOP structures
- **File Operations**: 1 output file (C1 activity records)

---

## Data Flow Summary

1. **Initialization**: Set error context, build output filename
2. **Date Calculation**: Determine 7-day processing window from RunDate or current date
3. **Position Retrieval**: Query plan positions for POOLLOAN3 security within date range
4. **For Each Position**:
   - Extract plan, trade date, loan amounts, prior applied cash, trust account
   - Check for reversals if Secondary1Buys > 0 (calls CHECK.SSSA)
   - Compare PriorCashApplied to Secondary1Buys for idempotency
   - If different (not already processed):
     - Build C1 activity record with negated loan amount
     - Write C1 record to output file
     - Update position record field 877 with Secondary1Buys value
5. **CHECK.SSSA Routine**:
   - Query security activity for same plan/security/date
   - Net buys (+) and sells (-) for transaction source 'XI'
   - Return adjusted Secondary1Buys amount

---

*This data dictionary was generated using the OMNISCRIPT Grammar Parser and manual program analysis.*
