# GAP_NewLoanCash Data Dictionary

## Program Variables

This document provides a comprehensive data dictionary for all variables used in the GAP_NewLoanCash OmniScript program.

---

## Global Variables

### `sd080`
- **Data Type**: Numeric
- **Initial Value**: `99999999`
- **Purpose**: Standard OmniScript variable, likely used for system-level operations or status codes
- **Usage Pattern**: Initialized at program start
- **Modified**: Line 11 (initialization)
- **Read**: Not explicitly referenced in business logic
- **Notes**: Common OmniScript convention for system variables

---

## Program-Defined Variables

### `FileName`
- **Data Type**: String (Text)
- **Purpose**: Stores the complete path and name of the output file for C1 activity records
- **Construction**: 
  - Base directory: `$XDAT`
  - File pattern: `OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.[DATE].[TIME].DAT`
  - Date format: `Z8` (YYYYMMDD)
  - Time format: `Z6` (HHMMSS)
- **Usage Pattern**: 
  - Constructed: Line 13-14
  - Displayed: Line 15 (OcShow)
  - Used for file open: Line 16
- **Example Value**: `/path/to/xdat/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.20260123.143056.DAT`
- **Notes**: Unique filename generated per execution using current date/time

### `RunDate`
- **Data Type**: Numeric (Date)
- **Purpose**: The business date for the current run, sourced from environment variable
- **Initialization**: Retrieved from `$RUN-DATE` environment variable (Line 18)
- **Usage Pattern**:
  - Retrieved: Line 18
  - Validated: Line 19 (OcDate_Valid check)
  - Used to calculate: `SevenDaysAgo`, `LastBusiness`
  - Displayed: Line 26
- **Business Logic**: 
  - If valid date in environment: Use it
  - If invalid/missing: Fall back to current date
- **Notes**: Critical for determining processing date range

### `SevenDaysAgo`
- **Data Type**: Numeric (Date)
- **Purpose**: Start date for the 7-day lookback period to process position accounts
- **Calculation**:
  - If `RunDate` valid: `OcDate_AddDays(RunDate, -7)` (Line 20)
  - Otherwise: `OcDate_AddDays(OcDate_Current(), -7)` (Line 23)
- **Usage Pattern**:
  - Calculated: Lines 20 or 23
  - Displayed: Line 26
  - Used in query: Line 28 (poppobj_view datelo parameter)
- **Business Logic**: Defines the starting boundary for retrieving plan position accounts
- **Notes**: Ensures processing covers previous 7 calendar days

### `LastBusiness`
- **Data Type**: Numeric (Date)
- **Purpose**: The last business day (prior to run date) used for date stamping C1 records
- **Calculation**:
  - If `RunDate` valid: `OcDate_AddBusDays(RunDate, -1)` (Line 21)
  - Otherwise: `OcDate_AddBusDays(OcDate_Current(), -1)` (Line 24)
- **Usage Pattern**:
  - Calculated: Lines 21 or 24
  - Displayed: Line 26
  - Used in query: Line 28 (poppobj_view datehi parameter)
  - Written to output: Line 44 (C1 record field position 31-38)
- **Business Logic**: C1 activity records are dated with the last business day
- **Notes**: Business day calculation excludes weekends and holidays

### `RKPlan`
- **Data Type**: String
- **Purpose**: Plan identifier from position account record
- **Source**: Retrieved from POPP database object field 030
- **Usage Pattern**:
  - Retrieved: Lines 30, 38 (from poppobj_de)
  - Displayed: Not explicitly shown
  - Written to output: Line 43 (C1 record field position 5-10)
  - Used in SSSA query: Line 57 (CHECK.SSSA routine)
- **Business Logic**: Links C1 activity to specific retirement plan
- **Notes**: 6-character field in C1 record format

### `TradeDate`
- **Data Type**: Numeric (Date)
- **Purpose**: The trade date from the position account record
- **Source**: Retrieved from POPP database object field 008
- **Usage Pattern**:
  - Retrieved: Lines 31, 39 (from poppobj_numde)
  - Displayed: Not explicitly shown
  - Used in SSSA query: Line 58 (CHECK.SSSA routine)
- **Business Logic**: Identifies when the loan transaction occurred
- **Conditions**: Must be non-zero to query SSSA (Line 56)
- **Notes**: Used to match corresponding SSSA activity records

### `Secondary1Buys`
- **Data Type**: Numeric (Decimal)
- **Purpose**: Dollar amount of secondary market loan purchases (new loans)
- **Source**: 
  - Initially: Retrieved from POPP database object field 741 (Line 32)
  - Recalculated: In CHECK.SSSA routine by analyzing SSSA buy/sell activity (Lines 60-67)
- **Usage Pattern**:
  - Retrieved: Line 32
  - Validated: Line 33 (check if non-zero to trigger SSSA check)
  - Validated: Line 36 (check if different from PriorCashApplied)
  - Recalculated: Line 68 (after SSSA aggregation)
  - Negated for output: Line 40 (NewLoanUnits = 0 - Secondary1Buys)
  - Written to POPP: Line 49 (updates field 877)
- **Business Logic**: 
  - Represents cash required for new loan purchases
  - Must be validated against SSSA for reversal activity
  - Netted with sales to handle reversals correctly
- **Notes**: Critical field for cash reconciliation accuracy

### `PriorCashApplied`
- **Data Type**: Numeric (Decimal)
- **Purpose**: Previous cash amount applied to this position (stored in UDF1 field 877)
- **Source**: Retrieved from POPP database object field 877 (Line 33)
- **Usage Pattern**:
  - Retrieved: Line 33
  - Compared: Line 36 (if different from Secondary1Buys, record needs processing)
- **Business Logic**: 
  - Acts as idempotency check to prevent duplicate processing
  - If value matches Secondary1Buys, record was already processed
  - If different, C1 record needs to be created and POPP updated
- **Notes**: Prevents re-processing of previously handled transactions

### `Line`
- **Data Type**: String (120 characters)
- **Purpose**: Formatted C1 activity record output line
- **Structure**: Fixed-width format with specific field positions:
  - Position 1-4: `'C100'` (record type)
  - Position 5-10: RKPlan (6 chars)
  - Position 31-38: LastBusiness date (8 chars, Z8 format)
  - Position 40-71: TrustAccount (32 chars)
  - Position 73-92: `'000000000000000    2'` (20 chars - Position 92 = '2')
  - Position 115: `'0'` (1 char)
  - Position 116-130: NewLoanUnits formatted (15 chars, Z,12V2- format)
  - Position 134-138: `'00339'` (5 chars - activity code)
- **Usage Pattern**:
  - Constructed: Lines 42-48 (field by field using OcText_Set)
  - Written: Line 48 (OcFile1_Write)
- **Business Logic**: Creates standardized C1 record for cash reconciliation
- **Notes**: Position 92 set to '2' per GPD-1704 correction (06/27/2024)

### `TrustAccount`
- **Data Type**: String
- **Purpose**: Trust account number associated with the position
- **Source**: Retrieved from POPP database object field 01510 (Line 41)
- **Usage Pattern**:
  - Retrieved: Line 41
  - Written to output: Line 44 (C1 record field position 40-71)
- **Business Logic**: Links C1 activity to specific trust account for reconciliation
- **Notes**: 32-character field in C1 record format

### `NewLoanUnits`
- **Data Type**: Numeric (Decimal)
- **Purpose**: Negated loan amount for C1 record (cash offset)
- **Calculation**: `0 - Secondary1Buys` (Line 40)
- **Usage Pattern**:
  - Calculated: Line 40
  - Written to output: Line 47 (formatted as Z,12V2-)
- **Business Logic**: 
  - Negative value represents cash required for loan purchase
  - Right side (AC) of cash reconciliation entry
- **Format**: 15-character field with 2 decimal places, comma separators, negative sign
- **Notes**: Represents the cash impact of new loan activity

### `WK001`
- **Data Type**: Numeric (Decimal)
- **Purpose**: Working variable to accumulate net buy/sell activity from SSSA
- **Scope**: Local to CHECK.SSSA routine
- **Usage Pattern**:
  - Initialized: Line 56 (WK001 = 0)
  - Accumulated: Lines 61, 64 (add buys, subtract sells)
  - Assigned to Secondary1Buys: Line 68
- **Business Logic**: 
  - Accumulates all 'XI' transaction type amounts
  - Adds 'B' (Buy) transactions: `WK001 = WK001 + sssaobj_numde(235)`
  - Subtracts 'S' (Sell/Reversal) transactions: `WK001 = WK001 - sssaobj_numde(235)`
  - Produces net loan activity accounting for reversals
- **Notes**: Critical for handling loan reversal activity correctly (added 09/25/2024)

---

## Database Object Fields Referenced

### POPP Object (Plan Position)
- **Field 008**: TradeDate - Date of the transaction
- **Field 030**: RKPlan - Plan identifier
- **Field 741**: Secondary1Buys - Secondary market buy amount (initial value)
- **Field 877**: PriorCashApplied - UDF1 field storing previously applied cash
- **Field 01510**: TrustAccount - Associated trust account number

### SSSA Object (Security Settlement Activity)
- **Field 009**: Transaction type ('B' = Buy, 'S' = Sell/Reversal)
- **Field 011**: Activity code (filtered for 'XI')
- **Field 235**: Transaction amount (dollar value)

---

## Environment Variables

### `$XDAT`
- **Purpose**: Environment variable pointing to the data directory for output files
- **Usage**: Used in FileName construction (Line 13)
- **Notes**: Must be set in runtime environment

### `$RUN-DATE`
- **Purpose**: Environment variable containing the business date for the run
- **Usage**: Retrieved and validated as RunDate (Line 18)
- **Format**: Numeric date value
- **Notes**: If invalid or missing, current date is used as fallback

---

## Constants

### Record Type: `'C100'`
- **Value**: String literal `'C100'`
- **Purpose**: Identifies C1 activity record type
- **Position**: Characters 1-4 of output record
- **Usage**: Line 42

### Position Indicator: `'000000000000000    2'`
- **Value**: 20-character string with '2' at position 92 (relative to field start)
- **Purpose**: Position indicator for C1 record format
- **Position**: Characters 73-92 of output record
- **Usage**: Line 45
- **History**: Changed from '1' to '2' per GPD-1704 (06/27/2024)

### Activity Code: `'00339'`
- **Value**: String literal `'00339'`
- **Purpose**: Identifies the type of C1 activity (new loan cash offset)
- **Position**: Characters 134-138 of output record
- **Usage**: Line 48

### Transaction Filter: `'XI'`
- **Value**: String literal `'XI'`
- **Purpose**: Filters SSSA records for specific transaction type
- **Usage**: Line 60 (conditional check in CHECK.SSSA)

### Buy Indicator: `'B'`
- **Value**: String literal `'B'`
- **Purpose**: Identifies buy transactions in SSSA
- **Usage**: Line 61 (conditional check)

### Sell Indicator: `'S'`
- **Value**: String literal `'S'`
- **Purpose**: Identifies sell/reversal transactions in SSSA
- **Usage**: Line 63 (conditional check)

---

## Variable Relationships

### Date Calculation Chain
```
$RUN-DATE → RunDate → SevenDaysAgo (RunDate - 7 days)
                   → LastBusiness (RunDate - 1 business day)
```

### SSSA Verification Chain
```
Secondary1Buys (POPP) → CHECK.SSSA → WK001 (accumulator) → Secondary1Buys (recalculated)
                                   ↓
                              Net Buy/Sell Activity
```

### C1 Record Construction
```
RKPlan, TrustAccount, LastBusiness, Secondary1Buys → Line (formatted record) → Output File
```

### Idempotency Check
```
Secondary1Buys vs PriorCashApplied → If different: Process record
                                    → If same: Skip (already processed)
```

---

## Buffer Sizes and Limits

### `Line` Variable
- **Maximum Size**: 138+ characters
- **Risk**: String overflow if field positions exceed buffer capacity
- **Mitigation**: Fixed-format fields with defined positions prevent overflow

### `FileName` Variable
- **Maximum Size**: Variable based on $XDAT path length
- **Risk**: Path length exceeds system limits on some platforms
- **Mitigation**: Standard paths should be within 256-character limit

### Record Accumulation
- **Limit**: No explicit limit on number of records processed in loop
- **Risk**: Very large datasets could cause memory issues
- **Mitigation**: Streaming record processing (one at a time) minimizes memory usage

---

## Notes

### Date Handling
- All date calculations use OmniScript built-in date functions
- Business day calculations respect holiday calendars
- Date format conversions use Z8 format (YYYYMMDD)

### Decimal Precision
- Financial amounts use OmniScript numeric types
- Output formatting uses Z,12V2- (12 integer digits, 2 decimals, comma separators, negative sign)
- Precision sufficient for typical financial calculations

### Variable Naming Conventions
- PascalCase for program variables (FileName, RunDate, etc.)
- UPPERCASE for database field names (POOLLOAN3, TRUSTTRANS)
- Working variables prefixed with WK (WK001)

---

**AI-Generated Documentation Notice**: This data dictionary was generated using AI analysis and should be reviewed by OmniScript experts for accuracy.

**Last Updated**: 2026-01-23
**Program Version**: Includes GPD-1704 correction (06/27/2024) and reversal handling (09/25/2024)
