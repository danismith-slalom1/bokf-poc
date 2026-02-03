# GAP_NewLoanCash Data Dictionary

## Program Information
- **Program Name**: GAP_NewLoanCash
- **Program Purpose**: Process new loan cash reconciliation by reading Plan Position Accounts from the last 7 days, finding POOLLOAN3 position records, and building C1 activity for cash reconciliation
- **Last Updated**: 09/25/2024
- **Author**: Gary Matten

---

## Global Variables

### sd080
- **Data Type**: Numeric
- **Initial Value**: 99999999
- **Purpose**: Standard OmniScript control variable used for program initialization
- **Declared At**: Line 13
- **Modified At**: Line 13
- **Usage Context**: Initialization only; not referenced elsewhere in program
- **Special Notes**: Standard OmniScript convention for setting program return code

### FileName
- **Data Type**: String
- **Initial Value**: Dynamically constructed output file path
- **Purpose**: Stores the complete path and name of the output file for C1 activity records
- **Declared At**: Line 16
- **Modified At**: Line 16
- **Usage Context**: 
  - Constructed using environment variable $XDAT
  - Includes timestamp (date and time) for uniqueness
  - Pattern: `$XDAT/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.YYYYMMDD.HHMMSS.DAT`
  - Used to open output file (Line 19)
- **Dependencies**: 
  - `OcText_string()` for string construction
  - `OCTEXT_GETENV()` for environment variable retrieval
  - `OcDate_Current()` and `OcTime_Current()` for timestamp
- **Special Notes**: File naming ensures unique outputs per execution; prevents overwriting previous runs

### RunDate
- **Data Type**: Numeric (Date)
- **Initial Value**: Retrieved from $RUN-DATE environment variable or current date
- **Purpose**: Determines the business date for processing
- **Declared At**: Line 21
- **Modified At**: Line 21
- **Usage Context**:
  - Retrieved from environment variable $RUN-DATE
  - Validated using `OcDate_Valid()`
  - Used to calculate SevenDaysAgo and LastBusiness dates
  - Falls back to current date if environment variable not set or invalid
- **Business Logic**: Central to date range calculations for query scope
- **Special Notes**: Critical for determining processing window (7 days back from RunDate)

### SevenDaysAgo
- **Data Type**: Numeric (Date)
- **Initial Value**: RunDate minus 7 calendar days
- **Purpose**: Defines the start date for Plan Position Account query
- **Declared At**: Line 23
- **Modified At**: Lines 23, 26
- **Usage Context**:
  - Calculated as RunDate - 7 days (Line 23) or Current Date - 7 days (Line 26)
  - Used as datelo parameter in poppobj_view() query (Line 31)
- **Business Logic**: Sets lower bound of date range for position record retrieval
- **Special Notes**: Uses calendar days, not business days

### LastBusiness
- **Data Type**: Numeric (Date)
- **Initial Value**: Last business day before RunDate
- **Purpose**: Represents the business date used in C1 activity records
- **Declared At**: Line 24
- **Modified At**: Lines 24, 27
- **Usage Context**:
  - Calculated using `OcDate_AddBusDays(RunDate -1)` or `OcDate_AddBusDays(OcDate_Current() -1)`
  - Used as:
    - datehi parameter in poppobj_view() query (Line 31)
    - Activity date in C1 output records (Line 47)
- **Business Logic**: Ensures C1 activity uses valid business date, not calendar date
- **Special Notes**: Critical for business day calculation; skips weekends/holidays

### RKPlan
- **Data Type**: String
- **Initial Value**: Retrieved from Plan Position record (DE 030)
- **Purpose**: Stores the plan identifier for the position account
- **Declared At**: Line 33
- **Modified At**: Lines 33, 41
- **Usage Context**:
  - Retrieved from poppobj_de(030) in main loop (Line 33)
  - Reassigned from same source before writing C1 record (Line 41)
  - Used in:
    - C1 output record construction (Line 46, positions 5-10)
    - CHECK.SSSA routine for reversal lookup (Line 62)
- **Business Logic**: Links position records to specific retirement plans
- **Special Notes**: 6-character field in C1 output; key identifier for plan-level processing

### TradeDate
- **Data Type**: Numeric (Date)
- **Initial Value**: Retrieved from Plan Position record (DE 008)
- **Purpose**: Stores the trade date of the position transaction
- **Declared At**: Line 34
- **Modified At**: Lines 34, 42
- **Usage Context**:
  - Retrieved from poppobj_numde(008) in main loop (Line 34)
  - Reassigned from same source before writing C1 record (Line 42)
  - Used in CHECK.SSSA routine to find reversals on same date (Line 62)
- **Business Logic**: Date when loan position was originally recorded
- **Special Notes**: Must be non-zero for CHECK.SSSA reversal processing

### Secondary1Buys
- **Data Type**: Numeric (Currency)
- **Initial Value**: Retrieved from Plan Position record (DE 741)
- **Purpose**: Stores the dollar amount of secondary market loan purchases
- **Declared At**: Line 35
- **Modified At**: Lines 35, 73
- **Usage Context**:
  - Retrieved from poppobj_numde(741) - Secondary1Buys field (Line 35)
  - Compared with PriorCashApplied to check if record already processed (Line 40)
  - Checked for non-zero value to trigger CHECK.SSSA routine (Line 37)
  - Recalculated in CHECK.SSSA routine to account for reversals (Line 73)
  - Stored in UDF1 (DE 877) upon successful processing (Line 54)
- **Business Logic**: 
  - Represents cash applied from trust transactions
  - Can be reversed by subsequent sell transactions
  - Zero value means no processing needed
- **Mutation Pattern**: Modified in CHECK.SSSA to net buy/sell transactions
- **Special Notes**: Core business value driving cash reconciliation; subject to reversal logic

### PriorCashApplied
- **Data Type**: Numeric (Currency)
- **Initial Value**: Retrieved from Plan Position record (DE 877)
- **Purpose**: Stores previously applied cash amount to prevent duplicate processing
- **Declared At**: Line 36
- **Modified At**: Line 36
- **Usage Context**:
  - Retrieved from poppobj_numde(877) - UDF1 field (Line 36)
  - Compared with Secondary1Buys to determine if reprocessing needed (Line 40)
  - If match, record already processed and skipped
- **Business Logic**: Idempotency check; ensures each position only processed once
- **Special Notes**: Uses UDF1 field as processing flag; critical for preventing duplicate C1 records

### NewLoanUnits
- **Data Type**: Numeric (Currency with sign)
- **Purpose**: Stores the negated value of Secondary1Buys for C1 offset entry
- **Declared At**: Line 43
- **Modified At**: Line 43
- **Usage Context**:
  - Calculated as negative of Secondary1Buys: `0 - Secondary1Buys`
  - Written to C1 output record (Line 50, positions 116-130)
  - Formatted as 'Z,12V2-' (12 digits, 2 decimals, with sign)
- **Business Logic**: Creates offsetting entry for cash reconciliation; negative value balances the loan purchase
- **Special Notes**: Always negative; represents cash outflow for loan purchases

### TrustAccount
- **Data Type**: String
- **Initial Value**: Retrieved from Plan Position record (DE 01510)
- **Purpose**: Stores the trust account number associated with the position
- **Declared At**: Line 44
- **Modified At**: Line 44
- **Usage Context**:
  - Retrieved from poppobj_de(01510) (Line 44)
  - Written to C1 output record (Line 48, positions 40-71)
- **Business Logic**: Links C1 activity to specific trust account
- **Special Notes**: 32-character field in C1 output; essential for account-level reconciliation

---

## Local Variables (Routine-Specific)

### WK001 (CHECK.SSSA Routine)
- **Data Type**: Numeric (Currency)
- **Initial Value**: 0
- **Scope**: Local to CHECK.SSSA routine
- **Purpose**: Accumulates net loan activity by processing buy and sell transactions
- **Declared At**: Line 61 (implicit initialization)
- **Modified At**: Lines 61, 66, 69
- **Usage Context**:
  - Initialized to 0 at start of CHECK.SSSA routine
  - Accumulates buys (XI/B transactions): `WK001 = WK001 + sssaobj_numde(235)`
  - Deducts sells (XI/S transactions): `WK001 = WK001 - sssaobj_numde(235)`
  - Final value assigned to Secondary1Buys (Line 73)
- **Business Logic**: 
  - Calculates net loan activity when both buys and reversals exist
  - Handles loan reversal activity (sells) that offset original buys
  - Critical for accurate cash reconciliation when multiple transactions on same date
- **Special Notes**: Added 09/25/2024 to recognize loan reversal activity correctly

---

## Database Objects and Data Elements

### poppobj (Plan Position Object)
- **Purpose**: Plan position account records
- **View Parameters**:
  - securityid: 'POOLLOAN3' (loan pool security)
  - datelo: SevenDaysAgo
  - datehi: LastBusiness
- **Data Elements Used**:
  - DE 008: TradeDate
  - DE 030: RKPlan (Plan ID)
  - DE 741: Secondary1Buys (loan purchase amount)
  - DE 877: UDF1 / PriorCashApplied (processing flag)
  - DE 01510: TrustAccount
- **Operations**: view, next, setde, update

### sssaobj (Secondary Market Activity Object)
- **Purpose**: Secondary market buy/sell activity records
- **View Parameters**:
  - PLAN: RKPlan
  - SECURITYID: 'POOLLOAN3'
  - DATE: TradeDate
- **Data Elements Used**:
  - DE 009: Transaction type (B=Buy, S=Sell)
  - DE 011: Activity code (XI=External In)
  - DE 235: Transaction amount
- **Operations**: view, next
- **Business Logic**: Used to identify reversals and net activity for the same plan/date

---

## C1 Output Record Structure

The program generates C1 activity records with the following fixed-format structure:

| Position | Length | Field | Source/Value |
|----------|--------|-------|--------------|
| 1-4 | 4 | Record Type | 'C100' |
| 5-10 | 6 | Plan ID | RKPlan |
| 31-38 | 8 | Activity Date | LastBusiness (YYYYMMDD format) |
| 40-71 | 32 | Trust Account | TrustAccount |
| 73-92 | 20 | Type/Position | '000000000000000    2' |
| 115 | 1 | Sign Flag | '0' |
| 116-130 | 15 | Amount | NewLoanUnits (formatted as Z,12V2-) |
| 134-138 | 5 | Activity Code | '00339' |

**Output File**: `$XDAT/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.YYYYMMDD.HHMMSS.DAT`

---

## Constants and Literal Values

### Security ID
- **Value**: 'POOLLOAN3'
- **Usage**: Query filter for plan position and secondary activity lookups
- **Purpose**: Identifies loan pool security type

### Activity Code (C1 Output)
- **Value**: '00339'
- **Position**: 134-138
- **Purpose**: Identifies the type of C1 activity being recorded

### Position Value (C1 Output)
- **Value**: '000000000000000    2'
- **Position**: 73-92
- **Purpose**: Position 92 set to '2' per GPD-1704 correction (06/27/2024)
- **Special Notes**: Previously set to '1'; corrected to '2' in June 2024 update

### Record Type (C1 Output)
- **Value**: 'C100'
- **Position**: 1-4
- **Purpose**: Identifies record as C1 activity type 00

---

## OmniScript Built-in Functions Used

### Date/Time Functions
- **OcDate_Current()**: Returns current system date
- **OcDate_Valid()**: Validates date format
- **OcDate_AddDays()**: Adds/subtracts calendar days
- **OcDate_AddBusDays()**: Adds/subtracts business days (skips weekends/holidays)
- **OcTime_Current()**: Returns current system time

### File Operations
- **OcFile1_Open()**: Opens output file
- **OcFile1_Write()**: Writes record to output file

### Text/String Functions
- **OcText_string()**: Constructs strings with dynamic values
- **OcText_Set()**: Sets substring at specific position
- **OcFmt()**: Formats numeric values (Z8, Z6, Z,12V2-)
- **OCTEXT_GETENV()**: Retrieves environment variable value

### Display Functions
- **OcShow()**: Displays values for debugging/logging

---

## Variable Mutation Analysis

### Variables Modified in Multiple Locations

#### Secondary1Buys
- **Assignment 1**: Line 35 - Initial retrieval from poppobj_numde(741)
- **Assignment 2**: Line 73 - Recalculated in CHECK.SSSA routine from WK001
- **Mutation Pattern**: 
  - Initial value from database field
  - Potentially replaced by net calculation (buys - sells) in CHECK.SSSA
  - Mutation occurs when reversal activity detected
- **Business Impact**: Changes the amount used for C1 activity and stored in UDF1
- **Risk Level**: Medium - mutation controlled by explicit reversal logic
- **Mitigation**: Clear separation of initial load (Line 35) and reversal adjustment (Line 73)

#### RKPlan
- **Assignment 1**: Line 33 - Initial retrieval from poppobj_de(030)
- **Assignment 2**: Line 41 - Re-retrieval from poppobj_de(030)
- **Mutation Pattern**: Redundant assignment; same value from same source
- **Business Impact**: None - defensive coding or artifact of code evolution
- **Risk Level**: None
- **Recommendation**: Second assignment appears unnecessary unless poppobj internal state changes

#### TradeDate
- **Assignment 1**: Line 34 - Initial retrieval from poppobj_numde(008)
- **Assignment 2**: Line 42 - Re-retrieval from poppobj_numde(008)
- **Mutation Pattern**: Redundant assignment; same value from same source
- **Business Impact**: None - defensive coding or artifact of code evolution
- **Risk Level**: None
- **Recommendation**: Second assignment appears unnecessary unless poppobj internal state changes

#### SevenDaysAgo
- **Assignment 1**: Line 23 - Calculated from RunDate when valid
- **Assignment 2**: Line 26 - Calculated from OcDate_Current() when RunDate invalid
- **Mutation Pattern**: Conditional initialization based on RunDate validity
- **Business Impact**: Ensures valid date range even when environment variable not set
- **Risk Level**: Low - controlled by explicit validation logic
- **Mitigation**: Clear if/else structure ensures only one calculation executes

#### LastBusiness
- **Assignment 1**: Line 24 - Calculated from RunDate when valid
- **Assignment 2**: Line 27 - Calculated from OcDate_Current() when RunDate invalid
- **Mutation Pattern**: Conditional initialization based on RunDate validity
- **Business Impact**: Ensures valid business date even when environment variable not set
- **Risk Level**: Low - controlled by explicit validation logic
- **Mitigation**: Clear if/else structure ensures only one calculation executes

### Variables Modified Once
All other variables (sd080, FileName, RunDate, NewLoanUnits, PriorCashApplied, TrustAccount, WK001) are assigned once and never modified, indicating stable, predictable behavior.

---

## Processing Logic Summary

1. **Initialization**: Set up output file with timestamped name
2. **Date Calculation**: Determine processing window (7 days back from RunDate or current date)
3. **Main Loop**: Query plan positions for POOLLOAN3 security within date range
4. **Reversal Check**: For non-zero Secondary1Buys, check for reversal activity via CHECK.SSSA
5. **Duplicate Prevention**: Compare PriorCashApplied with Secondary1Buys; skip if match
6. **C1 Generation**: Build fixed-format C1 activity record with offset amount
7. **Mark Processed**: Update position record UDF1 field with Secondary1Buys amount
8. **Output**: Write C1 record to output file

---

## Error Handling and Data Quality Notes

### Implicit Error Handling
- **Date Validation**: RunDate validated before use; falls back to current date
- **Zero Check**: Secondary1Buys <> 0 prevents unnecessary CHECK.SSSA processing
- **Null Check**: RKPlan <> '' and TradeDate <> 0 prevent invalid CHECK.SSSA queries
- **Duplicate Prevention**: PriorCashApplied check prevents reprocessing

### Data Quality Assumptions
- Plan Position records (poppobj) exist and are accessible
- DE fields (008, 030, 741, 877, 01510) contain valid data
- SSSA records (sssaobj) accurately reflect buy/sell transactions
- Environment variable $XDAT contains valid directory path
- Environment variable $RUN-DATE contains valid date (or omitted)

### Known Issues
- No explicit error handling for file open/write failures
- No handling for malformed or missing database records
- No validation of Secondary1Buys amount range or format
- No logging of skipped records (already processed)

---

## Revision History
- **12/21/2023**: Initial OmniScript creation by Gary Matten
- **06/27/2024**: GPD-1704 - Corrected position 92 from '1' to '2'
- **09/25/2024**: Added logic to recognize loan reversal activity and net activity correctly when there are BUYS and SELLS (CHECK.SSSA routine enhancement)

---

## Related Documentation
- [GAP_NewLoanCash Call Graph](GAP_NewLoanCash_CALL_GRAPH.md)
- [GAP_NewLoanCash Comprehensive Documentation](GAP_NewLoanCash_OVERVIEW.md)
- [CHECK.SSSA Procedure Documentation](procedures/CHECK.SSSA.md)
