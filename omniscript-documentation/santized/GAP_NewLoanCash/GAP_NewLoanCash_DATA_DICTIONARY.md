# GAP_NewLoanCash Data Dictionary

**Program**: GAP_NewLoanCash.cbl  
**Last Updated**: February 4, 2026  
**Purpose**: Comprehensive catalog of all variables used in the New Loan Cash reconciliation program

---

## Overview

This data dictionary documents all variables used in the GAP_NewLoanCash program, which processes Plan Position Accounts for loan cash reconciliation. The program reads POOLLOAN3 position records and builds C1 activity records for cash reconciliation.

## Global Variables

### sd080
- **Level**: Global
- **Type**: Numeric
- **Initial Value**: 99999999
- **Purpose**: Standard OMNISCRIPT system variable initialization
- **Usage**: Set at program start (Line 13)
- **Mutations**: Single assignment at initialization

### FileName
- **Level**: Global
- **Type**: String
- **Purpose**: Output file path for C1 activity records
- **Declared**: Line 16
- **Assignments**: Line 16
- **Construction**: Built from environment variable $XDAT concatenated with formatted filename:
  - Pattern: `OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.{YYYYMMDD}.{HHMMSS}.DAT`
  - Uses current date/time for uniqueness
- **Usage**: Opened for OUTPUT mode (Line 19), receives all C1 activity records
- **Example Value**: `/data/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.20260204.143052.DAT`

### RunDate
- **Level**: Global
- **Type**: Numeric (Date)
- **Purpose**: Processing date for the batch run
- **Declared**: Line 21
- **Assignments**: Line 21
- **Source**: Retrieved from environment variable `$RUN-DATE`
- **Validation**: Checked with OcDate_Valid() before use
- **Usage**: Determines date ranges for account processing
- **Dependencies**: Used to calculate SevenDaysAgo and LastBusiness dates

### SevenDaysAgo
- **Level**: Global
- **Type**: Numeric (Date)
- **Purpose**: Start date for position account query (7 days prior to run date)
- **Declared**: Line 23
- **Assignments**: Lines 23, 26
- **Calculation**:
  - If RunDate is valid: `OcDate_AddDays(RunDate, -7)`
  - If RunDate is invalid: `OcDate_AddDays(OcDate_Current(), -7)`
- **Usage**: Lower bound for poppobj_view date range (Line 31)
- **Business Logic**: Ensures processing includes one week of historical position data

### LastBusiness
- **Level**: Global
- **Type**: Numeric (Date)
- **Purpose**: Most recent business day (end date for processing)
- **Declared**: Line 24
- **Assignments**: Lines 24, 27
- **Calculation**:
  - If RunDate is valid: `OcDate_AddBusDays(RunDate, -1)`
  - If RunDate is invalid: `OcDate_AddBusDays(OcDate_Current(), -1)`
- **Usage**: 
  - Upper bound for poppobj_view date range (Line 31)
  - Written to C1 output records (Line 47, positions 31-38)
- **Business Logic**: Uses previous business day to ensure complete settlement data

### RKPlan
- **Level**: Global
- **Type**: String
- **Purpose**: Plan identifier from position object
- **Declared**: Line 33
- **Assignments**: Lines 33, 41
- **Source**: Extracted from poppobj_de(030) - Position Object Data Element 30
- **Usage**:
  - Written to C1 output records (Line 46, positions 5-10)
  - Used as filter for SSSA (Secondary Settlement Activity) query (Line 62)
  - Validated in CHECK.SSSA routine (Line 60)
- **Length**: 6 characters (based on OcText_Set positions)

### TradeDate
- **Level**: Global
- **Type**: Numeric (Date)
- **Purpose**: Trade date of position record
- **Declared**: Line 34
- **Assignments**: Lines 34, 42
- **Source**: Extracted from poppobj_numde(008) - Position Object Numeric Data Element 8
- **Usage**:
  - Used as filter for SSSA query (Line 62)
  - Validated in CHECK.SSSA routine (Line 60)
- **Business Logic**: Matches position record to corresponding settlement activity

### Secondary1Buys
- **Level**: Global
- **Type**: Numeric
- **Purpose**: Dollar amount of secondary market loan purchases
- **Declared**: Line 35
- **Assignments**: Lines 35, 73
- **Sources**:
  - Initial: Extracted from poppobj_numde(741) - UDF Position Data Element 741
  - Updated: Calculated in CHECK.SSSA routine by aggregating SSSA BUY/SELL activity
- **Usage**:
  - Triggers SSSA validation check if non-zero (Line 37)
  - Compared against PriorCashApplied to detect unprocessed records (Line 40)
  - Negated and written to C1 output as NewLoanUnits (Line 43)
  - Stored back to position object UDF1 field (Line 54)
- **Business Logic**: 
  - Represents loan purchase activity requiring cash reconciliation
  - SSSA check accounts for reversals (BUYS - SELLS)
  - Must match PriorCashApplied to skip already-processed records

### PriorCashApplied
- **Level**: Global
- **Type**: Numeric
- **Purpose**: Previously recorded cash application amount (idempotency check)
- **Declared**: Line 36
- **Assignments**: Line 36
- **Source**: Extracted from poppobj_numde(877) - UDF1 Position Data Element 877
- **Usage**: Compared against Secondary1Buys to prevent duplicate processing (Line 40)
- **Business Logic**: If equals Secondary1Buys, record already processed; skip to next

### NewLoanUnits
- **Level**: Global
- **Type**: Numeric
- **Purpose**: Loan units for C1 cash activity record (negative of Secondary1Buys)
- **Declared**: Line 43
- **Assignments**: Line 43
- **Calculation**: `0 - Secondary1Buys`
- **Usage**: Written to C1 output record (Line 50, positions 116-130)
- **Format**: Formatted as 'Z,12V2-' (15 characters: 12 integer digits, 2 decimal places, negative sign)
- **Business Logic**: Negative value represents cash outflow for loan purchases

### TrustAccount
- **Level**: Global
- **Type**: String
- **Purpose**: Trust account identifier for the plan position
- **Declared**: Line 44
- **Assignments**: Line 44
- **Source**: Extracted from poppobj_de(01510) - Position Object Data Element 1510
- **Usage**: Written to C1 output record (Line 47, positions 40-71)
- **Length**: 32 characters (based on OcText_Set positions)

## Routine-Scoped Variables

### WK001 (CHECK.SSSA routine)
- **Level**: Routine-local
- **Type**: Numeric
- **Purpose**: Accumulator for netting BUY and SELL activity from SSSA
- **Declared**: Implicitly in CHECK.SSSA routine (Line 61)
- **Initial Value**: 0
- **Mutations**:
  - Incremented by BUY amounts: `WK001 = WK001 + sssaobj_numde(235)` (Line 65)
  - Decremented by SELL amounts: `WK001 = WK001 - sssaobj_numde(235)` (Line 68)
- **Usage**: Aggregates net loan activity, accounting for reversals
- **Final Value**: Stored back to global Secondary1Buys variable (Line 73)
- **Business Logic**: Nets BUYS and SELLS to correctly calculate cash impact

## Database Object Fields Referenced

### Position Object (poppobj) Fields
- **DE 030**: RKPlan - Plan identifier
- **DE 008**: TradeDate - Trade date
- **DE 741**: Secondary1Buys - Initial value from UDF position field
- **DE 877**: PriorCashApplied - UDF1 field for idempotency tracking
- **DE 01510**: TrustAccount - Trust account identifier

### Secondary Settlement Activity Object (sssaobj) Fields
- **DE 011**: Transaction type indicator ('XI' = loan activity)
- **DE 009**: Buy/Sell indicator ('B' = Buy, 'S' = Sell)
- **DE 235**: Transaction amount (dollars)

## File I/O Variables

### OcFile1 (Output File Handle)
- **Associated Variable**: FileName
- **Mode**: OUTPUT
- **Purpose**: Writes C1 activity records for cash reconciliation
- **Record Format**: Fixed-format positional layout
  - Positions 1-4: 'C100' (record type)
  - Positions 5-10: RKPlan (6 chars)
  - Positions 31-38: LastBusiness date (8 digits YYYYMMDD)
  - Positions 40-71: TrustAccount (32 chars)
  - Positions 73-92: '000000000000000    2' (20 chars - position indicator)
  - Position 115: '0' (1 char)
  - Positions 116-130: NewLoanUnits (15 chars formatted)
  - Positions 134-138: '00339' (5 chars - transaction code)

## Variable Mutations Summary

**High Mutation Variables** (modified in multiple locations):
- **Secondary1Buys**: Modified at Lines 35 (initial read), 73 (after SSSA processing)
- **SevenDaysAgo**: Modified at Lines 23, 26 (conditional initialization)
- **LastBusiness**: Modified at Lines 24, 27 (conditional initialization)
- **RKPlan**: Modified at Lines 33, 41 (read from different position records)
- **TradeDate**: Modified at Lines 34, 42 (read from different position records)

**Single Mutation Variables** (set once):
- sd080, FileName, RunDate, PriorCashApplied, NewLoanUnits, TrustAccount

**Critical Mutation Pattern**:
The program uses a defensive pattern with SevenDaysAgo and LastBusiness, initializing them twice based on RunDate validity to ensure the program always has valid date ranges even if environment configuration fails.

---

## Notes

1. **Date Handling**: Program uses both YYYYMMDD numeric format and OMNISCRIPT date functions for calculations
2. **Idempotency**: PriorCashApplied check prevents duplicate C1 record generation
3. **Error Handling**: Fallback to current date if $RUN-DATE environment variable invalid
4. **Position 92 Update**: Comments reference GPD-1704 - position 92 changed from value 1 to value 2 (Line 49)
5. **Reversal Handling**: CHECK.SSSA routine added 09/25/2024 to correctly net BUY and SELL activity

**Review Status**: Generated by automated parser - requires expert validation of business logic interpretations
