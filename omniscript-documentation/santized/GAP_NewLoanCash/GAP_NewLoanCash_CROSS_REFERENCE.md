# GAP_NewLoanCash Cross Reference

## Program: GAP_NewLoanCash
**Purpose**: Comprehensive cross-reference for all program elements  
**Last Updated**: 2026-02-03

---

## Variable Cross Reference

### sd080
- **Type**: Numeric (Error code)
- **Declared**: Line 13
- **Assigned**: Line 13
- **Referenced**: None (set for OMNISCRIPT context)
- **Purpose**: Error context indicator
- **Scope**: Global

---

### FileName
- **Type**: String
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**: Line 16 (constructed from environment)
- **Referenced**: 
  - Line 18 (OcShow for logging)
  - Line 19 (OcFile1_Open)
- **Purpose**: Output file path
- **Scope**: Global

---

### RunDate
- **Type**: Numeric (Date YYYYMMDD)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**: Line 21 (from environment)
- **Referenced**:
  - Line 22 (OcDate_Valid validation)
  - Line 23 (SevenDaysAgo calculation - true branch)
  - Line 24 (LastBusiness calculation - true branch)
  - Line 29 (OcShow for logging - implied)
- **Purpose**: Batch processing date
- **Scope**: Global

---

### SevenDaysAgo
- **Type**: Numeric (Date YYYYMMDD)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**:
  - Line 23 (true branch: RunDate - 7 days)
  - Line 26 (false branch: Current - 7 days)
- **Referenced**:
  - Line 29 (OcShow for logging)
  - Line 31 (poppobj_view datelo parameter)
- **Purpose**: Start date for position query
- **Scope**: Global

---

### LastBusiness
- **Type**: Numeric (Date YYYYMMDD)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**:
  - Line 24 (true branch: RunDate - 1 business day)
  - Line 27 (false branch: Current - 1 business day)
- **Referenced**:
  - Line 29 (OcShow for logging)
  - Line 31 (poppobj_view datehi parameter)
  - Line 47 (C1 record effective date)
- **Purpose**: End date for position query and C1 effective date
- **Scope**: Global

---

### RKPlan
- **Type**: String (Plan ID, 6 chars)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**:
  - Line 33 (poppobj_de(030) - first read)
  - Line 41 (poppobj_de(030) - second read in conditional)
- **Referenced**:
  - Line 46 (C1 record construction)
  - Line 60 (CHECK.SSSA validation - passed via global)
  - Line 62 (sssaobj_view PLAN parameter - in CHECK.SSSA)
- **Purpose**: Retirement plan identifier
- **Scope**: Global
- **Mutated**: 2 locations

---

### TradeDate
- **Type**: Numeric (Date YYYYMMDD)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**:
  - Line 34 (poppobj_numde(008) - first read)
  - Line 42 (poppobj_numde(008) - second read in conditional)
- **Referenced**:
  - Line 60 (CHECK.SSSA validation - passed via global)
  - Line 62 (sssaobj_view DATE parameter - in CHECK.SSSA)
- **Purpose**: Transaction trade date
- **Scope**: Global
- **Mutated**: 2 locations

---

### Secondary1Buys
- **Type**: Numeric (Dollar amount)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**:
  - Line 35 (poppobj_numde(741) - initial read)
  - Line 73 (WK001 assignment - after reversal check)
- **Referenced**:
  - Line 37 (IF condition: <> 0 check)
  - Line 40 (IF condition: <> PriorCashApplied)
  - Line 43 (Negation for NewLoanUnits)
  - Line 54 (poppobj_setde value parameter)
- **Purpose**: Loan purchase amount (may be netted)
- **Scope**: Global
- **Mutated**: 6 locations (HIGH MUTATION)

---

### PriorCashApplied
- **Type**: Numeric (Dollar amount)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**: Line 36 (poppobj_numde(877) - read from UDF1)
- **Referenced**: Line 40 (IF condition: <> Secondary1Buys)
- **Purpose**: Idempotency check value
- **Scope**: Global

---

### NewLoanUnits
- **Type**: Numeric (Dollar amount, negative)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**: Line 43 (0 - Secondary1Buys)
- **Referenced**: Line 50 (C1 record amount field)
- **Purpose**: Negated loan amount for C1 record
- **Scope**: Global

---

### TrustAccount
- **Type**: String (Account ID, 32 chars)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**: Line 44 (poppobj_de(01510))
- **Referenced**: Line 48 (C1 record trust account field)
- **Purpose**: Trust account identifier
- **Scope**: Global

---

### Line
- **Type**: String (Fixed-length record buffer)
- **Declared**: Line 14 (via OcLVar_Define)
- **Assigned**: Lines 45-52 (OcText_Set operations)
- **Referenced**: Line 53 (OcFile1_Write)
- **Purpose**: C1 record construction buffer
- **Scope**: Global

---

### WK001 (Local to CHECK.SSSA)
- **Type**: Numeric (Work variable)
- **Declared**: Implicitly at first use (Line 61)
- **Assigned**:
  - Line 61 (initialized to 0)
  - Line 65 (accumulated: WK001 + Amount for Buys)
  - Line 68 (accumulated: WK001 - Amount for Sells)
- **Referenced**:
  - Line 65 (accumulation expression)
  - Line 68 (accumulation expression)
  - Line 73 (assigned to Secondary1Buys)
- **Purpose**: Accumulator for net loan activity
- **Scope**: Local to CHECK.SSSA routine
- **Mutated**: 3 locations

---

## Routine Cross Reference

### Main Program
- **Lines**: 13-57
- **Calls**: CHECK.SSSA (via PERFORM at line 38)
- **Called By**: Entry point (not called)
- **Database Operations**:
  - poppobj_view (Line 31)
  - poppobj_next (Line 32)
  - poppobj_de (Lines 33, 41, 44)
  - poppobj_numde (Lines 34, 35, 36, 42)
  - poppobj_setde (Line 54)
  - poppobj_update (Line 55)
- **File Operations**:
  - OcFile1_Open (Line 19)
  - OcFile1_Write (Line 53)
- **Global Variables Used**: All

---

### CHECK.SSSA
- **Lines**: 59-75
- **Calls**: None
- **Called By**: Main program (Line 38)
- **Database Operations**:
  - sssaobj_view (Line 62)
  - sssaobj_next (Line 63)
  - sssaobj_de (Lines 64, 67)
  - sssaobj_numde (Lines 65, 68)
- **Local Variables**: WK001
- **Global Variables Used**: RKPlan, TradeDate, Secondary1Buys
- **Global Variables Modified**: Secondary1Buys (Line 73)

---

## Database Field Cross Reference

### POPP Table (poppobj)

#### Field 008 (TradeDate)
- **Read**: Lines 34, 42
- **Write**: None
- **Used In**: Date filtering, reversal check

#### Field 030 (RKPlan)
- **Read**: Lines 33, 41
- **Write**: None
- **Used In**: C1 record, reversal check

#### Field 741 (Secondary1Buys)
- **Read**: Line 35
- **Write**: None
- **Used In**: Amount calculations, C1 record

#### Field 877 (PriorCashApplied / UDF1)
- **Read**: Line 36
- **Write**: Line 54
- **Used In**: Idempotency check, marking processed

#### Field 1510 (TrustAccount)
- **Read**: Line 44
- **Write**: None
- **Used In**: C1 record

---

### SSSA Table (sssaobj)

#### Field 009 (Transaction Type)
- **Read**: Lines 65, 67
- **Write**: None
- **Values**: 'B' (Buy), 'S' (Sell)
- **Used In**: Reversal netting logic

#### Field 011 (Transaction Source)
- **Read**: Line 64
- **Write**: None
- **Values**: 'XI' (filter value)
- **Used In**: Transaction filtering

#### Field 235 (Transaction Amount)
- **Read**: Lines 65, 68
- **Write**: None
- **Used In**: Reversal amount accumulation

---

## Built-In Function Cross Reference

### Date/Time Functions

#### OcDate_Current()
- **Usage**: Lines 17, 26, 27
- **Purpose**: Get current system date
- **Returns**: Numeric date (YYYYMMDD)

#### OcTime_Current()
- **Usage**: Line 17
- **Purpose**: Get current system time
- **Returns**: Numeric time (HHMMSS)

#### OcDate_Valid()
- **Usage**: Line 22
- **Purpose**: Validate date format
- **Returns**: Boolean

#### OcDate_AddDays()
- **Usage**: Lines 23, 26
- **Purpose**: Add calendar days to date
- **Returns**: Numeric date

#### OcDate_AddBusDays()
- **Usage**: Lines 24, 27
- **Purpose**: Add business days to date
- **Returns**: Numeric date

---

### String Functions

#### OcText_string()
- **Usage**: Line 16
- **Purpose**: Concatenate strings
- **Returns**: String

#### OCTEXT_GETENV()
- **Usage**: Lines 16, 21
- **Purpose**: Get environment variable
- **Returns**: String

#### octext_tonum()
- **Usage**: Line 21
- **Purpose**: Convert string to numeric
- **Returns**: Numeric

#### OcText_Set()
- **Usage**: Lines 45-52 (8 calls)
- **Purpose**: Set substring in fixed-length record
- **Returns**: None (modifies variable)

---

### Formatting Functions

#### OCFMT() / OcFmt()
- **Usage**: Lines 17, 47, 50
- **Purpose**: Format numeric values
- **Formats Used**:
  - 'Z8': Zero-filled 8 digits
  - 'Z6': Zero-filled 6 digits
  - 'Z,12V2-': 12 digits, 2 decimals, signed
- **Returns**: Formatted string

---

### File I/O Functions

#### OcFile1_Open()
- **Usage**: Line 19
- **Purpose**: Open file for output
- **Parameters**: name (FileName), mode ('OUTPUT')
- **Returns**: Status (not checked)

#### OcFile1_Write()
- **Usage**: Line 53
- **Purpose**: Write record to file
- **Parameters**: record (Line)
- **Returns**: Status (not checked)

---

### Display Functions

#### OcShow()
- **Usage**: Lines 18, 29
- **Purpose**: Display values (logging/debugging)
- **Parameters**: Variable-length arguments
- **Returns**: None

---

## Control Flow Cross Reference

### Conditional Statements

#### IF OcDate_Valid(RunDate) (Line 22)
- **True Branch**: Lines 23-24 (use RunDate)
- **False Branch**: Lines 26-27 (use current date)
- **End**: Line 28

#### IF (Secondary1Buys <> 0) (Line 37)
- **True Branch**: Line 38 (PERFORM CHECK.SSSA)
- **False Branch**: None (continue)
- **End**: Line 39

#### IF (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0) (Line 40)
- **True Branch**: Lines 41-55 (process record)
- **False Branch**: None (skip record)
- **End**: Line 56

#### IF (RKPlan <> '') and (TradeDate <> 0) (Line 60)
- **True Branch**: Lines 61-72 (reversal check)
- **False Branch**: None (early exit)
- **End**: Line 74

#### IF sssaobj_de(011) = 'XI' (Line 64)
- **True Branch**: Lines 65-71 (process transaction)
- **False Branch**: None (skip transaction)
- **End**: Line 71

#### IF sssaobj_de(009) = 'B' (Line 65)
- **True Branch**: Line 65 (accumulate buy)
- **False Branch**: None (check next condition)
- **End**: Line 66

#### IF sssaobj_de(009) = 'S' (Line 67)
- **True Branch**: Line 68 (subtract sell)
- **False Branch**: None (continue)
- **End**: Line 70

---

### Loop Statements

#### LOOP while poppobj_next() (Lines 32-57)
- **Entry**: Line 32
- **Body**: Lines 33-56
- **Exit**: Line 57 (ENDLOOP)
- **Iteration**: Next position record

#### LOOP while sssaobj_next() (Lines 63-72)
- **Entry**: Line 63
- **Body**: Lines 64-71
- **Exit**: Line 72 (ENDLOOP)
- **Iteration**: Next activity record

---

## Line Number Index

### Program Structure

| Line Range | Section | Description |
|------------|---------|-------------|
| 1-11 | Header | Comments and documentation |
| 13 | Initialization | Set error context |
| 14 | Declaration | Define local variables |
| 16-19 | Initialization | Build filename and open file |
| 21-29 | Date Setup | Calculate date range |
| 31-57 | Main Loop | Process position records |
| 33-36 | Data Extraction | Read position fields |
| 37-39 | Reversal Check | Conditional PERFORM |
| 40-56 | Record Processing | Build and write C1 record |
| 41-44 | Data Extraction | Re-read position fields |
| 45-52 | Record Construction | Build C1 fields |
| 53 | File Output | Write record |
| 54-55 | Database Update | Update field 877 |
| 59-75 | CHECK.SSSA | Reversal detection routine |
| 60-74 | Reversal Logic | Query and net activity |
| 61 | Initialization | Set WK001 = 0 |
| 62-72 | Activity Loop | Process SSSA records |
| 64-71 | Transaction Processing | Accumulate buys/sells |
| 73 | Result Assignment | Update Secondary1Buys |
| 75 | Exit | GOBACK |

---

## Documentation Cross Reference

| Topic | Primary Document | Related Documents |
|-------|------------------|-------------------|
| Program Overview | OVERVIEW.md | All documents |
| Variables | DATA_DICTIONARY.md | OVERVIEW.md, CROSS_REFERENCE.md |
| Routines | CALL_GRAPH.md | OVERVIEW.md, procedures/ |
| Business Logic | BUSINESS_RULES.md | OVERVIEW.md |
| Error Handling | ERROR_HANDLING.md | OVERVIEW.md |
| Integration | INTEGRATION_GUIDE.md | OVERVIEW.md, ERROR_HANDLING.md |
| Visual Flows | DIAGRAMS.md | CALL_GRAPH.md |
| Cross References | CROSS_REFERENCE.md | All documents |
| Validation | VALIDATION_REPORT.md | All documents |
| CHECK.SSSA Routine | procedures/CHECK_SSSA.md | CALL_GRAPH.md, OVERVIEW.md |

---

## External Reference Index

### Environment Variables
- **$XDAT**: Lines 16 (directory path)
- **$RUN-DATE**: Line 21 (processing date)

### Database Tables
- **POPP**: Lines 31-55 (position records)
- **SSSA**: Lines 62-72 (activity records)

### Output Files
- **C1 Activity File**: Lines 16-19, 53 (creation and writing)

---

## Change Impact Matrix

| Change Type | Lines Affected | Documents to Update | Related Elements |
|-------------|----------------|---------------------|------------------|
| Date range window | 23, 26 | BUSINESS_RULES, OVERVIEW | BR-002, SevenDaysAgo |
| Idempotency field | 36, 54 | DATA_DICTIONARY, BUSINESS_RULES | Field 877, BR-003 |
| C1 record format | 45-52 | INTEGRATION_GUIDE, BUSINESS_RULES | BR-007, Line variable |
| Reversal logic | 60-72 | BUSINESS_RULES, procedures/CHECK_SSSA | BR-005, WK001 |
| Activity code | 52 | INTEGRATION_GUIDE, BUSINESS_RULES | BR-007, '00339' |

---

*This cross-reference provides comprehensive traceability for all program elements, facilitating maintenance and impact analysis.*

*Last Updated*: 2026-02-03
