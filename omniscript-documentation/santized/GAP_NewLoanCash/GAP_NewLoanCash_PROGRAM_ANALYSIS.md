# OMNISCRIPT Program Analysis - GAP_NewLoanCash - January 28, 2026

## Program Identification

### Program Metadata
- **Program ID**: GAP_NewLoanCash
- **Author**: Gary Matten
- **Date Written**: December 21, 2023
- **Date Modified**: September 25, 2024 (Last known modification)
- **Source File**: /Users/dani.beckley/LocalDocuments/2026/bokf-poc/temp-repos/santized/GAP_NewLoanCash.cbl
- **OMNISCRIPT Version**: Unknown (analyzing syntax for compatibility)
- **Runtime Environment**: Requires OMNISCRIPT interpreter with file I/O and database object access capabilities

### Program Purpose
This script reads Plan Position Accounts for the last 7 calendar days, finding POOLLOAN3 position records and Secondary1Buys loaded from TRUSTTRANS.P1. It builds the necessary C1 activity for the right side (AC) of the cash reconciliation. Each record found is checked to see if UDF1 in POPP877 matches the expected amount, indicating whether the record has already been updated.

### Modification History
- **12/21/2023**: Initial creation by Gary Matten
- **06/27/2024**: GPD-1704 - Re-correcting position 92 to be a value of 2 instead of 1
- **09/25/2024**: Recognize Loan Reversal Activity and Net Activity Correctly when there are BUYS and SELLS

## Program Structure Analysis

### Size Metrics
- **Total Lines**: 59 (including comments and blank lines)
- **Lines of Code**: Approximately 45 executable OMNISCRIPT statements
- **Number of Procedures**: 1 main procedure + 1 subroutine (CHECK.SSSA)
- **Number of Modules**: Single module structure

### Module Breakdown
- **Main Processing Module**: Lines 10-50
  - Variable declarations: Line 10
  - File operations: Lines 11-14
  - Date calculations: Lines 15-25
  - Database view processing: Lines 27-48
- **CHECK.SSSA Subroutine**: Lines 50-59
  - Reversal activity checking: Lines 51-59

## Data Analysis

### Variable Structure
- **Total Variables**: 11 explicitly declared variables
- **Data Structures**: None (flat variable structure)
- **Arrays**: None
- **Constants**: 1 (sd080 = 99999999)
- **Dynamic Variables**: 1 (WK001 used in CHECK.SSSA routine)

### Variable Declarations
From OcLVar_Define statement:
1. **x.FileName** - Output file name (text)
2. **n.SevenDaysAgo** - Date 7 days prior (numeric)
3. **x.RKPlan** - Plan identifier (text)
4. **n.TradeDate** - Trade date (numeric)
5. **n.NewLoanUnits** - Calculated new loan units (numeric)
6. **n.PriorCashApplied** - Previously applied cash amount (numeric)
7. **x.Line** - Output line buffer (text)
8. **x.TrustAccount** - Trust account identifier (text)
9. **n.LastBusiness** - Last business day (numeric)
10. **n.Secondary1Buys** - Secondary1 buy amount (numeric)
11. **n.Secondary1Sells** - Secondary1 sell amount (numeric)
12. **n.RunDate** - Run date from environment (numeric)

Additional working variables:
- **WK001** - Working total for reversal calculations (numeric)

### File Operations
- **Output File**: OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.[YYYYMMDD].[HHMMSS].DAT
  - Created using environment variable $XDAT
  - File opened with OUTPUT mode (Line 14)
  - Records written using OcFile1_Write() (Line 47)
  - File organization: Sequential output

### Database Access
- **POPPOBJ (Position Object)**: Plan Position Accounts database
  - View parameters: securityid='POOLLOAN3', datelo:SevenDaysAgo, datehi:LastBusiness
  - Fields accessed:
    - DE 030: RKPlan
    - DE 008: TradeDate
    - DE 741: Secondary1Buys
    - DE 877: PriorCashApplied (UDF1)
    - DE 01510: TrustAccount
  - Update operation: poppobj_setde(877) and poppobj_update()

- **SSSAOBJ (Security Activity Object)**: Activity records database
  - View parameters: PLAN, SECURITYID='POOLLOAN3', DATE
  - Fields accessed:
    - DE 011: Activity type code
    - DE 009: Buy/Sell indicator ('B' or 'S')
    - DE 235: Activity amount

### External Dependencies
- **Environment Variables**:
  - $XDAT: Data directory path
  - $RUN-DATE: Run date for processing
- **OmniScript Functions Used**:
  - OcText_string, OcText_getenv, OcText_Set, OcText_tonum
  - OcDate_Current, OcDate_Valid, OcDate_AddDays, OcDate_AddBusDays
  - OcTime_Current
  - OcFMT
  - OcShow
  - OcFile1_Open, OcFile1_Write
  - poppobj_view, poppobj_next, poppobj_de, poppobj_numde, poppobj_setde, poppobj_update
  - sssaobj_view, sssaobj_next, sssaobj_de, sssaobj_numde

## Procedure Analysis

### Program Flow Structure
1. **Initialization (Lines 10-25)**:
   - Set constant sd080 = 99999999
   - Declare variables
   - Construct output filename with timestamp
   - Open output file
   - Calculate date ranges (SevenDaysAgo, LastBusiness, RunDate)

2. **Main Processing Loop (Lines 27-48)**:
   - View POPPOBJ records for POOLLOAN3 security
   - For each position record:
     - Extract plan, trade date, secondary buys, prior cash applied, trust account
     - If Secondary1Buys ≠ 0, call CHECK.SSSA to check for reversals
     - If PriorCashApplied ≠ Secondary1Buys and Secondary1Buys ≠ 0:
       - Calculate NewLoanUnits = 0 - Secondary1Buys
       - Build C1 activity record (C100 format)
       - Write record to output file
       - Update POPPOBJ DE 877 with Secondary1Buys value

3. **CHECK.SSSA Subroutine (Lines 50-59)**:
   - Validates RKPlan and TradeDate are present
   - Views SSSAOBJ for specific plan/security/date
   - Calculates net buy/sell activity:
     - Add amounts for 'B' (Buy) activity with type 'XI'
     - Subtract amounts for 'S' (Sell) activity with type 'XI' (reversals)
   - Updates Secondary1Buys with net amount (WK001)

### Call Relationships
- **Entry Point**: Line 10 (main program start)
- **Call Depth**: 2 levels maximum
  - Main program → CHECK.SSSA subroutine
- **Subroutines**:
  - CHECK.SSSA: Called from main loop (Line 33) to handle reversal activity
- **Database Iterations**:
  - poppobj_view loop (Lines 27-48)
  - sssaobj_view loop (Lines 53-58)

### Conditional Logic
- **IF Statements**: 6 major conditionals
  1. Line 16: OcDate_Valid(RunDate) - Date validation
  2. Line 32: Secondary1Buys ≠ 0 - Check for reversal processing
  3. Line 35: PriorCashApplied ≠ Secondary1Buys AND Secondary1Buys ≠ 0 - Process new records
  4. Line 51: (RKPlan ≠ '') AND (TradeDate ≠ 0) - Validate subroutine parameters
  5. Line 54: sssaobj_de(011) = 'XI' - Activity type check
  6. Lines 55-58: Buy/Sell indicator checks ('B' or 'S')

- **LOOP Structures**: 2
  1. poppobj_next() loop - Main position record processing
  2. sssaobj_next() loop - Reversal activity processing

- **ROUTINE/GOBACK**: 1 subroutine with GOBACK return

## Static Analysis Data

### Variable Cross-Reference
Since full static analysis tools are not available, manual cross-reference from code inspection:

**High-Activity Variables**:
- **Secondary1Buys**: Read (Line 31, 32, 35, 36, 48), Modified (Line 31, 59)
- **RKPlan**: Read/Written (Lines 28, 36, 51), Accessed from database
- **TradeDate**: Read/Written (Lines 29, 51), Accessed from database
- **Line**: Written (Lines 39-45), Read (Line 47) - Output buffer construction

**Read-Only Variables**:
- **SevenDaysAgo**: Set once (Lines 17/19), read once (Line 27)
- **LastBusiness**: Set once (Lines 18/20), read once (Line 27)
- **RunDate**: Set once (Line 15), read once (Line 16)

**Working Variables**:
- **WK001**: Local to CHECK.SSSA, accumulates net buy/sell amounts

### Call Graph Data
```
MAIN-PROGRAM
├── Initialization
│   ├── OcLVar_Define(variables)
│   ├── FileName construction
│   ├── OcFile1_Open(FileName)
│   ├── Date calculations (RunDate, SevenDaysAgo, LastBusiness)
│   └── OcShow(dates)
├── Main Loop: poppobj_view()
│   ├── Extract position data (RKPlan, TradeDate, Secondary1Buys, etc.)
│   ├── IF Secondary1Buys ≠ 0
│   │   └── PERFORM 'CHECK.SSSA'
│   │       └── sssaobj_view() loop
│   │           └── Calculate net buy/sell (WK001)
│   └── IF conditions met
│       ├── Calculate NewLoanUnits
│       ├── Build output Line
│       ├── OcFile1_Write(Line)
│       ├── poppobj_setde(877, Secondary1Buys)
│       └── poppobj_update()
└── [Implicit file close]

ROUTINE 'CHECK.SSSA'
├── IF (RKPlan ≠ '') AND (TradeDate ≠ 0)
│   ├── WK001 = 0
│   ├── sssaobj_view(PLAN, SECURITYID, DATE)
│   └── LOOP: sssaobj_next()
│       ├── IF sssaobj_de(011) = 'XI'
│       │   ├── IF sssaobj_de(009) = 'B': WK001 += amount
│       │   └── IF sssaobj_de(009) = 'S': WK001 -= amount (reversal)
│       └── Secondary1Buys = WK001
└── GOBACK
```

### Complexity Indicators
- **Cyclomatic Complexity**: Low-Medium (estimated ~8)
  - 6 IF statements
  - 2 LOOP structures
  - 1 subroutine call
- **Largest Procedure**: Main program (~40 executable lines)
- **Most Complex Procedure**: CHECK.SSSA (nested conditions within loop)
- **Database Operations**: 2 view/loop operations with record updates
- **String Operations**: Moderate (filename construction, output line formatting with OcText_Set)

## Chunking Strategy

### Recommended Approach
**Single Unit Documentation** - This program is small enough (59 lines, 2 procedures) to document as a single unit without chunking.

### Documentation Sequence
1. **Data Dictionary**: Document all 11 variables plus WK001, database field mappings
2. **Main Procedure**: Document main loop and C1 record generation logic
3. **CHECK.SSSA Subroutine**: Document reversal activity calculation
4. **Call Graph**: Simple two-level hierarchy
5. **Variable Mutations**: Focus on Secondary1Buys modification flow
6. **Comprehensive Documentation**: Synthesize with business context

## Risk and Error Handling Assessment

### Current Error Handling
- **Date Validation**: IF OcDate_Valid(RunDate) checks date validity (Line 16)
- **Parameter Validation in CHECK.SSSA**: Checks RKPlan and TradeDate before database access (Line 51)
- **Conditional Processing**: Checks Secondary1Buys ≠ 0 before processing (Lines 32, 35)

### Missing Error Handling
- **File Open Failure**: No check for OcFile1_Open success
- **Database View Failures**: No error handling for poppobj_view or sssaobj_view
- **Database Update Failures**: No verification of poppobj_update() success
- **File Write Failures**: No check for OcFile1_Write success
- **Environment Variable Missing**: No validation that $XDAT exists

### Risk Assessment
- **Medium Risk**: Database operations without error checking
- **Medium Risk**: File operations without failure detection
- **Low Risk**: Date calculations have validation
- **Low Risk**: Parameter validation in subroutine

## Business Logic Summary

### Core Business Function
This program performs **cash reconciliation for new loan activities** by:
1. Identifying POOLLOAN3 position records from the last 7 days
2. Checking for reversal activity (buys/sells) in SSSA records
3. Determining net loan amounts after reversals
4. Generating C1 activity records for cash reconciliation
5. Marking processed records (UDF1) to prevent duplicate processing

### Key Business Rules
1. Only process records where Secondary1Buys ≠ 0
2. Skip records already processed (PriorCashApplied = Secondary1Buys)
3. Net buy/sell activity: Buys add, Sells subtract (reversals)
4. Only process 'XI' activity type in SSSA
5. NewLoanUnits is negative of Secondary1Buys for C1 record
6. C1 record format: Position 92 = '2' (corrected in GPD-1704)

### Integration Points
- **Input**: POPPOBJ (Plan Position Accounts) for POOLLOAN3 security
- **Input**: SSSAOBJ (Security Activity) for reversal checks
- **Output**: C1 activity file for cash reconciliation system
- **Update**: POPPOBJ DE 877 (UDF1) to track processed records

## Next Steps

### Phase 2: Data Dictionary
Create comprehensive data dictionary for all variables, including:
- Variable purposes and business meanings
- Data type specifications
- Usage patterns from cross-reference
- Database field mappings (POPPOBJ, SSSAOBJ data elements)

### Phase 3: Procedure Documentation
Document:
1. Main processing loop and C1 record generation
2. CHECK.SSSA subroutine and reversal logic
3. Output file format (C1 record structure)

### Phase 4: Call Graph
Create visual representation of:
- Main program → CHECK.SSSA relationship
- Database view loops
- Conditional execution paths

### Phase 5: Variable Mutations
Track mutations of:
- **Secondary1Buys**: Modified in CHECK.SSSA, read in main loop
- **Line**: Constructed incrementally with OcText_Set calls
- **PriorCashApplied**: Database field comparison for duplicate detection

### Phase 6: Comprehensive Documentation
Synthesize all documentation with:
- Executive summary of cash reconciliation process
- Business context and integration with reconciliation system
- Error handling recommendations
- Maintenance considerations
