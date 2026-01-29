# GAP_NewLoanCash Comprehensive Documentation

**Program Name**: GAP_NewLoanCash  
**Language**: OmniScript  
**Location**: temp-repos/santized/GAP_NewLoanCash.cbl  
**Lines of Code**: 78  
**Last Updated**: 09/25/2024  
**Author**: Gary Matten

---

## Executive Summary

### Program Purpose
The GAP_NewLoanCash script generates C1 cash reconciliation activity records for loan pool securities (POOLLOAN3) by querying position accounts from the last 7 calendar days, checking for reversals in the secondary settlement system, and creating offset entries for new loan purchases. This ensures the right side (AC) of cash reconciliation accurately reflects net loan activity after accounting for trade reversals.

### Business Value
- **Cash Reconciliation Accuracy**: Ensures trust account cash positions accurately reflect loan purchase activity
- **Reversal Handling**: Prevents over-reporting by netting out reversed trades
- **Idempotency**: Prevents duplicate C1 activity records through intelligent tracking
- **Audit Trail**: Creates standardized C1 records for downstream reconciliation processes

### Key Business Rules
1. **7-Day Lookback Window**: Process positions from the last 7 calendar days
2. **POOLLOAN3 Only**: Filter for loan pool securities exclusively
3. **Net After Reversals**: Check SSSA for reversals before generating C1 activity
4. **Idempotency Control**: Track processed amounts in POPP DE 877 to prevent duplicates
5. **Right-Side Activity**: Position indicator '2' for asset (AC) side of reconciliation
6. **Prior Business Day Dating**: C1 records dated to LastBusiness, not trade date
7. **Transaction Code 00339**: Standard code for new loan cash offset activity

### Critical Risks
🔴 **HIGH RISK**: No error handling for file operations or database updates  
🔴 **HIGH RISK**: File write and DB update not atomic - risk of duplicate records  
🟡 **MEDIUM RISK**: Environment variable $XDAT not validated  
🟡 **MEDIUM RISK**: SSSA query failure could incorrectly zero Secondary1Buys

---

## 1. Program Architecture

### Program Type
**OmniScript Batch Processor** - Scheduled daily job for cash reconciliation

### Processing Model
- **Input**: POPP position database (last 7 days)
- **Processing**: Query, validate, adjust for reversals, generate C1 records
- **Output**: Timestamped C1 activity file + POPP database updates

### Component Structure
```
GAP_NewLoanCash
├── Initialization Section (Lines 11-26)
│   ├── Environment variable reads
│   ├── Date calculations
│   └── Output file setup
│
├── Main Processing Loop (Lines 28-52)
│   ├── POPP position query
│   ├── Position data extraction
│   ├── SSSA reversal check (conditional)
│   ├── Idempotency validation
│   ├── C1 record generation
│   └── POPP update
│
└── CHECK.SSSA Subroutine (Lines 54-70)
    ├── Input validation
    ├── SSSA activity query
    ├── Net amount calculation (buys - sells)
    └── Secondary1Buys update
```

### Technology Stack
- **Language**: OmniScript (proprietary scripting language)
- **Runtime**: OmniScript interpreter/runtime environment
- **Databases**: POPP (Plan Position), SSSA (Settlement Activity)
- **File System**: Windows/Unix file system via OmniScript API

---

## 2. Detailed Functional Description

### Initialization Phase (Lines 11-26)

**Purpose**: Establish processing environment and date ranges

**Key Operations**:
1. **System Default**: Set `sd080 = 99999999` (runtime configuration parameter)
2. **Variable Declaration**: Declare all working variables using `OcLVar_Define()`
3. **Output File Construction**:
   ```
   Format: $XDAT\OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.YYYYMMDD.HHMMSS.DAT
   Example: C:\DATA\OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.20231221.143022.DAT
   ```
4. **Date Range Calculation**:
   - Read `$RUN-DATE` from environment (or fallback to current date)
   - Calculate `SevenDaysAgo` = RunDate - 7 calendar days
   - Calculate `LastBusiness` = RunDate - 1 business day (skip weekends/holidays)

**Environment Dependencies**:
- **$XDAT**: Directory path for output files (REQUIRED, no validation)
- **$RUN-DATE**: Business run date in YYYYMMDD format (OPTIONAL, fallback to current date)

**Output**: Open output file ready for C1 activity records

---

### Main Processing Loop (Lines 28-52)

**Purpose**: Process POOLLOAN3 positions and generate C1 activity records

#### Step 1: Query Position Records
```omniscript
poppobj_view(securityid:'POOLLOAN3' datelo:SevenDaysAgo datehi:LastBusiness);
```
- **Query Criteria**: Security = POOLLOAN3, Trade Date between SevenDaysAgo and LastBusiness
- **Expected Records**: 100-500 positions (typical 7-day window)
- **Performance**: Single query, cursor-based iteration

#### Step 2: Extract Position Data
For each position record, read:
- **RKPlan** (DE 030): Plan identifier (6 chars)
- **TradeDate** (DE 008): Original trade date (YYYYMMDD)
- **Secondary1Buys** (DE 741): Secondary market buy amount
- **PriorCashApplied** (DE 877): Previously processed amount (idempotency flag)
- **TrustAccount** (DE 01510): Trust account number (32 chars)

#### Step 3: Check for Reversals (Conditional)
```omniscript
If (Secondary1Buys <> 0);
   PERFORM 'CHECK.SSSA';
End;
```
- **Trigger**: Only if Secondary1Buys is non-zero
- **Purpose**: Query SSSA to recalculate net amount after reversals
- **Effect**: May modify Secondary1Buys (see CHECK.SSSA section below)

#### Step 4: Idempotency Check
```omniscript
if (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0);
```
- **Condition 1**: Amounts differ (new activity or adjusted amount)
- **Condition 2**: Non-zero amount (skip zero-dollar positions)
- **Business Logic**: Only process if unprocessed AND has actual activity

#### Step 5: Generate C1 Activity Record
Build fixed-width record with:
- **Positions 1-4**: 'C100' (record type)
- **Positions 5-10**: RKPlan (plan ID)
- **Positions 31-38**: LastBusiness (effective date - YYYYMMDD)
- **Positions 40-71**: TrustAccount (32 chars)
- **Positions 73-92**: '000000000000000    2' (position indicator, byte 92='2' for right side AC)
- **Position 115**: '0' (flag)
- **Positions 116-130**: NewLoanUnits (amount, formatted Z,12V2-, 15 chars)
- **Positions 134-138**: '00339' (transaction code)

**Amount Calculation**:
```omniscript
NewLoanUnits = 0 - Secondary1Buys
```
- **Negation**: Loan purchases reduce cash (negative value)
- **Format**: Comma-separated with 2 decimals and trailing sign

#### Step 6: Write and Update
1. **Write to File**: `OcFile1_Write(Line)` - Append C1 record to output file
2. **Update POPP**: `poppobj_setde(denum:877 value:Secondary1Buys)` - Mark as processed
3. **Commit Update**: `poppobj_update()` - Persist to database

**⚠️ CRITICAL**: Steps 1 and 2-3 are NOT atomic - if file write succeeds but DB update fails, next run generates duplicate C1 activity

---

### CHECK.SSSA Subroutine (Lines 54-70)

**Purpose**: Recalculate Secondary1Buys by netting SSSA buy/sell activity

**Entry Condition**: Called only when Secondary1Buys ≠ 0

#### Algorithm
1. **Validate Inputs**: Skip if RKPlan empty or TradeDate zero
2. **Initialize Accumulator**: `WK001 = 0`
3. **Query SSSA**: 
   ```omniscript
   sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
   ```
4. **Process Each SSSA Record**:
   - If `DE011 = 'XI'` (transaction indicator filter):
     - If `DE009 = 'B'`: `WK001 = WK001 + Amount` (buy transaction)
     - If `DE009 = 'S'`: `WK001 = WK001 - Amount` (sell/reversal transaction)
5. **Update Global**: `Secondary1Buys = WK001` (net amount)
6. **Return**: `GOBACK` to main loop

#### Business Logic Examples

**Example 1: Partial Reversal**
- POPP: Secondary1Buys = $50,000
- SSSA: Buy $50,000, Sell $10,000
- Net: WK001 = 50000 - 10000 = $40,000
- Result: Secondary1Buys updated to $40,000, C1 generated for $40K

**Example 2: Full Reversal**
- POPP: Secondary1Buys = $50,000
- SSSA: Buy $50,000, Sell $50,000
- Net: WK001 = 50000 - 50000 = $0
- Result: Secondary1Buys = $0, main loop skips C1 generation (zero amount)

**Example 3: No SSSA Records**
- POPP: Secondary1Buys = $50,000
- SSSA: No matching records found
- Net: WK001 = 0
- **⚠️ RISK**: Secondary1Buys incorrectly set to $0 (should remain $50,000)
- **Root Cause**: No distinction between "no records" and "net zero"

---

## 3. Data Dictionary Summary

See [GAP_NewLoanCash_DATA_DICTIONARY.md](./GAP_NewLoanCash_DATA_DICTIONARY.md) for complete variable documentation.

### Critical Variables
| Variable | Purpose | Mutation Risk |
|----------|---------|---------------|
| **Secondary1Buys** | Net loan purchase amount | 🔴 HIGH - Modified by CHECK.SSSA |
| **PriorCashApplied** | Idempotency flag | 🟢 LOW - Read-only |
| **NewLoanUnits** | Negated amount for C1 | 🟢 LOW - Calculated once |
| **WK001** | SSSA accumulator | 🟡 MEDIUM - Complex accumulation |
| **Line** | C1 record buffer | 🟢 LOW - Incremental build |

---

## 4. Error Handling and Risk Analysis

See [GAP_NewLoanCash_ERROR_HANDLING.md](./GAP_NewLoanCash_ERROR_HANDLING.md) for detailed risk analysis.

### Critical Gaps
| Gap | Risk Level | Impact |
|-----|-----------|--------|
| No FILE STATUS handling | 🔴 HIGH | Silent file write failures |
| No DB error checking | 🔴 HIGH | Duplicate C1 records on update failure |
| Non-atomic file+DB operations | 🔴 HIGH | Data integrity violations |
| $XDAT not validated | 🟡 MEDIUM | Malformed file paths |
| SSSA query failure undetected | 🟡 MEDIUM | Incorrect amount calculations |
| No bounds checking | 🟡 MEDIUM | Arithmetic overflow possible |

### Recommended Immediate Actions
1. Add file operation error checking
2. Add database update error checking
3. Validate environment variables
4. Implement transaction logging
5. Add SSSA query validation

---

## 5. Performance Analysis

### Query Performance
| Query | Frequency | Typical Records | Performance |
|-------|-----------|----------------|-------------|
| POPP poppobj_view() | 1 per run | 100-500 | Good (indexed) |
| SSSA sssaobj_view() | 0-500 per run | 1-10 each | Moderate (nested) |

### Complexity Analysis
- **Time Complexity**: O(N × M) where N = POPP records, M = SSSA records per position
- **Typical**: 100 POPP × 5 SSSA = 500 operations (acceptable)
- **Worst Case**: 10,000 POPP × 100 SSSA = 1,000,000 operations (slow)

### Optimization Opportunities
1. **Batch SSSA Queries**: Reduce from O(N) to O(1) database calls
2. **Eliminate Redundant Reads**: Remove duplicate RKPlan/TradeDate reads (lines 39-40)
3. **Add Date Range Validation**: Prevent accidental large date ranges

---

## 6. Business Rules and Compliance

### Explicit Business Rules
1. **Transaction Code 00339**: Standard code for new loan cash offset
2. **Position Indicator '2'**: Right side (AC) cash reconciliation
3. **7-Day Lookback**: Process last 7 calendar days of positions
4. **POOLLOAN3 Filter**: Only process loan pool securities
5. **Net After Reversals**: Always check SSSA before generating C1

### Implicit Business Rules
1. **Negative Cash Flow**: Loan purchases are negative amounts (asset acquisition)
2. **Prior Business Day Dating**: C1 activity dated to LastBusiness, not trade date
3. **Idempotency**: Track processed amounts in POPP DE 877
4. **'XI' Indicator Filter**: Only process specific SSSA transaction types

### Compliance Considerations
- **Audit Trail**: C1 records provide audit trail for cash movements
- **Data Retention**: Output files should be retained per company policy
- **Reconciliation**: C1 records feed downstream cash reconciliation processes
- **Accuracy**: Critical for trust accounting and regulatory reporting

---

## 7. Integration and Dependencies

### External Systems
| System | Interface Type | Data Flow | Dependency Level |
|--------|---------------|-----------|------------------|
| **POPP Database** | Read/Write | Position data → C1 activity | CRITICAL |
| **SSSA Database** | Read-only | Settlement activity → Net amounts | HIGH |
| **File System** | Write | C1 records → Output file | HIGH |
| **Batch Scheduler** | Environment | Run date → Processing window | MEDIUM |

### Data Dependencies
- **POPP DE 030, 008, 741, 877, 01510**: Required fields
- **SSSA DE 009, 011, 235**: Required fields for net calculation
- **$XDAT Environment Variable**: Required for file path
- **$RUN-DATE Environment Variable**: Optional (fallback to current date)

### Downstream Consumers
- **Cash Reconciliation System**: Consumes C1 activity file
- **Trust Accounting**: Uses C1 records for trust account balancing
- **Audit Reports**: References C1 activity for compliance

---

## 8. Testing and Validation

### Standard Test Scenarios
1. **Normal Processing**: POPP records with no reversals
2. **Partial Reversal**: SSSA shows buy with partial sell
3. **Full Reversal**: SSSA shows buy with full sell (net zero)
4. **Already Processed**: PriorCashApplied matches Secondary1Buys
5. **Zero Amount**: Secondary1Buys = 0 (skip processing)
6. **No POPP Records**: Empty result set (valid or query failure?)

### Edge Cases
1. **Missing Environment Variables**: $XDAT or $RUN-DATE not set
2. **Invalid Date Range**: SevenDaysAgo > LastBusiness
3. **Negative Secondary1Buys**: Over-reversal scenario
4. **Extremely Large Amounts**: Test numeric overflow
5. **Concurrent Updates**: Multiple processes updating same POPP record
6. **File System Full**: Disk space exhausted during write

### Error Scenarios
1. **File Open Failure**: $XDAT invalid or permissions issue
2. **POPP Query Failure**: Database connection lost
3. **SSSA Query Failure**: Database error during CHECK.SSSA
4. **File Write Failure**: Disk full or I/O error
5. **POPP Update Failure**: Record lock or constraint violation

### Validation Checks
- Compare output file record count with expected POPP record count
- Verify all C1 records have transaction code '00339'
- Confirm all amounts are negative (loan purchases)
- Validate POPP DE 877 updated for all processed records
- Check for duplicate C1 records (idempotency validation)

---

## 9. Maintenance and Change History

### Change History
| Date | Author | Ticket | Description |
|------|--------|--------|-------------|
| 12/21/2023 | Gary Matten | - | Created OmniScript |
| 06/27/2024 | Gary Matten | GPD-1704 | Corrected position 92 to '2' instead of '1' |
| 09/25/2024 | Gary Matten | - | Enhanced reversal recognition for buys and sells |

### Known Technical Debt
1. **Redundant Reads**: RKPlan and TradeDate read twice (lines 30-31, 39-40)
2. **No Error Handling**: Missing error checks for all file and DB operations
3. **Non-Atomic Operations**: File write and DB update not transactional
4. **Unused Variable**: Secondary1Sells declared but never used
5. **No Logging**: No transaction logging or audit trail
6. **Hard-Coded Values**: Transaction code '00339' and position indicator '2' not configurable

### Maintenance Procedures
1. **After POPP Schema Change**: Update data element references if DE 030, 741, 877, or 01510 change
2. **After SSSA Schema Change**: Update data element references if DE 009, 011, or 235 change
3. **Adding New Transaction Codes**: Modify line 48 to support additional codes
4. **Changing Date Range**: Modify line 20/23 to adjust lookback window
5. **Error Handling Enhancements**: Add error checking per recommendations

---

## 10. Visual Documentation

See [GAP_NewLoanCash_MERMAID_DIAGRAMS.md](./GAP_NewLoanCash_MERMAID_DIAGRAMS.md) for:
- Program flow flowchart
- PERFORM hierarchy graph
- Data flow diagram
- File I/O sequence diagram
- Variable lifecycle state machines

---

## 11. Quick Reference

### File Locations
- **Source Code**: `temp-repos/santized/GAP_NewLoanCash.cbl`
- **Output Files**: `$XDAT\OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.YYYYMMDD.HHMMSS.DAT`
- **Documentation**: `cobol-documentation/santized/GAP_NewLoanCash/`

### Key Parameters
- **Security ID**: 'POOLLOAN3'
- **Date Range**: Last 7 calendar days
- **Transaction Code**: '00339'
- **Position Indicator**: '2' (byte 92)
- **Record Type**: 'C100'

### Database Objects
- **POPP**: Plan Position database (read/write)
  - DE 030: RKPlan
  - DE 008: TradeDate
  - DE 741: Secondary1Buys
  - DE 877: PriorCashApplied (User-defined field)
  - DE 01510: TrustAccount
- **SSSA**: Settlement Activity database (read-only)
  - DE 009: Transaction Type ('B' or 'S')
  - DE 011: Transaction Indicator ('XI')
  - DE 235: Amount

### Common Issues
1. **Empty Output File**: Check if POPP query returned records, verify date range
2. **Duplicate C1 Records**: POPP update failed, check for database errors
3. **Incorrect Amounts**: SSSA query failed, check CHECK.SSSA execution
4. **File Not Found**: Verify $XDAT environment variable is set correctly

---

## 12. Related Documentation

### Component Documentation
- [Data Dictionary](./GAP_NewLoanCash_DATA_DICTIONARY.md) - All variable definitions
- [Error Handling Analysis](./GAP_NewLoanCash_ERROR_HANDLING.md) - Risk assessment and error scenarios
- [Call Graph](./GAP_NewLoanCash_CALL_GRAPH.md) - Routine call relationships
- [Variable Mutations](./GAP_NewLoanCash_VARIABLE_MUTATIONS.md) - Variable lifecycle tracking
- [Mermaid Diagrams](./GAP_NewLoanCash_MERMAID_DIAGRAMS.md) - Visual representations

### Paragraph Documentation
- [Main Processing Loop](./paragraphs/MAIN_PROCESSING_LOOP.md)
- [CHECK.SSSA Routine](./paragraphs/CHECK.SSSA.md)

### Cross-References
- [Master Index](./GAP_NewLoanCash_INDEX.md) - Navigation hub for all documentation

---

## 13. Summary and Recommendations

### Strengths
✅ Simple, focused business logic  
✅ Effective idempotency mechanism  
✅ Handles trade reversals correctly  
✅ Generates standardized C1 records  
✅ Clear separation of concerns (main loop vs SSSA calculation)

### Weaknesses
❌ No error handling for file or database operations  
❌ Non-atomic file write + DB update (integrity risk)  
❌ No validation of environment variables  
❌ No logging or audit trail  
❌ SSSA query failure could corrupt data  
❌ Redundant code (duplicate reads)

### Priority Recommendations
1. **CRITICAL**: Implement error handling for file and database operations
2. **CRITICAL**: Add transaction logging for troubleshooting
3. **HIGH**: Validate environment variables before use
4. **HIGH**: Add SSSA query failure detection
5. **MEDIUM**: Eliminate redundant variable reads
6. **MEDIUM**: Add bounds checking for numeric calculations

### Modernization Considerations
- Consider migrating to a modern language (Python, Java)
- Implement proper transaction management (commit/rollback)
- Add comprehensive logging framework
- Externalize configuration (dates, codes, file paths)
- Implement unit testing framework

---

**Document Version**: 1.0  
**Generated**: January 23, 2026  
**Review Status**: ⚠️ AI-Generated - Requires expert review

*This documentation was generated by AI analysis and must be reviewed by OmniScript/COBOL experts for accuracy. Cross-reference with system documentation and consult experienced developers before making production changes.*
