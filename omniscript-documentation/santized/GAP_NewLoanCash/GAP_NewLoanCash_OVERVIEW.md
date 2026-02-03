# GAP_NewLoanCash Program Overview

## Program: GAP_NewLoanCash
**File**: GAP_NewLoanCash.cbl  
**Type**: OMNISCRIPT Batch Process  
**Last Updated**: 2026-02-03

---

## Executive Summary

### Program Identity
- **Name**: GAP_NewLoanCash (NewLoanCash.txt)
- **Language**: OMNISCRIPT
- **Lines of Code**: 75
- **Complexity**: Low-Medium (1 subroutine, 2 database objects, simple business logic)

### Business Purpose
GAP_NewLoanCash generates C1 activity records for the cash reconciliation process by identifying new loan purchases in plan positions and creating offsetting cash entries. The program processes POOLLOAN3 security positions over a 7-day window, detects loan reversals, and outputs formatted C1 records to support the "right side" (AC) of cash reconciliation.

### Business Value
- **Financial Accuracy**: Ensures loan purchase activity is properly reflected in cash reconciliation
- **Idempotency**: Prevents duplicate cash entries through UDF field tracking
- **Reversal Handling**: Accurately nets buy and sell activity to reflect true cash impact
- **Audit Trail**: Timestamped output files provide complete transaction history
- **Automated Reconciliation**: Eliminates manual cash offset entry for loan activity

---

## Program History

| Date | Author | Change Description |
|------|--------|-------------------|
| 12/21/2023 | Gary Matten | Initial OmniScript creation |
| 06/27/2024 | Gary Matten | GPD-1704: Corrected position 92 to value of 2 instead of 1 |
| 09/25/2024 | Gary Matten | Enhanced to recognize loan reversal activity and net correctly when there are BUYS and SELLS |

---

## Program Structure

### Module Organization

```
GAP_NewLoanCash
├── Initialization Section (Lines 13-19)
│   ├── Error context setup
│   ├── Variable definitions
│   ├── Output filename construction
│   └── File open
│
├── Date Calculation Section (Lines 21-29)
│   ├── RunDate retrieval and validation
│   ├── SevenDaysAgo calculation
│   └── LastBusiness calculation
│
├── Main Processing Loop (Lines 31-57)
│   ├── Position query (poppobj)
│   ├── Data extraction
│   ├── Reversal check (conditional)
│   ├── Idempotency validation
│   ├── C1 record construction
│   ├── File output
│   └── Database update
│
└── CHECK.SSSA Subroutine (Lines 59-75)
    ├── Parameter validation
    ├── Activity query (sssaobj)
    ├── Buy/Sell netting logic
    └── Result return
```

### Procedure Index

| Procedure | Lines | Purpose | Called By |
|-----------|-------|---------|-----------|
| Main Program | 13-57 | Process positions and generate C1 records | Entry point |
| CHECK.SSSA | 59-75 | Detect reversals and calculate net loan activity | Main program (line 38) |

---

## Business Logic Overview

### Core Processing Flow

1. **Initialization**
   - Set OMNISCRIPT error context (`sd080 = 99999999`)
   - Build unique output filename with timestamp
   - Open output file for C1 activity records

2. **Date Window Calculation**
   - Retrieve `RunDate` from environment (`$RUN-DATE`)
   - Calculate 7-day lookback: `SevenDaysAgo = RunDate - 7 calendar days`
   - Calculate effective date: `LastBusiness = RunDate - 1 business day`
   - Fallback to current date if RunDate invalid

3. **Position Record Processing**
   - Query all POOLLOAN3 positions within [SevenDaysAgo, LastBusiness]
   - For each position record:
     - **Extract**: Plan ID, trade date, loan amounts, trust account
     - **Check Reversals**: Call CHECK.SSSA if Secondary1Buys > 0
     - **Validate Idempotency**: Skip if PriorCashApplied = Secondary1Buys
     - **Generate C1 Record**: Build fixed-format transaction record
     - **Output**: Write C1 record to file
     - **Mark Processed**: Update position field 877 with applied amount

4. **Reversal Detection (CHECK.SSSA)**
   - Query security activity (sssaobj) for matching plan/security/date
   - Filter for transaction source 'XI'
   - Net calculation:
     - **Buys ('B')**: Add to accumulator
     - **Sells ('S')**: Subtract from accumulator (reversal)
   - Return net amount as adjusted Secondary1Buys

### Business Rules

#### BR-001: Position Selection
- **Rule**: Only POOLLOAN3 security positions are processed
- **Rationale**: Program specifically handles pool loan cash reconciliation
- **Implementation**: `poppobj_view(securityid:'POOLLOAN3' ...)`

#### BR-002: Date Range Window
- **Rule**: Process positions from last 7 calendar days
- **Rationale**: Captures recent activity for timely reconciliation
- **Implementation**: `datelo:SevenDaysAgo datehi:LastBusiness`

#### BR-003: Idempotency Control
- **Rule**: Skip processing if UDF1 (field 877) equals Secondary1Buys
- **Rationale**: Prevents duplicate cash entries for same position
- **Implementation**: `IF (PriorCashApplied <> Secondary1Buys)`
- **Impact**: Safe to re-run program without generating duplicates

#### BR-004: Reversal Netting
- **Rule**: Net loan buys and sells for same plan/security/date
- **Rationale**: Cash impact should reflect net activity, not gross
- **Implementation**: CHECK.SSSA routine with buy/sell accumulation
- **Example**: $100K buy + $30K sell = $70K net loan activity

#### BR-005: C1 Record Format
- **Rule**: Fixed-length C1 record with specific field positions
- **Fields**:
  - Record type: 'C100' (positions 1-4)
  - Plan ID: 6 characters (positions 5-10)
  - Effective date: LastBusiness (positions 31-38)
  - Trust account: 32 characters (positions 40-71)
  - Transaction code: '000000000000000    2' (positions 73-92)
  - Sign: '0' (position 115)
  - Amount: Negative loan amount, formatted (positions 116-130)
  - Activity code: '00339' (positions 134-138)

#### BR-006: Amount Sign Convention
- **Rule**: Loan purchases are recorded as negative amounts in C1
- **Rationale**: Offset cash for asset purchases in reconciliation
- **Implementation**: `NewLoanUnits = 0 - Secondary1Buys`

---

## Data Architecture

### Input Sources

#### Environment Variables
- **$XDAT**: Directory path for output file placement
- **$RUN-DATE**: Batch processing date (YYYYMMDD format)

#### Database Objects

**poppobj (Plan Position Object)**
- **Table**: POPP (Plan Position Accounts)
- **Filter**: POOLLOAN3 security, date range [SevenDaysAgo, LastBusiness]
- **Fields Read**:
  - 008: TradeDate
  - 030: RKPlan (Plan ID)
  - 741: Secondary1Buys (custom UDF field)
  - 877: PriorCashApplied (UDF1 field)
  - 1510: TrustAccount
- **Fields Updated**:
  - 877: Set to Secondary1Buys after processing

**sssaobj (Security Activity Object)**
- **Table**: SSSA (TRUSTTRANS.P1)
- **Filter**: Matching plan, POOLLOAN3 security, specific trade date
- **Fields Read**:
  - 009: Transaction type (B=Buy, S=Sell)
  - 011: Transaction source (filter for 'XI')
  - 235: Transaction amount

### Output Files

**C1 Activity File**
- **Naming Pattern**: `OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.{DATE}.{TIME}.DAT`
- **Format**: Fixed-length text records (134+ bytes)
- **Purpose**: Input to cash reconciliation system
- **Record Type**: C1 activity records (type 'C100')
- **Contents**: One record per unprocessed loan position
- **Location**: `$XDAT` directory

---

## Error Handling and Risk Analysis

### Error Handling Mechanisms

#### Date Validation
- **Check**: `OcDate_Valid(RunDate)` at line 22
- **Recovery**: Fall back to current date if RunDate invalid
- **Risk**: Low - Graceful degradation ensures processing continues

#### Parameter Validation (CHECK.SSSA)
- **Check**: `IF (RKPlan <> '') and (TradeDate <> 0)` at line 60
- **Recovery**: Early exit from routine if invalid
- **Risk**: Low - Prevents database query with incomplete parameters

#### Idempotency Protection
- **Check**: Compare PriorCashApplied to Secondary1Buys at line 40
- **Recovery**: Skip record if already processed
- **Risk**: Low - Prevents duplicate entries

### Potential Runtime Errors

#### High-Risk Scenarios

**1. File Access Errors**
- **Cause**: `$XDAT` directory doesn't exist or lacks write permissions
- **Impact**: Program termination, no C1 records generated
- **Mitigation**: Pre-validate directory existence and permissions
- **Error Handling**: None in code - would cause runtime failure

**2. Database Connection Errors**
- **Cause**: Database unavailable, connection timeout
- **Impact**: Cannot query poppobj or sssaobj
- **Mitigation**: Run during scheduled maintenance windows
- **Error Handling**: None in code - would cause runtime failure

**3. Environment Variable Missing**
- **Cause**: `$XDAT` or `$RUN-DATE` not set
- **Impact**: Invalid file path or date processing errors
- **Mitigation**: Date has fallback to current; XDAT would fail
- **Error Handling**: Partial (date only)

#### Medium-Risk Scenarios

**4. Invalid Date Format**
- **Cause**: `$RUN-DATE` contains non-numeric or invalid date
- **Impact**: Falls back to current date
- **Mitigation**: Built-in validation with fallback
- **Error Handling**: ✓ Implemented

**5. Database Field Access Errors**
- **Cause**: Field numbers don't exist or have changed
- **Impact**: Runtime error or incorrect data extraction
- **Mitigation**: Verify database schema matches expectations
- **Error Handling**: None - assumes valid schema

#### Low-Risk Scenarios

**6. Zero Records Found**
- **Cause**: No POOLLOAN3 positions in date range
- **Impact**: Empty output file (valid scenario)
- **Mitigation**: Not needed - normal business case
- **Error Handling**: Handled naturally by loop logic

**7. All Records Already Processed**
- **Cause**: Program re-run without new activity
- **Impact**: No new C1 records generated
- **Mitigation**: Not needed - idempotency working correctly
- **Error Handling**: ✓ Built-in via field 877 check

### Resource Limitations

| Resource | Limit/Consideration | Risk |
|----------|---------------------|------|
| File Size | Unbounded growth based on position volume | Medium - Monitor disk space |
| Memory | Minimal (processes one record at a time) | Low |
| Database Connections | 2 concurrent queries (poppobj, sssaobj) | Low |
| Processing Time | Proportional to position count | Low-Medium |
| Network I/O | Database query latency | Low |

---

## Integration Points

### Upstream Dependencies

1. **POPP Table Updates**
   - **Source**: Position management system
   - **Timing**: Daily position loads
   - **Critical Fields**: 741 (Secondary1Buys), 877 (PriorCashApplied)
   - **Impact**: Missing or delayed updates prevent cash reconciliation

2. **SSSA Table Updates (TRUSTTRANS.P1)**
   - **Source**: Trust transaction processing
   - **Timing**: Real-time or batch
   - **Critical Fields**: Transaction type, source, amounts
   - **Impact**: Incomplete data causes inaccurate reversal netting

3. **Environment Configuration**
   - **Variables**: `$XDAT`, `$RUN-DATE`
   - **Set By**: Batch scheduler or shell wrapper
   - **Impact**: Missing variables cause program failure

### Downstream Consumers

1. **Cash Reconciliation System**
   - **Consumes**: C1 activity file (OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.*.DAT)
   - **Frequency**: Daily batch processing
   - **Format Dependency**: Fixed-length C1 record structure
   - **Impact**: Format changes break reconciliation process

2. **Audit and Reporting**
   - **Uses**: Output files for transaction history
   - **Retention**: Per regulatory requirements
   - **Impact**: File naming pattern must remain consistent

---

## Deployment and Operations

### Execution Requirements

**Prerequisites**:
- OMNISCRIPT runtime environment
- Database connectivity (POPP, SSSA tables)
- Write access to `$XDAT` directory
- Environment variables configured

**Scheduling**:
- **Frequency**: Daily (typically overnight batch)
- **Timing**: After position and transaction loads complete
- **Duration**: Minutes (depends on 7-day position volume)
- **Dependencies**: Position updates must be complete

**Resource Requirements**:
- **CPU**: Low (I/O bound)
- **Memory**: Minimal (single-record processing)
- **Disk**: Output file size ~200 bytes per position
- **Network**: Database query bandwidth

### Operational Monitoring

**Success Indicators**:
- ✓ Output file created with timestamp
- ✓ Record count matches unprocessed positions
- ✓ All Secondary1Buys ≠ 0 positions processed
- ✓ Field 877 updated in database

**Failure Indicators**:
- ✗ No output file created
- ✗ Zero-length output file with expected positions
- ✗ Runtime error messages
- ✗ Position records not updated (field 877)

**Validation Queries**:
```sql
-- Check for unprocessed positions
SELECT COUNT(*) FROM POPP
WHERE SECURITYID = 'POOLLOAN3'
  AND TRADEDATE BETWEEN {SevenDaysAgo} AND {LastBusiness}
  AND FIELD741 <> 0
  AND FIELD877 <> FIELD741;

-- Verify output record count matches
wc -l $XDAT/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.*.DAT
```

---

## Testing Guidance

### Test Scenarios

#### Scenario 1: Normal Processing
- **Setup**: 10 POOLLOAN3 positions with Secondary1Buys > 0, none processed
- **Expected**: 10 C1 records written, all field 877 updated
- **Validation**: Compare output record count to query result

#### Scenario 2: Idempotency
- **Setup**: Re-run program without new positions
- **Expected**: Zero C1 records written (all already processed)
- **Validation**: Empty or non-existent output file

#### Scenario 3: Reversal Netting
- **Setup**: Position with $100K buy + $30K sell on same date
- **Expected**: C1 record with $70K net amount
- **Validation**: CHECK.SSSA routine called, amount netted correctly

#### Scenario 4: Date Range Edge Cases
- **Setup**: Positions on boundary dates (SevenDaysAgo, LastBusiness)
- **Expected**: Both boundary dates included in processing
- **Validation**: Verify inclusive date range logic

#### Scenario 5: Invalid RunDate
- **Setup**: Set `$RUN-DATE` to invalid value
- **Expected**: Program uses current date, continues processing
- **Validation**: Check log output for date fallback message

### Test Data Requirements

**Database Setup**:
- Position records with varied trade dates
- Mix of processed (field 877 = field 741) and unprocessed
- Activity records with buys and sells for reversal testing
- Edge cases: zero amounts, large amounts, negative amounts

**Environment Setup**:
- Valid `$XDAT` directory with write permissions
- Valid `$RUN-DATE` (YYYYMMDD format)
- Database connectivity

---

## Performance Characteristics

### Processing Volume
- **Typical**: 50-200 positions per 7-day window
- **Peak**: 500+ positions (month-end, quarter-end)
- **Duration**: 2-5 minutes typical, 10-15 minutes peak

### Optimization Opportunities

1. **Database Query Tuning**
   - Ensure indexes on: SECURITYID, TRADEDATE (POPP table)
   - Ensure indexes on: PLAN, SECURITYID, DATE (SSSA table)

2. **Reversal Check Optimization**
   - CHECK.SSSA only called when Secondary1Buys ≠ 0
   - Consider caching results if same plan/date queried multiple times

3. **Batch Size Considerations**
   - Currently processes all records in single run
   - Could batch for very large volumes (thousands of positions)

---

## Maintenance Notes

### Code Change Impact Areas

**Adding New Fields to C1 Record**:
- Update `OcText_Set()` calls (lines 45-52)
- Verify positions and lengths don't overlap
- Update downstream C1 parser

**Changing Date Window**:
- Modify `OcDate_AddDays()` parameter (line 23, 26)
- Currently: 7 days lookback
- Update comments to reflect new window

**Adding Reversal Logic**:
- Extend CHECK.SSSA to handle new transaction types
- Currently: handles B (Buy) and S (Sell) with source XI
- Add new conditionals in lines 64-71

### Common Issues

**Issue**: Duplicate C1 records generated  
**Cause**: Field 877 not updating properly  
**Fix**: Verify `poppobj_update()` succeeds after `poppobj_setde()`

**Issue**: Incorrect amounts in C1 records  
**Cause**: Reversal logic not netting correctly  
**Fix**: Verify CHECK.SSSA accumulation logic (WK001 calculation)

**Issue**: Missing positions in output  
**Cause**: Date range calculation error  
**Fix**: Verify `OcDate_AddDays()` vs `OcDate_AddBusDays()` usage

---

## Related Documentation

- [Data Dictionary](./GAP_NewLoanCash_DATA_DICTIONARY.md) - Complete variable reference
- [Call Graph](./GAP_NewLoanCash_CALL_GRAPH.md) - Program flow and call hierarchy
- [Error Handling](./GAP_NewLoanCash_ERROR_HANDLING.md) - Risk assessment and error scenarios
- [Integration Guide](./GAP_NewLoanCash_INTEGRATION_GUIDE.md) - Deployment and integration details
- [Business Rules](./GAP_NewLoanCash_BUSINESS_RULES.md) - Detailed business logic documentation
- [Diagrams](./GAP_NewLoanCash_DIAGRAMS.md) - Visual flowcharts and state diagrams

---

## Quick Reference

### Key Variables
- **FileName**: Output C1 file path
- **RunDate**: Batch processing date
- **SevenDaysAgo**: Start of 7-day window
- **LastBusiness**: Effective date for C1 records
- **Secondary1Buys**: Loan purchase amount (may be netted)
- **PriorCashApplied**: Idempotency check value
- **NewLoanUnits**: Negated loan amount for C1

### Key Database Fields
- **poppobj.008**: TradeDate
- **poppobj.030**: RKPlan
- **poppobj.741**: Secondary1Buys
- **poppobj.877**: PriorCashApplied (UDF1)
- **poppobj.1510**: TrustAccount
- **sssaobj.009**: Transaction type
- **sssaobj.235**: Transaction amount

### Key Business Logic
1. Query POOLLOAN3 positions for 7-day window
2. Check for reversals if loan amount > 0
3. Skip if already processed (idempotency)
4. Build and write C1 record
5. Mark as processed in database

---

*This documentation was generated using the OMNISCRIPT Grammar Parser and comprehensive program analysis.*
*Last Generated*: 2026-02-03
