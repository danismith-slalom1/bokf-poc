# GAP_NewLoanCash Error Handling Analysis

**Program**: GAP_NewLoanCash.cbl  
**Last Updated**: February 4, 2026  
**Purpose**: Document error handling strategies, risks, and mitigation approaches

---

## Error Handling Strategy Overview

The GAP_NewLoanCash program employs a **defensive programming** approach with:
- Input validation for critical parameters
- Fallback logic for environmental dependencies
- Idempotency to handle re-runs safely
- Conditional processing to skip invalid data

However, the program **lacks explicit error trapping** for:
- Database connection failures
- File I/O errors
- Data type mismatches
- Missing environment variables

---

## Implemented Error Handling

### 1. Invalid Run Date Handling

**Location**: Lines 22-28

**Error Condition**: $RUN-DATE environment variable invalid or missing

**Detection Method**:
```omniscript
if OcDate_Valid(RunDate);
```

**Recovery Strategy**:
```omniscript
else;
   SevenDaysAgo = OcDate_AddDays(OcDate_Current() -7);
   LastBusiness = OcDate_AddBusDays(OcDate_Current() -1);
end;
```

**Risk Level**: LOW
- **Impact**: Uses current date instead of configured run date
- **Business Impact**: May process slightly different date range than intended
- **Mitigation**: Explicit date validation with automatic fallback

**Recommendation**: ✅ Adequate - consider logging when fallback is used

---

### 2. Zero Secondary1Buys Optimization

**Location**: Line 37

**Condition**: No loan activity on position record

**Handling**:
```omniscript
If (Secondary1Buys <> 0);
   PERFORM 'CHECK.SSSA';
End;
```

**Risk Level**: LOW
- **Impact**: Skips unnecessary SSSA query
- **Business Impact**: Performance optimization only
- **Mitigation**: Explicit guard clause

**Recommendation**: ✅ Optimal - reduces database load

---

### 3. Idempotency Protection

**Location**: Line 40

**Error Condition**: Attempting to process already-handled position

**Detection Method**:
```omniscript
if (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0);
```

**Recovery Strategy**: Skip processing, move to next record

**Risk Level**: LOW
- **Impact**: Prevents duplicate C1 records
- **Business Impact**: Critical for data integrity on re-runs
- **Mitigation**: Compare current value to previously processed value

**Recommendation**: ✅ Excellent - essential idempotency pattern

---

### 4. CHECK.SSSA Parameter Validation

**Location**: Line 60

**Error Condition**: Missing required parameters for SSSA query

**Detection Method**:
```omniscript
if (RKPlan <> '') and (TradeDate <> 0);
```

**Recovery Strategy**: Early exit via GOBACK without processing

**Risk Level**: MEDIUM
- **Impact**: SSSA query skipped, uses original Secondary1Buys value
- **Business Impact**: May use incorrect amount if SSSA data exists
- **Mitigation**: Explicit parameter validation

**Recommendation**: ⚠️ Consider logging when parameters invalid - indicates data quality issue

---

## Unhandled Error Scenarios

### CRITICAL RISKS

#### 1. Database Connection Failures

**Affected Operations**:
- `poppobj_view()` (Line 31)
- `sssaobj_view()` (Line 62)
- `poppobj_update()` (Line 55)

**Current Handling**: None - program assumes database availability

**Potential Impact**:
- **poppobj_view() failure**: No positions processed, zero C1 records (complete failure)
- **sssaobj_view() failure**: Uses original Secondary1Buys (potentially incorrect amounts)
- **poppobj_update() failure**: No idempotency tracking, duplicate C1s on re-run

**Risk Level**: CRITICAL

**Mitigation Strategies**:
1. **Wrap in error handler**: Check return codes from database operations
2. **Retry logic**: Implement connection retry with exponential backoff
3. **Alerting**: Send notification on database connection failure
4. **Logging**: Log failed queries with parameters for debugging

**Recommended Code Enhancement**:
```omniscript
/* Check if view succeeded */
if poppobj_view(securityid:'POOLLOAN3' datelo:SevenDaysAgo datehi:LastBusiness);
   loop while poppobj_next();
      /* process records */
   endloop;
else;
   /* Log error and exit */
   OcShow('ERROR: Failed to query position objects');
   sd080 = 1;  /* Set error return code */
end;
```

---

#### 2. File I/O Failures

**Affected Operations**:
- `OcFile1_Open()` (Line 19)
- `OcFile1_Write()` (Line 53)

**Current Handling**: None - assumes file system availability

**Potential Impact**:
- **Open failure**: Program crashes or writes to wrong location
- **Write failure**: Lost C1 records, incomplete reconciliation
- **Disk full**: Partial file written, reconciliation data corrupted

**Risk Level**: CRITICAL

**Mitigation Strategies**:
1. **Check open result**: Verify file opened successfully
2. **Check write result**: Verify each write succeeded
3. **Pre-flight check**: Verify $XDAT directory exists and is writable
4. **Disk space check**: Verify sufficient space before processing
5. **Atomic writes**: Write to temp file, rename on success

**Recommended Code Enhancement**:
```omniscript
/* Verify file opened */
if OcFile1_Open(name:FileName mode:'OUTPUT');
   /* proceed */
else;
   OcShow('ERROR: Failed to open output file: ' FileName);
   sd080 = 2;
end;
```

---

#### 3. Missing Environment Variables

**Affected Variables**:
- `$XDAT` (Line 16)
- `$RUN-DATE` (Line 21)

**Current Handling**:
- $XDAT: No validation - could be undefined
- $RUN-DATE: Validated via OcDate_Valid(), has fallback

**Potential Impact**:
- **$XDAT undefined**: Filename constructed incorrectly, file written to wrong location
- **$RUN-DATE invalid**: Handled via fallback (LOW risk)

**Risk Level**: HIGH (for $XDAT)

**Mitigation Strategies**:
1. **Validate environment variables**: Check for undefined/empty values
2. **Default values**: Provide sensible defaults if undefined
3. **Pre-flight validation**: Check all required env vars at program start

**Recommended Code Enhancement**:
```omniscript
/* Validate $XDAT */
x.XdatDir = OcText_GetEnv('$XDAT');
if (x.XdatDir = '');
   x.XdatDir = '/data/omniscript';  /* Default location */
   OcShow('WARNING: $XDAT undefined, using default: ' x.XdatDir);
end;
```

---

### HIGH RISKS

#### 4. Data Type Mismatches

**Risk Areas**:
- Numeric fields containing non-numeric data
- Date fields with invalid date values
- String fields exceeding expected length

**Current Handling**: None - assumes clean data

**Potential Impact**:
- Calculation errors in CHECK.SSSA (WK001 accumulation)
- Invalid C1 record formatting
- Program crash on type coercion failure

**Risk Level**: HIGH

**Mitigation Strategies**:
1. **Type validation**: Check field types before arithmetic operations
2. **Range checks**: Validate amounts are within reasonable bounds
3. **Data quality monitoring**: Track and alert on invalid data patterns

**Recommended Code Enhancement**:
```omniscript
/* Validate numeric fields */
if OcNum_Valid(Secondary1Buys) and (Secondary1Buys >= 0);
   /* proceed */
else;
   OcShow('WARNING: Invalid Secondary1Buys value: ' Secondary1Buys ' for plan: ' RKPlan);
   /* Skip this record */
end;
```

---

#### 5. Missing SSSA Records

**Scenario**: Position has Secondary1Buys but no corresponding SSSA records

**Current Handling**: WK001 remains 0, Secondary1Buys set to 0 (implicit)

**Potential Impact**:
- No C1 record generated (because Secondary1Buys becomes 0)
- Cash reconciliation incomplete
- Unreported discrepancy between position and settlement

**Risk Level**: HIGH

**Mitigation Strategies**:
1. **Log missing SSSA**: Alert when position has activity but no SSSA records
2. **Exception report**: Generate report of positions with missing SSSA
3. **Business rule**: Decide if missing SSSA should use position value or skip

**Recommended Code Enhancement**:
```omniscript
ROUTINE 'CHECK.SSSA';
if (RKPlan <> '') and (TradeDate <> 0);
   WK001 = 0;
   n.RecordCount = 0;
   sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
   loop while sssaobj_next();
      n.RecordCount = n.RecordCount + 1;
      /* process records */
   endloop;
   if (n.RecordCount = 0) and (Secondary1Buys <> 0);
      OcShow('WARNING: No SSSA records found for plan: ' RKPlan ' date: ' TradeDate ' amount: ' Secondary1Buys);
   end;
   Secondary1Buys = WK001;
end;
GOBACK;
```

---

### MEDIUM RISKS

#### 6. Concurrent Execution

**Scenario**: Multiple instances of program running simultaneously

**Current Handling**: None - no locking mechanism

**Potential Impact**:
- File name collisions (LOW - timestamp provides uniqueness)
- Duplicate C1 records if both instances process before UDF1 updates
- Database contention on position updates

**Risk Level**: MEDIUM

**Mitigation Strategies**:
1. **File locking**: Use lock file to prevent concurrent execution
2. **Timestamp granularity**: Current timestamp to seconds provides uniqueness
3. **Scheduler control**: Ensure batch scheduler prevents concurrent runs
4. **Database locking**: Rely on database row-level locks during update

---

#### 7. Incomplete SSSA Data

**Scenario**: SSSA records not yet fully loaded when program runs

**Current Handling**: None - assumes SSSA data complete

**Potential Impact**:
- Incorrect netting calculations (missing recent SELLS)
- Cash reconciliation inaccurate
- Must be re-run after SSSA loading completes

**Risk Level**: MEDIUM

**Mitigation Strategies**:
1. **Dependency management**: Ensure SSSA loading completes before program runs
2. **Timestamp checks**: Validate SSSA data freshness
3. **Batch sequencing**: Schedule program after SSSA load job

---

## Error Handling Maturity Assessment

| Category | Current State | Recommended State | Gap |
|----------|---------------|-------------------|-----|
| **Input Validation** | Partial (dates only) | Comprehensive | HIGH |
| **Database Errors** | None | Explicit handling | CRITICAL |
| **File I/O Errors** | None | Explicit handling | CRITICAL |
| **Data Quality** | Implicit (skip on conditions) | Explicit validation + logging | HIGH |
| **Error Logging** | Minimal (OcShow only) | Structured logging | MEDIUM |
| **Error Recovery** | Fallback (dates) | Retry + alerting | HIGH |
| **Alerting** | None | Critical error alerts | HIGH |

---

## Recommended Error Handling Enhancements

### Priority 1: Critical

1. **Add database error checking**:
   - Validate poppobj_view() success
   - Validate poppobj_update() success
   - Log and alert on failures

2. **Add file I/O error checking**:
   - Validate OcFile1_Open() success
   - Validate OcFile1_Write() success
   - Check $XDAT directory exists

3. **Implement structured error logging**:
   - Log to error file (not just OcShow)
   - Include timestamp, program name, error details
   - Capture full context (plan, date, amounts)

### Priority 2: High

4. **Add data validation**:
   - Validate numeric fields before arithmetic
   - Check date field validity
   - Validate string lengths

5. **Implement missing SSSA detection**:
   - Log when position has activity but no SSSA records
   - Generate exception report

6. **Add environment variable validation**:
   - Check $XDAT defined and valid path
   - Provide defaults or fail gracefully

### Priority 3: Medium

7. **Add concurrent execution protection**:
   - Implement lock file mechanism
   - Detect and prevent concurrent runs

8. **Implement retry logic**:
   - Retry database operations on transient failures
   - Exponential backoff strategy

9. **Add monitoring metrics**:
   - Count records processed
   - Count records skipped
   - Count C1 records generated
   - Track processing time

---

## Error Logging Standards

### Proposed Log Format

```
[TIMESTAMP] [LEVEL] [PROGRAM] [PLAN] [MESSAGE]
```

### Log Levels

- **ERROR**: Critical failures requiring intervention
- **WARN**: Recoverable issues requiring investigation
- **INFO**: Normal processing events
- **DEBUG**: Detailed diagnostic information

### Example Log Entries

```
[2026-02-04 14:30:52] ERROR GAP_NewLoanCash - Failed to open output file: /data/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.20260204.143052.DAT
[2026-02-04 14:31:15] WARN GAP_NewLoanCash ABC123 No SSSA records found for plan: ABC123 date: 20260204 amount: 100000.00
[2026-02-04 14:31:16] INFO GAP_NewLoanCash ABC123 Generated C1 record for plan: ABC123 amount: 75000.00
```

---

## Operational Monitoring

### Key Metrics to Track

1. **Success Rate**: % of runs completing without errors
2. **Record Counts**:
   - Position records queried
   - Position records with activity
   - C1 records generated
   - Position records skipped (already processed)
3. **Processing Time**: Duration of batch run
4. **Error Counts**: By error type
5. **Data Quality**: Count of validation failures

### Alert Conditions

1. **Database connection failure** → Page on-call
2. **File write failure** → Page on-call
3. **Zero C1 records generated** (when activity expected) → Email alert
4. **High rate of missing SSSA records** → Email alert
5. **Processing time > threshold** → Email alert

---

## Disaster Recovery

### Recovery Procedures

**Scenario 1: Program crashes mid-run**
- **Detection**: No output file created or file incomplete
- **Recovery**: 
  1. Fix underlying issue (database, disk space, etc.)
  2. Re-run program
  3. Idempotency prevents duplicate C1 records

**Scenario 2: Duplicate C1 records generated**
- **Detection**: Manual review or reconciliation reports
- **Root Cause**: poppobj_update() failed to mark records as processed
- **Recovery**:
  1. Identify duplicate C1 records
  2. Manually mark position records (set UDF1 = Secondary1Buys)
  3. Re-run program to process remaining positions only

**Scenario 3: Incorrect C1 amounts**
- **Detection**: Cash reconciliation discrepancies
- **Root Cause**: SSSA data incomplete or incorrect
- **Recovery**:
  1. Investigate SSSA data quality
  2. Correct SSSA records
  3. Reset position UDF1 fields to 0
  4. Re-run program

---

## Questions for Expert Review

1. **Error Handling Standards**: What are organizational standards for OMNISCRIPT error handling?
2. **Database Error Codes**: What error codes does OMNISCRIPT return for database failures?
3. **File I/O Return Codes**: How to check success of OcFile1_Open() and OcFile1_Write()?
4. **Logging Infrastructure**: Is there a centralized logging system for OMNISCRIPT programs?
5. **Alerting**: What alerting mechanisms are available (email, paging, etc.)?
6. **Missing SSSA Policy**: Should program skip positions with missing SSSA or use original value?
7. **Concurrent Execution**: Are there existing lock file patterns in use?

---

**Review Status**: Generated by automated analysis - **REQUIRES EXPERT VALIDATION** of error handling requirements and organizational standards

**Generated**: February 4, 2026
