# GAP_NewLoanCash Business Rules

## Program: GAP_NewLoanCash
**Purpose**: Detailed business logic and rules documentation  
**Last Updated**: 2026-02-03

---

## Business Rules Catalog

### BR-001: Security Type Filter
**Category**: Data Selection  
**Priority**: CRITICAL

**Rule Statement**:  
Only process position records with `SECURITYID = 'POOLLOAN3'`

**Business Rationale**:  
This program specifically handles pool loan cash reconciliation. Other security types have different reconciliation processes and should not be processed by this program.

**Implementation**:
```omniscript
poppobj_view(securityid:'POOLLOAN3' datelo:SevenDaysAgo datehi:LastBusiness);
```

**Validation**:
- Query explicitly filters for 'POOLLOAN3'
- No other security types are retrieved

**Impact if Violated**:
- HIGH: Incorrect cash reconciliation for other security types
- Could cause reconciliation breaks
- May generate invalid C1 records

---

### BR-002: Seven-Day Lookback Window
**Category**: Data Selection  
**Priority**: HIGH

**Rule Statement**:  
Process position records with trade dates in the range [RunDate - 7 calendar days, RunDate - 1 business day]

**Business Rationale**:  
- Captures recent loan activity for timely reconciliation
- 7-day window ensures late-arriving positions are captured
- Upper bound of last business day ensures only completed days are processed

**Implementation**:
```omniscript
SevenDaysAgo = OcDate_AddDays(RunDate -7);
LastBusiness = OcDate_AddBusDays(RunDate -1);
poppobj_view(securityid:'POOLLOAN3' datelo:SevenDaysAgo datehi:LastBusiness);
```

**Parameters**:
- **Lower Bound**: RunDate - 7 calendar days (inclusive)
- **Upper Bound**: RunDate - 1 business day (inclusive)

**Date Calculation Rules**:
- Calendar days: Include weekends and holidays
- Business days: Exclude weekends and holidays (for upper bound only)
- Date range is inclusive on both ends

**Examples**:
| RunDate | SevenDaysAgo | LastBusiness | Positions Included |
|---------|--------------|--------------|-------------------|
| 2024-01-15 (Mon) | 2024-01-08 (Mon) | 2024-01-12 (Fri) | Jan 8-12 |
| 2024-01-16 (Tue) | 2024-01-09 (Tue) | 2024-01-15 (Mon) | Jan 9-15 |
| 2024-01-22 (Mon) | 2024-01-15 (Mon) | 2024-01-19 (Fri) | Jan 15-19 (skips weekend) |

**Impact if Violated**:
- MEDIUM: Miss recent positions (too short window)
- MEDIUM: Process old positions unnecessarily (too long window)
- Could cause reconciliation timing issues

---

### BR-003: Idempotency Control
**Category**: Data Integrity  
**Priority**: CRITICAL

**Rule Statement**:  
Skip processing if `PriorCashApplied (field 877) = Secondary1Buys (field 741)`

**Business Rationale**:  
- Prevents duplicate C1 activity records
- Allows safe re-execution of the program
- Supports operational recovery scenarios
- Maintains cash reconciliation accuracy

**Implementation**:
```omniscript
PriorCashApplied = poppobj_numde(877);
Secondary1Buys = poppobj_numde(741);
if (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0);
   [... process record ...]
   poppobj_setde(denum:877 value:Secondary1Buys);
   poppobj_update();
end;
```

**Processing States**:
- **Unprocessed**: Field 877 ≠ Field 741 → Process and generate C1 record
- **Processed**: Field 877 = Field 741 → Skip (already done)

**Field 877 (UDF1) Usage**:
- Stores the Secondary1Buys amount after processing
- Acts as "processed flag" and "amount verification"
- Allows reprocessing if amount changes in field 741

**Scenarios**:

| Field 741 (Buy) | Field 877 (Prior) | Action | Reason |
|----------------|-------------------|--------|---------|
| $100,000 | $0 | Process | First time |
| $100,000 | $100,000 | Skip | Already processed |
| $100,000 | $50,000 | Process | Amount changed (rare) |
| $0 | $0 | Skip | No loan activity |
| $0 | $100,000 | Skip | Zero amount (unusual) |

**Impact if Violated**:
- CRITICAL: Duplicate C1 records generated
- Double-counted cash offsets
- Reconciliation errors
- Data integrity violations

---

### BR-004: Zero Amount Exclusion
**Category**: Data Validation  
**Priority**: HIGH

**Rule Statement**:  
Skip position records where `Secondary1Buys = 0`

**Business Rationale**:  
- No loan activity occurred (no cash impact)
- Avoids generating meaningless C1 records
- Reduces processing overhead

**Implementation**:
```omniscript
if (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0);
   [... process record ...]
end;
```

**Examples**:
- `Secondary1Buys = 0`: Skip (no activity)
- `Secondary1Buys = $100,000`: Check reversal, then process
- `Secondary1Buys = -$50,000`: Process (negative value unusual but valid)

**Impact if Violated**:
- LOW: Generate unnecessary C1 records with $0 amount
- Clutters reconciliation with no-op entries
- Minimal financial impact

---

### BR-005: Reversal Netting Logic
**Category**: Transaction Processing  
**Priority**: CRITICAL

**Rule Statement**:  
Net loan buys and sells for the same plan, security, and trade date before generating C1 record

**Business Rationale**:  
- Cash impact should reflect net activity, not gross
- Loan reversals (sells) offset loan purchases (buys)
- Prevents overstating cash requirements
- Aligns with actual cash movement

**Implementation**:
```omniscript
/* In main program */
If (Secondary1Buys <> 0);
   PERFORM 'CHECK.SSSA';  /* Calculates net amount */
End;

/* In CHECK.SSSA routine */
WK001 = 0;
sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
loop while sssaobj_next();
  if sssaobj_de(011) = 'XI';  /* Transaction source filter */
     if sssaobj_de(009) = 'B';
        WK001 = WK001 + sssaobj_numde(235);  /* Add buys */
     end;
     if sssaobj_de(009) = 'S';
        WK001 = WK001 - sssaobj_numde(235);  /* Subtract sells */
     end; 
  end;
endloop;
Secondary1Buys = WK001;  /* Net amount */
```

**Netting Calculation**:
```
Net Loan Activity = Σ(Buys) - Σ(Sells) where TransactionSource = 'XI'
```

**Transaction Type Codes**:
- **'B' (Buy)**: Loan purchase → Add to net
- **'S' (Sell)**: Loan reversal → Subtract from net
- **Other**: Not processed by this logic

**Transaction Source Filter**:
- Only process transactions where `Field 011 = 'XI'`
- Other source codes are excluded from netting

**Examples**:

| Scenario | Buys | Sells | Net Activity | C1 Amount |
|----------|------|-------|--------------|-----------|
| Simple Buy | $100,000 | $0 | $100,000 | -$100,000 |
| Partial Reversal | $100,000 | $30,000 | $70,000 | -$70,000 |
| Full Reversal | $100,000 | $100,000 | $0 | -$0 (skip) |
| Over-Reversal | $100,000 | $120,000 | -$20,000 | +$20,000 (unusual) |
| Multiple Buys/Sells | $100K + $50K | $30K + $20K | $100,000 | -$100,000 |

**Special Cases**:
- **No Reversal Activity**: WK001 = sum of buys only
- **Full Reversal**: Net = 0, skip processing (BR-004)
- **Negative Net**: Unusual but valid (generates positive C1 amount)

**Impact if Violated**:
- CRITICAL: Incorrect cash reconciliation
- Overstated or understated cash requirements
- Reconciliation breaks
- Financial reporting errors

---

### BR-006: C1 Amount Sign Convention
**Category**: Record Formatting  
**Priority**: CRITICAL

**Rule Statement**:  
C1 record amount must be the negative of the loan purchase amount

**Business Rationale**:  
- Asset purchases reduce cash (negative cash flow)
- C1 records represent cash side of reconciliation
- Consistent with accounting convention for cash offsets

**Implementation**:
```omniscript
NewLoanUnits = 0 - Secondary1Buys;  /* Negate amount */
OcText_Set(Line 116 OcFmt(NewLoanUnits 'Z,12V2-') 15);
```

**Sign Convention**:
- Loan purchase of $100,000 → C1 amount = -$100,000
- Loan reversal net of -$20,000 → C1 amount = +$20,000 (rare)

**Formatting**:
- Format: `Z,12V2-` (12 digits, 2 decimals, signed)
- Example: `-    100,000.00` (negative with leading spaces)

**Impact if Violated**:
- CRITICAL: Reverses cash flow direction
- Cash reconciliation completely incorrect
- Financial statements affected

---

### BR-007: C1 Record Structure
**Category**: Record Formatting  
**Priority**: CRITICAL

**Rule Statement**:  
C1 records must conform to fixed-length format with specific field positions and values

**Business Rationale**:  
- Downstream reconciliation system expects exact format
- Fixed positions enable efficient parsing
- Standard codes enable automated processing

**Field Specifications**:

| Field | Position | Length | Value | Source |
|-------|----------|--------|-------|--------|
| Record Type | 1-4 | 4 | 'C100' | Constant |
| Plan ID | 5-10 | 6 | Variable | POPP.030 |
| Unused | 11-30 | 20 | Spaces | - |
| Effective Date | 31-38 | 8 | YYYYMMDD | LastBusiness |
| Unused | 39 | 1 | Space | - |
| Trust Account | 40-71 | 32 | Variable | POPP.1510 |
| Unused | 72 | 1 | Space | - |
| Transaction Code | 73-92 | 20 | '000000000000000    2' | Constant |
| Unused | 93-114 | 22 | Spaces | - |
| Sign | 115 | 1 | '0' | Constant |
| Amount | 116-130 | 15 | Formatted | NewLoanUnits |
| Unused | 131-133 | 3 | Spaces | - |
| Activity Code | 134-138 | 5 | '00339' | Constant |

**Fixed Values**:
- **Record Type**: 'C100' (identifies cash activity record)
- **Transaction Code**: '000000000000000    2' (specific to loan offsets)
- **Sign**: '0' (sign embedded in amount field)
- **Activity Code**: '00339' (loan cash offset activity type)

**Variable Values**:
- **Plan ID**: From position record field 030 (6 characters)
- **Effective Date**: LastBusiness date (YYYYMMDD format)
- **Trust Account**: From position record field 1510 (32 characters)
- **Amount**: Negated Secondary1Buys, formatted with sign

**Implementation**:
```omniscript
OcText_Set(Line 1 'C100' 4);                              /* Record type */
OcText_Set(Line 5 RKPlan 6);                              /* Plan ID */
OcText_Set(Line 31 OcFmt(LastBusiness 'Z8') 8);          /* Effective date */
OcText_Set(Line 40 TrustAccount 32);                      /* Trust account */
OcText_Set(Line 73 '000000000000000    2' 20);           /* Transaction code */
OcText_Set(Line 115 '0' 1);                               /* Sign */
OcText_Set(Line 116 OcFmt(NewLoanUnits 'Z,12V2-') 15);   /* Amount */
OcText_Set(Line 134 '00339' 5);                           /* Activity code */
```

**Impact if Violated**:
- CRITICAL: Downstream system rejects records
- Reconciliation process fails
- Manual intervention required

---

### BR-008: Effective Date Convention
**Category**: Business Logic  
**Priority**: HIGH

**Rule Statement**:  
Use LastBusiness (last business day) as effective date in C1 records, NOT trade date

**Business Rationale**:  
- Cash reconciliation operates on business day basis
- Aligns with settlement and cash position reporting
- Consistent with other cash activity records
- Ensures proper date sequence in reconciliation

**Implementation**:
```omniscript
LastBusiness = OcDate_AddBusDays(RunDate -1);
/* ... */
OcText_Set(Line 31 OcFmt(LastBusiness 'Z8') 8);
```

**Date Relationship**:
```
TradeDate ≤ LastBusiness ≤ RunDate
```

**Example**:
- RunDate: 2024-01-15 (Monday)
- LastBusiness: 2024-01-12 (Friday) [skips weekend]
- TradeDate: 2024-01-10 (Wednesday)
- C1 Effective Date: 2024-01-12 (LastBusiness, not TradeDate)

**Reasoning**:
- Trade date represents when position occurred
- Effective date represents when cash impact is reconciled
- Cash reconciliation aggregates by business day

**Impact if Violated**:
- MEDIUM: C1 records dated incorrectly
- Reconciliation timing mismatches
- Reporting period discrepancies

---

### BR-009: Database Update Atomicity
**Category**: Data Integrity  
**Priority**: CRITICAL

**Rule Statement**:  
Update position record field 877 immediately after writing C1 record to maintain consistency

**Business Rationale**:  
- Ensures C1 record and database update are treated as atomic operation
- Prevents orphaned C1 records (written but not marked as processed)
- Enables accurate idempotency checking

**Implementation**:
```omniscript
OcFile1_Write(Line);                              /* Write C1 record */
poppobj_setde(denum:877 value:Secondary1Buys);   /* Set field 877 */
poppobj_update();                                 /* Update database */
```

**Sequence Order** (CRITICAL):
1. Build C1 record
2. Write C1 record to file
3. Update database field 877
4. Continue to next position

**Failure Scenarios**:
- **File write succeeds, DB update fails**: C1 written, but position not marked (will reprocess)
- **File write fails**: Neither C1 nor DB updated (safe)
- **DB update succeeds, file write fails**: Position marked but no C1 (data loss)

**Current Implementation Risk**:
- No explicit error handling for atomicity
- Relies on OMNISCRIPT runtime transaction handling
- See [Error Handling Documentation](./GAP_NewLoanCash_ERROR_HANDLING.md) for details

**Impact if Violated**:
- HIGH: Data inconsistency between file and database
- Duplicate C1 records or missing C1 records
- Reconciliation errors

---

### BR-010: Output File Naming Uniqueness
**Category**: File Management  
**Priority**: HIGH

**Rule Statement**:  
Each program execution must generate a uniquely named output file using date and time stamp

**Business Rationale**:  
- Prevents overwriting previous runs' output
- Enables historical audit trail
- Supports operational troubleshooting
- Allows multiple runs per day if needed

**Implementation**:
```omniscript
FileName = OcText_string(OCTEXT_GETENV('$XDAT') '\OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.' 
           OCFMT(OcDate_Current() 'Z8') '.' OcFMT(OcTime_Current() 'Z6') '.DAT');
```

**Naming Pattern**:
```
OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.{YYYYMMDD}.{HHMMSS}.DAT
```

**Components**:
- **Static Prefix**: OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET
- **Date**: YYYYMMDD (from OcDate_Current())
- **Time**: HHMMSS (from OcTime_Current())
- **Extension**: .DAT

**Uniqueness Guarantee**:
- Date + time combination ensures uniqueness (to the second)
- Multiple runs within same second would conflict (unlikely)
- Time component makes file sortable chronologically

**Impact if Violated**:
- MEDIUM: Previous run's output overwritten
- Loss of audit trail
- Difficulty troubleshooting issues

---

## Business Rule Dependencies

### Rule Interaction Matrix

| Rule | Depends On | Impacts | Priority |
|------|-----------|---------|----------|
| BR-001 | None | BR-002, BR-005 | CRITICAL |
| BR-002 | None | BR-003 | HIGH |
| BR-003 | BR-009 | BR-004 | CRITICAL |
| BR-004 | None | BR-005, BR-006 | HIGH |
| BR-005 | BR-001, BR-004 | BR-006, BR-009 | CRITICAL |
| BR-006 | BR-005 | BR-007 | CRITICAL |
| BR-007 | BR-006, BR-008 | None (output format) | CRITICAL |
| BR-008 | BR-002 | BR-007 | HIGH |
| BR-009 | BR-003, BR-007 | BR-003 | CRITICAL |
| BR-010 | None | None (operational) | HIGH |

---

## Business Rule Validation

### Validation Queries

**Validate BR-001 (Security Filter)**:
```sql
-- Should return 0 (no non-POOLLOAN3 positions processed)
SELECT COUNT(*) FROM POPP
WHERE SECURITYID <> 'POOLLOAN3'
  AND FIELD877 = FIELD741
  AND FIELD741 <> 0
  AND TRADEDATE >= {SevenDaysAgo};
```

**Validate BR-003 (Idempotency)**:
```sql
-- Compare positions updated to C1 records generated
SELECT COUNT(*) FROM POPP
WHERE SECURITYID = 'POOLLOAN3'
  AND TRADEDATE BETWEEN {SevenDaysAgo} AND {LastBusiness}
  AND FIELD877 = FIELD741
  AND FIELD741 <> 0;

-- Should match line count in output file
wc -l {OutputFile}
```

**Validate BR-005 (Reversal Netting)**:
```sql
-- Calculate net activity for a specific position
SELECT 
    PLAN,
    DATE,
    SUM(CASE WHEN FIELD009 = 'B' THEN FIELD235 ELSE 0 END) AS Total_Buys,
    SUM(CASE WHEN FIELD009 = 'S' THEN FIELD235 ELSE 0 END) AS Total_Sells,
    SUM(CASE WHEN FIELD009 = 'B' THEN FIELD235 ELSE -FIELD235 END) AS Net_Activity
FROM SSSA
WHERE SECURITYID = 'POOLLOAN3'
  AND FIELD011 = 'XI'
  AND PLAN = {specific_plan}
  AND DATE = {specific_date}
GROUP BY PLAN, DATE;

-- Compare to C1 record amount (should be negated net activity)
```

---

## Business Rule Exceptions

### Exception Handling

**Exception 1: Late-Arriving Positions**
- **Scenario**: Position arrives after 7-day window
- **Impact**: Missed by program
- **Resolution**: Manual C1 entry or extend lookback window temporarily
- **Business Owner Decision**: Required

**Exception 2: Corrected Amounts**
- **Scenario**: Field 741 value corrected after processing
- **Current Behavior**: Re-runs will skip (field 877 = old value)
- **Resolution**: Manually reset field 877 to 0 or run correction script
- **Business Owner Decision**: Required

**Exception 3: System Date Mismatch**
- **Scenario**: `$RUN-DATE` set incorrectly
- **Current Behavior**: Processes wrong date range
- **Resolution**: Falls back to current date if invalid format
- **Prevention**: Validate $RUN-DATE in batch wrapper

---

## Business Process Integration

### Upstream Processes

**Position Loading** (Prerequisite):
- Source: Position management system
- Timing: Nightly batch before GAP_NewLoanCash
- Data: POPP table populated with POOLLOAN3 positions
- Critical Fields: 741 (Secondary1Buys), 030 (RKPlan), 1510 (TrustAccount)

**Transaction Processing** (Prerequisite):
- Source: Trust transaction system
- Timing: Real-time or batch before GAP_NewLoanCash
- Data: SSSA table populated with buy/sell activity
- Critical Fields: 009 (TransactionType), 011 (Source), 235 (Amount)

### Downstream Processes

**Cash Reconciliation** (Consumer):
- Consumes: C1 activity files
- Timing: Immediately after GAP_NewLoanCash completes
- Processing: Imports C1 records, matches to asset entries
- Validation: Verifies record format, activity codes, amounts

---

## Change Impact Analysis

### Rule Change Scenarios

**Scenario 1: Change Date Lookback Window**
- **Example**: Change from 7 days to 10 days
- **Impact**: More positions included, longer processing time
- **Changes Required**: 
  - Modify line 23/26 parameter: `-7` → `-10`
  - Update documentation
  - Test with extended range
- **Business Owner Approval**: Required

**Scenario 2: Add New Transaction Type**
- **Example**: Support transaction type 'T' (Transfer)
- **Impact**: Reversal netting logic affected
- **Changes Required**:
  - Modify CHECK.SSSA routine
  - Add new conditional for type 'T'
  - Determine if adds or subtracts from net
- **Business Owner Approval**: Required

**Scenario 3: Change Activity Code**
- **Example**: Change '00339' to new code
- **Impact**: C1 record format changes
- **Changes Required**:
  - Modify line 52 constant
  - Coordinate with downstream reconciliation system
  - Parallel run to validate both systems
- **Business Owner Approval**: Required

---

*This business rules documentation provides comprehensive details on the logic and constraints governing GAP_NewLoanCash program behavior.*  
*Last Updated*: 2026-02-03
