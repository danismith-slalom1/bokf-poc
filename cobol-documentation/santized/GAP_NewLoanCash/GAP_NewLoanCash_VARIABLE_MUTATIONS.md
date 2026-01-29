# GAP_NewLoanCash Variable Mutation Analysis

## Executive Summary

This program has **7 key variables** with mutation patterns that affect program behavior:
- **1 critical mutation** (Secondary1Buys) that directly impacts C1 activity generation
- **6 one-time assignments** that establish program state
- **1 complex accumulator pattern** (WK001) used for net amount calculation
- **1 multi-field record buffer** (Line) built incrementally

---

## Global Variable Mutation Tracking

### Critical Mutation: Secondary1Buys

**Data Type**: Numeric (decimal, likely 12.2 format)  
**Scope**: Global (main loop and CHECK.SSSA routine)  
**Mutation Count**: 2 locations  
**Risk Level**: 🔴 **HIGH** - Direct impact on business logic

#### Mutation Timeline
```
1. INITIALIZATION → Read from POPP DE 741 (Line 32)
   Initial Value: Dollar amount of secondary market buys from position record
   ↓
2. CONDITIONAL MODIFICATION → CHECK.SSSA routine (Line 69)
   Condition: IF Secondary1Buys ≠ 0 (Line 34)
   Modified Value: Net amount from SSSA (buys - sells)
   ↓
3. COMPARISON → Idempotency check (Line 38)
   Compared With: PriorCashApplied
   Decision: Generate C1 activity if mismatch and non-zero
   ↓
4. USAGE → NewLoanUnits calculation (Line 41)
   Transformation: 0 - Secondary1Buys (negated)
   ↓
5. PERSISTENCE → Written to POPP DE 877 (Line 50)
   Purpose: Mark as processed for future runs
```

#### Mutation Analysis

**Location 1: Initial Read (Line 32)**
```omniscript
Secondary1Buys = poppobj_numde(741);
```
- **Source**: POPP data element 741 (User-defined field containing secondary market buy amount)
- **Trigger**: Every iteration of main loop for each POPP position record
- **Value**: Dollar amount from position database (typically positive, may be zero)
- **Business Context**: Original recorded amount of loan purchases

**Location 2: Recalculation (Line 69 - CHECK.SSSA routine)**
```omniscript
Secondary1Buys = WK001;
```
- **Source**: Calculated net amount from SSSA activity records
- **Trigger**: Only if original Secondary1Buys ≠ 0 (Line 34 condition)
- **Calculation**: WK001 = Σ(SSSA Buys) - Σ(SSSA Sells)
- **Value Range**: May be zero (full reversal), positive (net buy), or negative (over-reversal/error)
- **Business Context**: Adjusted amount after accounting for reversals and corrections

#### Mutation Impact

**Impact on Program Flow**:
1. **Line 38 Decision**: If Secondary1Buys modified by CHECK.SSSA:
   - May change from non-zero → zero (skip C1 generation)
   - May change amount (different C1 activity amount)
   - May change from processed → unprocessed state comparison

2. **Line 41 Calculation**: NewLoanUnits directly depends on final Secondary1Buys value
   - Negated value written to C1 activity record
   - If Secondary1Buys modified, NewLoanUnits reflects adjusted amount

3. **Line 50 Update**: Final Secondary1Buys value written to POPP DE 877
   - Future runs compare against this adjusted value
   - Ensures idempotency based on net amount, not original amount

**Business Significance**:
- **Critical Path Variable**: Directly determines C1 activity generation and amount
- **Data Integrity**: Ensures cash reconciliation reflects actual settled activity
- **Idempotency**: Prevents duplicate C1 records by tracking processed amounts

#### Mutation Scenarios

**Scenario 1: No SSSA Reversal Activity**
```
Initial: Secondary1Buys = 50000 (from POPP DE 741)
CHECK.SSSA: No SSSA records found OR net = 50000
Final: Secondary1Buys = 50000 (unchanged or confirmed)
Impact: C1 activity generated for $50,000
```

**Scenario 2: Partial Reversal**
```
Initial: Secondary1Buys = 50000 (from POPP DE 741)
CHECK.SSSA: SSSA shows Buy 50000 - Sell 10000 = Net 40000
Final: Secondary1Buys = 40000 (reduced)
Impact: C1 activity generated for $40,000 (not $50,000)
```

**Scenario 3: Full Reversal**
```
Initial: Secondary1Buys = 50000 (from POPP DE 741)
CHECK.SSSA: SSSA shows Buy 50000 - Sell 50000 = Net 0
Final: Secondary1Buys = 0 (zeroed)
Impact: NO C1 activity generated (line 38 condition fails)
```

**Scenario 4: Over-Reversal (Data Error)**
```
Initial: Secondary1Buys = 50000 (from POPP DE 741)
CHECK.SSSA: SSSA shows Sell 60000 (no buys found) = Net -10000
Final: Secondary1Buys = -10000 (negative)
Impact: C1 activity generated for NEGATIVE amount (data integrity issue)
```

#### Concurrency Concerns

**Risk**: If POPP record modified between read (line 32) and update (line 50)
- **Timeline**: 
  1. Read Secondary1Buys = 50000 (line 32)
  2. Another process updates POPP DE 877 = 50000
  3. This process writes DE 877 = 50000 (line 50)
  4. Result: Last write wins, may overwrite concurrent updates

**Mitigation**: Likely handled by database record locking (not visible in script)

#### Recommendations

1. **Add bounds checking**:
   ```omniscript
   If Secondary1Buys < 0;
      OcShow('WARNING: Negative Secondary1Buys: ' Secondary1Buys ' for Plan: ' RKPlan);
   End;
   ```

2. **Add mutation logging**:
   ```omniscript
   If Secondary1Buys <> poppobj_numde(741);
      OcShow('ADJUSTED: Plan ' RKPlan ' from ' poppobj_numde(741) ' to ' Secondary1Buys);
   End;
   ```

3. **Validate SSSA modification**:
   ```omniscript
   n.OriginalAmount = Secondary1Buys;
   PERFORM 'CHECK.SSSA';
   If Secondary1Buys = 0 and n.OriginalAmount <> 0;
      OcShow('INFO: Full reversal detected for Plan ' RKPlan);
   End;
   ```

---

## Other Variable Mutations

### WK001 (Local Accumulator)

**Data Type**: Numeric  
**Scope**: Local to CHECK.SSSA routine  
**Mutation Count**: Multiple (loop-based accumulation)  
**Risk Level**: 🟡 **MEDIUM** - Complex accumulation logic

#### Mutation Pattern
```
1. INITIALIZATION → Line 58
   WK001 = 0
   ↓
2. ACCUMULATION (Loop) → Lines 63, 66
   For each SSSA 'B' record: WK001 = WK001 + Amount
   For each SSSA 'S' record: WK001 = WK001 - Amount
   ↓
3. FINAL VALUE → End of loop
   WK001 = Net amount (all buys - all sells)
   ↓
4. ASSIGNMENT → Line 69
   Secondary1Buys = WK001 (transferred to global variable)
```

#### Mutation Analysis
- **Pattern**: Accumulator (sum and difference)
- **Iterations**: One per SSSA record matching plan/date/security
- **Algorithm**: 
  ```
  WK001 = 0
  FOR each SSSA record:
      IF type='B': WK001 += amount
      IF type='S': WK001 -= amount
  ENDFOR
  ```
- **Risk**: No overflow protection, assumes SSSA amounts are reasonable

#### Mutation Impact
- **Direct Impact**: Determines final Secondary1Buys value
- **Business Logic**: Correctly implements net calculation for reversals
- **Validation**: No validation that WK001 result is reasonable

---

### Line (Output Record Buffer)

**Data Type**: String (fixed-width record buffer)  
**Scope**: Main loop  
**Mutation Count**: 8 (one per field set)  
**Risk Level**: 🟢 **LOW** - Structured field assignment

#### Mutation Pattern
```
Line is built incrementally using OcText_Set():
1. Position 1-4:     'C100' (Record type)
2. Position 5-10:    RKPlan
3. Position 31-38:   LastBusiness (formatted date)
4. Position 40-71:   TrustAccount
5. Position 73-92:   '000000000000000    2'
6. Position 115:     '0'
7. Position 116-130: NewLoanUnits (formatted amount)
8. Position 134-138: '00339'
→ Written to file (Line 49)
```

#### Mutation Analysis
- **Pattern**: Incremental field assignment (write-once per iteration)
- **Format**: Fixed-width positional record
- **Risk**: No validation of field lengths, potential buffer overflow if fields exceed expected size

---

### FileName

**Data Type**: String  
**Mutation Count**: 1 (one-time construction)  
**Risk Level**: 🟢 **LOW**

#### Mutation Timeline
```
1. CONSTRUCTION → Lines 13-14
   FileName = OcText_string(OCTEXT_GETENV('$XDAT') '\OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.' 
              OCFMT(OcDate_Current() 'Z8') '.' OcFMT(OcTime_Current() 'Z6') '.DAT')
   ↓
2. USAGE → Line 16
   OcFile1_Open(name:FileName mode:'OUTPUT')
   ↓
3. NO FURTHER MODIFICATION
```

**Mutation Type**: One-time initialization  
**Business Logic**: Unique timestamped filename for each run  
**Risk**: Low - read-only after construction

---

### RunDate, SevenDaysAgo, LastBusiness

**Data Type**: Numeric (Date - YYYYMMDD)  
**Mutation Count**: 1 each (one-time calculation)  
**Risk Level**: 🟢 **LOW**

#### Mutation Timeline
```
RunDate:
1. READ → Line 18: octext_tonum(octext_getenv('$RUN-DATE'))
2. VALIDATE → Line 19: if OcDate_Valid(RunDate)
3. FALLBACK (if invalid) → Line 23: OcDate_Current()
4. NO FURTHER MODIFICATION

SevenDaysAgo:
1. CALCULATE → Line 20 or 23: OcDate_AddDays(RunDate, -7) OR OcDate_AddDays(OcDate_Current(), -7)
2. NO FURTHER MODIFICATION

LastBusiness:
1. CALCULATE → Line 21 or 24: OcDate_AddBusDays(RunDate, -1) OR OcDate_AddBusDays(OcDate_Current(), -1)
2. NO FURTHER MODIFICATION
```

**Mutation Type**: One-time initialization with conditional logic  
**Business Logic**: Establish date range for processing  
**Risk**: Low - dates are read-only after calculation

---

### RKPlan, TradeDate, TrustAccount, PriorCashApplied

**Mutation Count**: Read multiple times per loop iteration (redundant reads)  
**Risk Level**: 🟢 **LOW**

**Pattern**: Read from POPP object, not modified

**Note**: RKPlan and TradeDate are redundantly read twice:
- First read: Lines 30-31
- Second read: Lines 39-40 (within condition)

**Recommendation**: Eliminate redundant reads for efficiency

---

## Variable Lifecycle Diagrams

### Secondary1Buys Lifecycle
```
┌─────────────────────────────────────────────────────────┐
│ INITIALIZATION PHASE                                     │
│ Read from POPP DE 741                                   │
│ Line 32: Secondary1Buys = poppobj_numde(741)           │
│ Value: Original amount from position record             │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ CONDITIONAL CHECK                                       │
│ Line 34: IF Secondary1Buys <> 0                        │
│ Decision: Should we check SSSA for reversals?         │
└────────┬────────────────────────────────────┬──────────┘
         │ YES (non-zero)          NO (zero)  │
         ▼                                     ▼
┌─────────────────────────┐        ┌──────────────────────┐
│ RECALCULATION PHASE     │        │ SKIP SSSA            │
│ PERFORM CHECK.SSSA      │        │ Use original value   │
│ Lines 57-70             │        └──────────────────────┘
│                         │                   │
│ Query SSSA for net amt  │                   │
│ WK001 = ΣBuys - ΣSells │                   │
│ Secondary1Buys = WK001  │                   │
└────────┬────────────────┘                   │
         │                                     │
         └──────────┬──────────────────────────┘
                    ▼
┌─────────────────────────────────────────────────────────┐
│ IDEMPOTENCY CHECK                                       │
│ Line 38: IF (PriorCashApplied <> Secondary1Buys)       │
│          AND (Secondary1Buys <> 0)                     │
│ Decision: Has this amount already been processed?      │
└────────┬───────────────────────────────────┬───────────┘
         │ YES (new/changed)     NO (match)  │
         ▼                                    ▼
┌─────────────────────────┐        ┌──────────────────────┐
│ USAGE PHASE             │        │ SKIP PROCESSING      │
│ Line 41: NewLoanUnits   │        │ Already processed    │
│ = 0 - Secondary1Buys    │        │ No C1 generation     │
│                         │        └──────────────────────┘
│ Lines 43-48: Build Line │
│ Line 49: Write to file  │
│                         │
│ Line 50: Update POPP    │
│ DE 877 = Secondary1Buys │
└─────────────────────────┘
```

### WK001 Accumulation Lifecycle
```
┌─────────────────────────────────────────────────────────┐
│ INITIALIZATION                                          │
│ Line 58: WK001 = 0                                     │
│ Value: Zero (reset accumulator)                        │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ SSSA QUERY                                              │
│ Line 58: sssaobj_view(PLAN:RKPlan SECURITYID:...       │
│ Result: Cursor positioned at first matching record     │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ ACCUMULATION LOOP (Lines 59-67)                        │
│                                                         │
│ For each SSSA record:                                  │
│   ┌─────────────────────────────────────────┐         │
│   │ IF DE011 = 'XI' (transaction indicator) │         │
│   └────────┬──────────────────────┬─────────┘         │
│            │ YES                   │ NO                │
│            ▼                       ▼                   │
│   ┌────────────────┐    ┌──────────────────┐          │
│   │ IF DE009 = 'B' │    │ Skip (ignore     │          │
│   │ (Buy)          │    │ non-XI records)  │          │
│   └───┬────────────┘    └──────────────────┘          │
│       │ YES                                            │
│       ▼                                                │
│   Line 63:                                             │
│   WK001 = WK001 + sssaobj_numde(235)                  │
│   [Add buy amount]                                     │
│       │                                                │
│   ┌───▼────────────┐                                  │
│   │ IF DE009 = 'S' │                                  │
│   │ (Sell/Reversal)│                                  │
│   └───┬────────────┘                                  │
│       │ YES                                            │
│       ▼                                                │
│   Line 66:                                             │
│   WK001 = WK001 - sssaobj_numde(235)                  │
│   [Subtract sell amount]                               │
│       │                                                │
│       ▼                                                │
│   Continue to next SSSA record...                      │
│                                                         │
└────────────────┬────────────────────────────────────────┘
                 │ (Loop complete)
                 ▼
┌─────────────────────────────────────────────────────────┐
│ TRANSFER PHASE                                          │
│ Line 69: Secondary1Buys = WK001                        │
│ Result: Global variable updated with net amount        │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ RETURN TO CALLER                                        │
│ Line 70: GOBACK                                        │
│ State: Secondary1Buys now contains adjusted value      │
└─────────────────────────────────────────────────────────┘
```

---

## Mutation Summary Table

| Variable | Initial Value | Mutation Locations | Final Value | Mutation Type | Risk |
|----------|--------------|-------------------|-------------|---------------|------|
| **Secondary1Buys** | POPP DE 741 | Lines 32, 69 | Net SSSA amount (if non-zero) | **Conditional mutation** | 🔴 HIGH |
| WK001 | 0 | Lines 58, 63, 66 | Net amount (buys - sells) | Accumulator | 🟡 MEDIUM |
| Line | (uninitialized) | Lines 43-48 | Fixed-width record | Incremental build | 🟢 LOW |
| FileName | (constructed) | Lines 13-14 | Timestamped path | One-time construction | 🟢 LOW |
| RunDate | ENV or current | Lines 18, 23 | Valid run date | Conditional initialization | 🟢 LOW |
| SevenDaysAgo | (calculated) | Lines 20, 23 | RunDate - 7 days | One-time calculation | 🟢 LOW |
| LastBusiness | (calculated) | Lines 21, 24 | Prior business day | One-time calculation | 🟢 LOW |
| RKPlan | POPP DE 030 | Lines 30, 39 | Plan ID (read-only) | Multiple reads (redundant) | 🟢 LOW |
| TradeDate | POPP DE 008 | Lines 31, 40 | Trade date (read-only) | Multiple reads (redundant) | 🟢 LOW |
| NewLoanUnits | (calculated) | Line 41 | 0 - Secondary1Buys | One-time calculation | 🟢 LOW |
| TrustAccount | POPP DE 01510 | Line 42 | Account ID (read-only) | Single read | 🟢 LOW |
| PriorCashApplied | POPP DE 877 | Line 33 | Previous amount (read-only) | Single read | 🟢 LOW |

---

## Mutation Risks and Mitigation

### High-Risk Mutations

**Secondary1Buys**:
- **Risk**: Modified in subroutine, affects multiple downstream operations
- **Impact**: Incorrect value leads to wrong C1 activity amount or duplicate records
- **Mitigation**: 
  - Add validation after CHECK.SSSA
  - Log when value changes significantly
  - Add bounds checking for negative or extreme values

### Medium-Risk Mutations

**WK001**:
- **Risk**: Complex accumulation logic, no overflow protection
- **Impact**: Incorrect net calculation leads to wrong Secondary1Buys
- **Mitigation**:
  - Add overflow checking
  - Validate that net amount is reasonable
  - Log detailed SSSA processing for audit trail

### Low-Risk Mutations

All other variables have simple, one-time assignment patterns with minimal risk.

---

## Recommendations

1. **Add Mutation Logging**: Log when Secondary1Buys changes
   ```omniscript
   n.OriginalAmt = Secondary1Buys;
   PERFORM 'CHECK.SSSA';
   If Secondary1Buys <> n.OriginalAmt;
      OcShow('MUTATION: Plan ' RKPlan ' adjusted from ' n.OriginalAmt ' to ' Secondary1Buys);
   End;
   ```

2. **Validate Mutations**: Check that mutated values are reasonable
   ```omniscript
   If Secondary1Buys < -999999 or Secondary1Buys > 999999999;
      OcShow('ERROR: Extreme Secondary1Buys value: ' Secondary1Buys);
   End;
   ```

3. **Eliminate Redundant Reads**: Remove duplicate reads of RKPlan and TradeDate (lines 39-40)

4. **Add Accumulator Safety**: Protect WK001 from overflow
   ```omniscript
   If WK001 > 999999999;
      OcShow('WARNING: WK001 accumulator approaching limit');
   End;
   ```

---

*AI-Generated Documentation - Review with OmniScript/COBOL experts for accuracy*
