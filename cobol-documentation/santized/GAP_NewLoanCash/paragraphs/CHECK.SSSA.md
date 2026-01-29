# CHECK.SSSA Routine

**Routine Name**: CHECK.SSSA  
**Type**: Subroutine  
**Location**: Lines 54-70  
**Called By**: Main Processing Loop (Line 35)

---

## Purpose and Function

This routine recalculates the Secondary1Buys amount by querying the SSSA (Security Secondary Settlement Activity) database to account for reversal transactions and net buy/sell activity. It ensures that C1 cash activity records reflect the true net loan purchase amount after accounting for any reversals or corrections.

---

## Business Logic

### High-Level Flow
1. Query SSSA database for all activity records matching:
   - Same Plan (RKPlan)
   - Same Security (POOLLOAN3)
   - Same Trade Date
2. For each SSSA activity record:
   - If it's a BUY transaction (code 'B') with indicator 'XI' → ADD amount to accumulator
   - If it's a SELL/REVERSAL transaction (code 'S') with indicator 'XI' → SUBTRACT amount from accumulator
3. Replace the original Secondary1Buys with the net calculated amount

### Business Purpose
- **Reversal Recognition**: Loan transactions may be reversed or corrected in the secondary settlement system
- **Net Activity Calculation**: Provides accurate net loan purchase amount by netting buys against sells
- **Data Integrity**: Ensures C1 activity matches actual settled activity, not just initial position data

### Business Context
- **SSSA System**: Secondary Settlement System Activity - tracks all trade corrections and reversals
- **XI Indicator**: Specific transaction indicator for loan pool settlements
- **Buy vs Sell**: 
  - 'B' = Original buy transaction (adds to position)
  - 'S' = Reversal/sell transaction (reduces position)

---

## Step-by-Step Processing Logic

### Step 1: Initialize Accumulator (Line 58)
```omniscript
WK001 = 0;
```
- **Purpose**: Reset working variable to zero before accumulation
- **Scope**: Local to CHECK.SSSA routine
- **Type**: Numeric accumulator for net dollar amount

### Step 2: Validate Input Parameters (Line 57)
```omniscript
if (RKPlan <> '') and (TradeDate <> 0);
```
- **Condition 1**: RKPlan not empty string (valid plan identifier)
- **Condition 2**: TradeDate not zero (valid trade date)
- **Purpose**: Prevent invalid SSSA query if required parameters missing
- **Fail Action**: If validation fails, skip SSSA query and return (Secondary1Buys unchanged)

**Business Rule**: Only query SSSA if we have valid plan and trade date to look up

### Step 3: Query SSSA Activity Records (Line 58)
```omniscript
sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
```
- **Query Parameters**:
  - PLAN: Specific retirement plan account (e.g., '123456')
  - SECURITYID: Fixed value 'POOLLOAN3' (loan pool security)
  - DATE: Exact trade date from POPP position
- **Result**: Cursor positioned at first matching SSSA record (if any)
- **Performance Note**: May return multiple records if plan had multiple transactions on same date

### Step 4: Iterate Through SSSA Records (Line 59)
```omniscript
loop while sssaobj_next();
```
- **Action**: Fetch next SSSA activity record from result set
- **Termination**: Loop ends when no more matching records
- **Typical Volume**: 1-10 records per plan/date (could be more with heavy reversal activity)

### Step 5: Check Transaction Indicator (Line 60)
```omniscript
if sssaobj_de(011) = 'XI';
```
- **Field**: SSSA Data Element 011 (Transaction Indicator)
- **Filter Value**: 'XI' - Identifies loan pool settlement activity
- **Purpose**: Only process specific transaction types, ignore other activity codes
- **Other Values**: May include 'XO', 'TI', etc. (not documented, excluded from calculation)

**Business Rule**: Only 'XI' indicator transactions affect loan cash reconciliation

### Step 6a: Process BUY Transactions (Lines 61-63)
```omniscript
if sssaobj_de(009) = 'B';
   WK001 = WK001 + sssaobj_numde(235);
end;
```
- **Condition**: Data Element 009 = 'B' (Buy transaction)
- **Action**: Add transaction amount (DE 235) to accumulator
- **Business Meaning**: Original buy transaction increases net loan purchases
- **Amount Field**: DE 235 contains dollar amount of the transaction

**Example**: 
- SSSA Record: Plan=123456, Date=20231221, DE009='B', DE235=50000.00
- Effect: WK001 = WK001 + 50000.00 (increases net buys)

### Step 6b: Process SELL/REVERSAL Transactions (Lines 64-66)
```omniscript
if sssaobj_de(009) = 'S';
   WK001 = WK001 - sssaobj_numde(235);
end;
```
- **Condition**: Data Element 009 = 'S' (Sell/Reversal transaction)
- **Action**: Subtract transaction amount (DE 235) from accumulator
- **Business Meaning**: Reversal transaction reduces net loan purchases
- **Impact**: Adjusts for cancelled or corrected trades

**Example**: 
- SSSA Record: Plan=123456, Date=20231221, DE009='S', DE235=10000.00
- Effect: WK001 = WK001 - 10000.00 (decreases net buys)

**Business Context**: Sells in loan pools typically represent:
- Trade reversals (original buy was cancelled)
- Trade corrections (buy amount was incorrect)
- Actual loan sell-backs (less common)

### Step 7: Continue Loop (Line 67)
```omniscript
endloop;
```
- **Action**: Return to Step 4, fetch next SSSA record
- **Net Calculation**: WK001 now contains sum of all buys minus sum of all sells
- **Result**: Net loan purchase amount for this plan/date/security

### Step 8: Update Secondary1Buys (Line 69)
```omniscript
Secondary1Buys = WK001;
```
- **Action**: Replace original Secondary1Buys value with calculated net amount
- **Impact**: Main loop now uses corrected amount for:
  - Comparison with PriorCashApplied (idempotency check)
  - NewLoanUnits calculation
  - C1 activity amount
  - POPP DE 877 update value

**CRITICAL MUTATION**: This is the only place Secondary1Buys is modified after initial read from POPP

### Step 9: Return to Caller (Line 70)
```omniscript
GOBACK;
```
- **Action**: Return control to main processing loop (line 35)
- **State**: Secondary1Buys now contains net amount, all other variables unchanged
- **Next Step**: Main loop continues with line 38 (idempotency check)

---

## Variables Used

### Input Variables (Read-Only)
| Variable | Source | Purpose |
|----------|--------|---------|
| RKPlan | POPP DE 030 (read in main loop) | Plan identifier for SSSA query |
| TradeDate | POPP DE 008 (read in main loop) | Trade date for SSSA query |

### Output Variables (Modified)
| Variable | Original Source | Final Value | Purpose |
|----------|----------------|-------------|---------|
| Secondary1Buys | POPP DE 741 | Net SSSA amount (WK001) | Replaced with calculated net buys |

### Working Variables (Local)
| Variable | Type | Purpose |
|----------|------|---------|
| WK001 | Numeric accumulator | Temporary storage for net amount calculation |

### SSSA Data Elements Referenced
| Data Element | Purpose |
|--------------|---------|
| DE 009 | Transaction type ('B' = Buy, 'S' = Sell/Reversal) |
| DE 011 | Transaction indicator ('XI' = Loan settlement activity) |
| DE 235 | Transaction dollar amount |

---

## Error Conditions and Special Handling

### Expected Error Conditions

1. **Empty RKPlan or Zero TradeDate**
   - **Detection**: Line 57 validation check
   - **Handling**: Skip SSSA query, return without modifying Secondary1Buys
   - **Impact**: Uses original POPP value (may be incorrect if reversals exist)

2. **No SSSA Records Found**
   - **Handling**: Loop executes zero times, WK001 remains 0
   - **Impact**: Secondary1Buys set to 0 (line 69)
   - **Risk**: May incorrectly zero out valid loan activity if SSSA query fails

3. **SSSA Query Failure**
   - **Handling**: NONE - no error checking on sssaobj_view()
   - **Impact**: Unpredictable - may return zero records, may cause program failure
   - **Risk**: HIGH - silent data corruption possible

4. **Mixed Buy/Sell Transactions**
   - **Handling**: Correctly nets buys and sells (implemented 09/25/2024)
   - **Example**: 
     - Buy $50,000 (WK001 = +50000)
     - Sell $10,000 (WK001 = 50000 - 10000 = 40000)
     - Net = $40,000
   - **Impact**: Correct net amount calculated

5. **Net Result is Zero**
   - **Scenario**: All buys exactly offset by reversals
   - **Handling**: Secondary1Buys = 0
   - **Impact**: Main loop skips C1 activity generation (line 38 condition fails)
   - **Correctness**: CORRECT - no net activity should generate no C1 activity

6. **Net Result is Negative**
   - **Scenario**: More reversals than original buys (data error or timing issue)
   - **Handling**: Secondary1Buys = negative value
   - **Impact**: Main loop may process (line 38 condition: `Secondary1Buys <> 0`)
   - **Risk**: MEDIUM - negative loan buys would generate incorrect C1 activity

### Special Cases

**Transaction Indicator Filtering**:
- Only 'XI' transactions processed (line 60)
- Other indicators (if present) are ignored
- **Assumption**: Only 'XI' affects loan cash reconciliation
- **Risk**: If other indicators should be included, logic is incomplete

**Transaction Type Coverage**:
- Only 'B' and 'S' types processed (lines 61, 64)
- Other types (if exist) are ignored
- **Assumption**: Only buys and sells exist for loan settlements
- **Risk**: If other transaction types should be included (e.g., 'C' for corrections), logic is incomplete

---

## Performance Considerations

### Database Queries
- **SSSA Query**: Executed once per POPP record with non-zero Secondary1Buys
- **Query Complexity**: Three-key lookup (Plan + Security + Date)
  - **Assumption**: Indexed for good performance
  - **Typical Volume**: 1-10 records per query
  - **Worst Case**: If not indexed, full table scan per POPP record

### Nested Loop Performance
```
Main Loop: N POPP records
  → For each record with Secondary1Buys > 0:
      CHECK.SSSA: M SSSA records
```
- **Complexity**: O(N * M) in worst case
- **Typical**: N = 100s, M = 1-10 → Acceptable
- **Worst Case**: N = 1000s, M = 100s → May be slow (10,000+ SSSA queries)

### Optimization Opportunities
1. **Batch SSSA Queries**: Collect all (Plan, Date) pairs, query SSSA once
2. **Add Date Range Filter**: SSSA query could filter by date range (currently exact match)
3. **Cache SSSA Results**: If multiple POPP records share same Plan/Date, reuse results

---

## Business Rules Summary

1. **Only Process 'XI' Indicator**: Transaction type filter ensures only loan settlement activity included
2. **Net Buys and Sells**: Accurately calculate net amount by summing buys and subtracting sells
3. **Validation Required**: Must have valid RKPlan and TradeDate to query SSSA
4. **Zero Result Handling**: If net amount is zero (all reversals), set Secondary1Buys to zero
5. **Overwrite Original Value**: SSSA calculation completely replaces POPP DE 741 value

---

## Algorithm Examples

### Example 1: Simple Buy (No Reversals)
**POPP Data**:
- RKPlan = '123456'
- TradeDate = 20231221
- Secondary1Buys = 50000.00 (DE 741)

**SSSA Records**:
- Record 1: DE009='B', DE011='XI', DE235=50000.00

**Calculation**:
- WK001 = 0
- WK001 = 0 + 50000.00 = 50000.00 (Buy)
- Secondary1Buys = 50000.00

**Result**: No change (SSSA confirms POPP amount)

---

### Example 2: Buy with Partial Reversal
**POPP Data**:
- RKPlan = '123456'
- TradeDate = 20231221
- Secondary1Buys = 50000.00 (DE 741)

**SSSA Records**:
- Record 1: DE009='B', DE011='XI', DE235=50000.00 (Original buy)
- Record 2: DE009='S', DE011='XI', DE235=10000.00 (Partial reversal)

**Calculation**:
- WK001 = 0
- WK001 = 0 + 50000.00 = 50000.00 (Buy)
- WK001 = 50000.00 - 10000.00 = 40000.00 (Reversal)
- Secondary1Buys = 40000.00

**Result**: Net loan purchase reduced from $50K to $40K

---

### Example 3: Buy with Full Reversal
**POPP Data**:
- RKPlan = '123456'
- TradeDate = 20231221
- Secondary1Buys = 50000.00 (DE 741)

**SSSA Records**:
- Record 1: DE009='B', DE011='XI', DE235=50000.00 (Original buy)
- Record 2: DE009='S', DE011='XI', DE235=50000.00 (Full reversal)

**Calculation**:
- WK001 = 0
- WK001 = 0 + 50000.00 = 50000.00 (Buy)
- WK001 = 50000.00 - 50000.00 = 0.00 (Full reversal)
- Secondary1Buys = 0.00

**Result**: Net loan purchase is zero, main loop skips C1 activity generation

---

### Example 4: Multiple Buys and Sells
**POPP Data**:
- RKPlan = '789012'
- TradeDate = 20231221
- Secondary1Buys = 100000.00 (DE 741)

**SSSA Records**:
- Record 1: DE009='B', DE011='XI', DE235=50000.00
- Record 2: DE009='B', DE011='XI', DE235=30000.00
- Record 3: DE009='B', DE011='XI', DE235=20000.00
- Record 4: DE009='S', DE011='XI', DE235=15000.00 (Reversal)

**Calculation**:
- WK001 = 0
- WK001 = 0 + 50000 = 50000 (Buy #1)
- WK001 = 50000 + 30000 = 80000 (Buy #2)
- WK001 = 80000 + 20000 = 100000 (Buy #3)
- WK001 = 100000 - 15000 = 85000 (Reversal)
- Secondary1Buys = 85000.00

**Result**: Net loan purchase is $85K (down from $100K after reversal)

---

## Dependencies

### Called By
| Caller | Location | Condition |
|--------|----------|-----------|
| Main Processing Loop | Line 35 | Only if Secondary1Buys ≠ 0 |

### Calls To
- None (does not call other routines)

### Database Dependencies
- **SSSA Database**: Requires read access to SSSA view/table
- **Required Fields**: DE 009, DE 011, DE 235

### Data Dependencies
- **Input**: RKPlan (from POPP DE 030)
- **Input**: TradeDate (from POPP DE 008)
- **Output**: Secondary1Buys (modified in place)

---

## Change History Impacts

### 09/25/2024 Enhancement
**Description**: "Recognize Loan Reversal Activity and Net Activity Correctly when there are BUYS and SELLS"

**Changes Made**:
- Added SELL transaction handling (lines 64-66)
- Proper netting of buys and sells

**Previous Behavior** (Assumed):
- Likely only processed 'B' transactions
- May have incorrectly handled reversals

**Current Behavior**:
- Correctly nets buys and sells
- Accurately reflects net settled activity

**Business Impact**:
- More accurate C1 cash activity amounts
- Prevents over-reporting of loan purchases
- Aligns cash reconciliation with actual settlements

---

## Testing Recommendations

### Unit Test Cases
1. **Test: No SSSA Records Found**
   - Input: Valid RKPlan/TradeDate, no SSSA matches
   - Expected: Secondary1Buys = 0
   - Verify: May incorrectly zero valid amount (test if query failure vs no records)

2. **Test: Single Buy, No Reversals**
   - Input: One 'XI'/'B' SSSA record
   - Expected: Secondary1Buys = SSSA amount
   - Verify: Amount matches SSSA DE 235

3. **Test: Buy with Partial Reversal**
   - Input: One 'B' for $50K, one 'S' for $10K
   - Expected: Secondary1Buys = $40K
   - Verify: Correct netting

4. **Test: Buy with Full Reversal**
   - Input: One 'B' for $50K, one 'S' for $50K
   - Expected: Secondary1Buys = $0
   - Verify: Main loop skips C1 generation

5. **Test: Empty RKPlan**
   - Input: RKPlan = ''
   - Expected: Skip SSSA query, Secondary1Buys unchanged
   - Verify: No database query executed

6. **Test: Zero TradeDate**
   - Input: TradeDate = 0
   - Expected: Skip SSSA query, Secondary1Buys unchanged
   - Verify: No database query executed

7. **Test: Non-XI Transactions**
   - Input: SSSA records with DE011 ≠ 'XI'
   - Expected: Ignored, not included in net
   - Verify: Only 'XI' transactions processed

8. **Test: Multiple Buys and Sells**
   - Input: Complex mix of B and S transactions
   - Expected: Correct net amount
   - Verify: (Sum of Buys) - (Sum of Sells)

---

## Related Documentation
- [Main Processing Loop Documentation](./MAIN_PROCESSING_LOOP.md)
- [Data Dictionary](../GAP_NewLoanCash_DATA_DICTIONARY.md) - WK001 variable definition
- [Error Handling Analysis](../GAP_NewLoanCash_ERROR_HANDLING.md) - Missing SSSA error checks

---

*AI-Generated Documentation - Review with OmniScript/COBOL experts for accuracy. Verify SSSA data element meanings and transaction codes with system documentation.*
