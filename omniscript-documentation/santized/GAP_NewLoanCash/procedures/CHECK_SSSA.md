# CHECK.SSSA Procedure Documentation

## Overview
- **Procedure Name**: CHECK.SSSA
- **Type**: Subroutine/Routine
- **Lines**: 55-69
- **Purpose**: Validates and recalculates Secondary1Buys amount by analyzing SSSA (Security Settlement Activity) records to account for buy/sell and reversal activity

---

## Business Purpose

This procedure ensures accurate net loan activity by:
1. Querying SSSA records for the specific plan, security, and trade date
2. Identifying all 'XI' transaction type activity
3. Netting buy transactions against sell/reversal transactions
4. Producing the true net loan amount accounting for any reversals

**Critical Business Context**: Without this check, loan reversals (sells) would not be properly netted against buys, leading to incorrect cash reconciliation amounts.

---

## Input Requirements

### Expected State
- **RKPlan**: Must be non-empty string (validated on line 56)
- **TradeDate**: Must be non-zero numeric date (validated on line 56)
- **Secondary1Buys**: Initial value from POPP field 741 (caller responsibility)

### Preconditions
- Called from main processing loop after retrieving POPP record
- Only invoked when `Secondary1Buys <> 0` (line 33-34)
- SSSA database must be accessible and queryable

---

## Processing Logic

### Step 1: Input Validation (Line 56)
```omniscript
if (RKPlan <> '') and (TradeDate <> 0);
```
- Validates that both required query parameters are present
- If either is empty/zero, procedure returns without processing (implicit return via if block)
- **Business Logic**: Prevents invalid SSSA queries that would return no results or cause errors

### Step 2: Initialize Accumulator (Line 56)
```omniscript
WK001 = 0;
```
- Resets working variable to accumulate net activity
- **Importance**: Ensures clean state for each invocation

### Step 3: Query SSSA Records (Line 57)
```omniscript
sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
```
- Queries SSSA (Security Settlement Activity) database
- **Filter Criteria**:
  - `PLAN`: Matches the specific retirement plan (RKPlan)
  - `SECURITYID`: Fixed to 'POOLLOAN3' (loan pool security)
  - `DATE`: Matches the specific trade date
- Returns all settlement activity records matching these criteria

### Step 4: Process SSSA Records Loop (Lines 58-66)
```omniscript
loop while sssaobj_next();
   if sssaobj_de(011) = 'XI';
      if sssaobj_de(009) = 'B';
         WK001 = WK001 + sssaobj_numde(235);
      end;
      if sssaobj_de(009) = 'S';
         WK001 = WK001 - sssaobj_numde(235);
      end; 
   end;
endloop;
```

#### Sub-step 4a: Filter for 'XI' Activity Type (Line 59)
- Checks if activity code (field 011) equals 'XI'
- **Business Logic**: Only certain activity types are relevant for loan cash reconciliation
- Non-'XI' records are ignored

#### Sub-step 4b: Process Buy Transactions (Lines 60-62)
```omniscript
if sssaobj_de(009) = 'B';
   WK001 = WK001 + sssaobj_numde(235);
end;
```
- Checks if transaction type (field 009) is 'B' (Buy)
- Adds the transaction amount (field 235) to the accumulator
- **Business Logic**: Buy transactions increase the net loan amount (positive cash impact)

#### Sub-step 4c: Process Sell/Reversal Transactions (Lines 63-65)
```omniscript
if sssaobj_de(009) = 'S';
   WK001 = WK001 - sssaobj_numde(235);
end;
```
- Checks if transaction type (field 009) is 'S' (Sell/Reversal)
- Subtracts the transaction amount (field 235) from the accumulator
- **Business Logic**: Sell transactions reverse loan amounts (negative cash impact)
- **Critical**: This handles loan reversal activity correctly (enhancement 09/25/2024)

### Step 5: Update Secondary1Buys (Line 68)
```omniscript
Secondary1Buys = WK001;
```
- Assigns the calculated net amount back to Secondary1Buys
- **Effect**: Overwrites the initial POPP field 741 value with the SSSA-verified amount
- **Business Logic**: SSSA is the authoritative source for actual net loan activity

### Step 6: Return to Caller (Line 69)
```omniscript
GOBACK;
```
- Returns control to the calling code (main processing loop)
- Updated `Secondary1Buys` value is now available to caller

---

## Output and Side Effects

### Variables Modified
- **Secondary1Buys**: Recalculated to reflect net buy/sell activity from SSSA
- **WK001**: Local working variable (not visible to caller)

### Database Operations
- **Read**: SSSA records (via sssaobj_view and sssaobj_next)
- **Write**: None (read-only operation)

### Return Values
- No explicit return value
- Updated `Secondary1Buys` variable serves as implicit output

---

## Error Handling

### Explicit Error Handling
- **Input Validation**: Checks for empty RKPlan or zero TradeDate (line 56)
- **Handling**: Silently returns without processing if validation fails

### Missing Error Handling
- **No check for SSSA query failures**: If database is unavailable or query fails, behavior is undefined
- **No check for field 235 data validity**: Assumes all amounts are valid numeric values
- **No overflow protection**: Large accumulation in WK001 could exceed numeric limits (unlikely in practice)
- **Risk Level**: **Medium** - Database errors could cause program termination or incorrect results

### Recommended Improvements
1. Add explicit error handling for SSSA database operations
2. Validate field 235 values before accumulation
3. Log warning if no SSSA records found when Secondary1Buys > 0 (potential data inconsistency)

---

## Performance Characteristics

### Database Queries
- **Query Count**: 1 per invocation (sssaobj_view)
- **Query Efficiency**: Indexed query on PLAN + SECURITYID + DATE (likely efficient)
- **Record Iteration**: Processes all matching SSSA records in loop

### Computational Complexity
- **Time Complexity**: O(n) where n = number of matching SSSA records
- **Typical n**: Small (1-10 records per plan/date combination)
- **Bottleneck**: Database query, not computation

### Optimization Opportunities
- Could cache SSSA query results if same plan/date queried multiple times
- Consider database-side aggregation (SUM) if OmniScript supports it

---

## Edge Cases and Boundary Conditions

### Edge Case 1: No SSSA Records Found
- **Scenario**: POPP shows Secondary1Buys > 0, but no matching SSSA records
- **Behavior**: WK001 remains 0, Secondary1Buys set to 0
- **Business Impact**: C1 record would show zero amount (likely incorrect)
- **Mitigation**: Consider logging warning if initial Secondary1Buys > 0 but result is 0

### Edge Case 2: Only Sell Transactions (Full Reversal)
- **Scenario**: All SSSA records are 'S' (sell/reversal)
- **Behavior**: WK001 becomes negative, Secondary1Buys becomes negative
- **Business Impact**: Negative loan amount (valid for reversals)
- **Handling**: Correctly handled - caller logic will process negative amounts appropriately

### Edge Case 3: Mixed Buy and Sell Transactions
- **Scenario**: Multiple 'B' and 'S' transactions for same plan/date
- **Behavior**: Net amount calculated correctly (buys - sells)
- **Business Impact**: Accurate net loan activity
- **Handling**: Primary purpose of this routine (added 09/25/2024)

### Edge Case 4: Non-'XI' Activity Types
- **Scenario**: SSSA contains records with activity code != 'XI'
- **Behavior**: Records are skipped (line 59 condition)
- **Business Impact**: Only relevant activity types processed
- **Handling**: Correct filtering behavior

### Edge Case 5: Empty RKPlan or Zero TradeDate
- **Scenario**: Caller invokes with invalid parameters
- **Behavior**: Procedure returns immediately without processing
- **Business Impact**: Secondary1Buys remains at initial POPP value
- **Handling**: Safeguards against invalid queries

---

## Security Considerations

### Input Validation
- **RKPlan**: Basic null/empty check (line 56)
  - **Risk**: No sanitization for SQL injection (if SSSA query uses SQL backend)
  - **Mitigation**: Assumes OmniScript framework handles query parameterization
- **TradeDate**: Zero check only
  - **Risk**: No validation for date range or format
  - **Mitigation**: Assumes caller provides valid dates

### Resource Limits
- **Query Result Size**: No limit on number of SSSA records returned
  - **Risk**: Extremely large result sets could cause memory issues
  - **Mitigation**: Business logic suggests small result sets (plan/date specific)

---

## Dependencies

### Database Objects
- **SSSA Database (sssaobj)**: Must be accessible and contain current data
  - **Fields Used**: 009 (transaction type), 011 (activity code), 235 (amount)
  - **Query Method**: sssaobj_view() and sssaobj_next()

### Called Procedures
- None (leaf procedure)

### OmniScript Functions
- `sssaobj_view()`: Database view/query function
- `sssaobj_next()`: Iterator for query results
- `sssaobj_de()`: Retrieve string field from current record
- `sssaobj_numde()`: Retrieve numeric field from current record

---

## Call Relationships

### Called By
- Main processing loop (line 34: `PERFORM 'CHECK.SSSA'`)
- **Invocation Condition**: When `Secondary1Buys <> 0` in POPP record

### Calls
- None (no sub-procedures)

---

## Business Rules Implemented

1. **Only 'XI' activity types are relevant** for loan cash reconciliation
2. **Buy transactions ('B') increase net loan amounts** (positive cash impact)
3. **Sell transactions ('S') decrease net loan amounts** (reversal activity)
4. **Net loan amount = Sum of Buys - Sum of Sells** for the specific plan/security/date
5. **SSSA is the authoritative source** for actual settlement activity (overrides POPP field 741)

---

## Historical Changes

### 09/25/2024 - Gary Matten
- **Enhancement**: Recognize Loan Reversal Activity and Net Activity Correctly when there are BUYS and SELLS
- **Implementation**: Added sell transaction handling (lines 63-65)
- **Impact**: Procedure now correctly nets buy and sell activity instead of only processing buys

---

## Testing Recommendations

### Test Case 1: Normal Buy Activity
- **Setup**: SSSA has 1 'XI'/'B' record with amount 10000
- **Expected**: Secondary1Buys = 10000

### Test Case 2: Normal Sell Activity (Reversal)
- **Setup**: SSSA has 1 'XI'/'S' record with amount 10000
- **Expected**: Secondary1Buys = -10000

### Test Case 3: Mixed Buy and Sell
- **Setup**: SSSA has 'XI'/'B' 15000 and 'XI'/'S' 5000
- **Expected**: Secondary1Buys = 10000 (net)

### Test Case 4: No SSSA Records
- **Setup**: No SSSA records for plan/date
- **Expected**: Secondary1Buys = 0

### Test Case 5: Non-XI Activity
- **Setup**: SSSA has activity code 'AB' (not 'XI')
- **Expected**: Secondary1Buys = 0 (record filtered out)

### Test Case 6: Invalid Input
- **Setup**: RKPlan = '' or TradeDate = 0
- **Expected**: Procedure returns immediately, Secondary1Buys unchanged

---

**AI-Generated Documentation Notice**: This procedure documentation was generated using AI analysis and should be reviewed by OmniScript and business logic experts for accuracy.

**Last Updated**: 2026-01-23
