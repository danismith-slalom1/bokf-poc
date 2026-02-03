# CHECK.SSSA Procedure Documentation

## Overview
- **Procedure Name**: CHECK.SSSA
- **Location**: Lines 59-75
- **Purpose**: Calculate net loan activity by checking for reversal transactions (sells) that offset original loan purchases (buys)
- **Called From**: Main program loop (Line 38 via PERFORM statement)
- **Business Function**: Recognize loan reversal activity and net activity correctly when there are BUYS and SELLS

---

## Procedure Signature

### Input Parameters (Via Global Variables)
- **RKPlan**: Plan identifier from current position record
- **TradeDate**: Trade date from current position record
- **Secondary1Buys**: Initial loan purchase amount from position record (DE 741)

### Output Parameters (Via Global Variables)
- **Secondary1Buys**: Updated with net loan activity (buys - sells)

### Local Variables
- **WK001**: Accumulator for net transaction amount

---

## Procedure Logic

### Entry Condition Check
```omniscript
if (RKPlan <> '') and (TradeDate <> 0);
```
- Validates that both RKPlan and TradeDate contain meaningful values
- Prevents invalid database queries with empty or zero parameters
- If condition false, procedure returns immediately via GOBACK without modifying Secondary1Buys

### Initialization
```omniscript
WK001 = 0;
```
- Resets accumulator to zero before processing transactions
- Ensures clean calculation start for each invocation

### Database Query
```omniscript
sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
```
- **Database Object**: sssaobj (Secondary Market Activity)
- **Query Parameters**:
  - PLAN: Current plan being processed
  - SECURITYID: 'POOLLOAN3' (fixed value for loan pool)
  - DATE: Trade date from position record
- **Purpose**: Retrieve all secondary market transactions for the specific plan/security/date combination

### Transaction Processing Loop
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

#### Activity Code Filter (DE 011)
- **Field**: sssaobj_de(011)
- **Expected Value**: 'XI' (External In)
- **Purpose**: Process only external loan activity; ignore internal transfers or other activity types

#### Buy Transaction Processing (DE 009 = 'B')
- **Condition**: Transaction type is 'B' (Buy)
- **Action**: Add transaction amount to WK001 accumulator
- **Field**: sssaobj_numde(235) - Transaction amount
- **Business Logic**: Original loan purchases increase net activity

#### Sell Transaction Processing (DE 009 = 'S')
- **Condition**: Transaction type is 'S' (Sell)
- **Action**: Subtract transaction amount from WK001 accumulator
- **Field**: sssaobj_numde(235) - Transaction amount
- **Business Logic**: Loan reversals/sells decrease net activity; represents returned/cancelled loans

### Result Assignment
```omniscript
Secondary1Buys = WK001;
```
- Replaces original Secondary1Buys value with net calculated amount
- This value becomes the new loan offset amount used in C1 record generation
- Represents net loan activity after accounting for both purchases and reversals

### Return
```omniscript
GOBACK;
```
- Returns control to calling program
- Modified Secondary1Buys value available to main program

---

## Business Rules

### Transaction Type Interpretation
1. **'XI' Activity Code**: External In - represents secondary market loan activity
2. **'B' Transaction Type**: Buy - loan purchase from secondary market
3. **'S' Transaction Type**: Sell - loan reversal or return to secondary market

### Net Activity Calculation
- **Formula**: Net Activity = Sum(Buys) - Sum(Sells)
- **Example Scenarios**:
  - Original buy of $10,000, no sells: Net = $10,000
  - Original buy of $10,000, sell of $3,000: Net = $7,000
  - Original buy of $10,000, sell of $10,000: Net = $0 (full reversal)

### Cash Reconciliation Impact
- Net activity amount determines the C1 offset entry
- Proper netting ensures cash reconciliation accurately reflects actual loan exposure
- Prevents overstating cash requirements when loans are reversed

---

## Data Flow

### Input Data Flow
1. Main program retrieves position record with Secondary1Buys from POPP877 (DE 741)
2. Main program extracts RKPlan and TradeDate from same position record
3. CHECK.SSSA called via PERFORM statement
4. Procedure uses RKPlan/TradeDate to query SSSA records

### Internal Data Flow
1. WK001 initialized to 0
2. Loop through all SSSA records for plan/date/security combination
3. For each 'XI' activity:
   - Add 'B' transactions to WK001
   - Subtract 'S' transactions from WK001
4. Final WK001 value represents net activity

### Output Data Flow
1. Secondary1Buys overwritten with WK001 value
2. Control returns to main program
3. Main program uses updated Secondary1Buys for:
   - Duplicate check (compare with PriorCashApplied)
   - C1 record generation (as NewLoanUnits)
   - Position record update (stored in DE 877)

---

## Error Handling

### Explicit Validation
- **Entry Condition**: RKPlan and TradeDate validated before database query
- **Result**: Early exit via GOBACK if invalid parameters

### Implicit Error Handling
- **No SSSA Records**: Loop never executes; WK001 remains 0; Secondary1Buys becomes 0
- **No 'XI' Activity**: Inner if block never executes; WK001 remains 0
- **Mixed Activity**: Both buys and sells processed; net calculated correctly

### Unhandled Error Scenarios
- **Database Connection Failure**: No explicit error handling; program would likely fail
- **Invalid DE Field Values**: No validation of transaction amounts or types
- **Negative Net Activity**: Possible if sells exceed buys; no validation logic

---

## Performance Considerations

### Query Efficiency
- **Indexed Query**: Assumes PLAN+SECURITYID+DATE is indexed in SSSA
- **Record Count**: Typically low (1-3 transactions per plan/date)
- **Filter Strategy**: Early filter on 'XI' activity code reduces processing

### Processing Overhead
- **Called Frequency**: Once per position record with non-zero Secondary1Buys
- **Typical Volume**: Depends on number of qualifying position records (7-day window)
- **Optimization**: Entry condition check prevents unnecessary database queries

---

## Testing Considerations

### Test Scenarios

#### Scenario 1: No Reversal Activity
- **Input**: Secondary1Buys = $10,000; no SSSA records
- **Expected**: Secondary1Buys remains $10,000 (WK001 = 0, but no SSSA data)
- **Validation**: C1 record generated with full $10,000 offset

#### Scenario 2: Partial Reversal
- **Input**: Secondary1Buys = $10,000; SSSA has buy $10,000 and sell $3,000
- **Expected**: Secondary1Buys becomes $7,000
- **Validation**: C1 record generated with $7,000 offset

#### Scenario 3: Full Reversal
- **Input**: Secondary1Buys = $10,000; SSSA has buy $10,000 and sell $10,000
- **Expected**: Secondary1Buys becomes $0
- **Validation**: Main program should skip C1 generation (zero check at line 40)

#### Scenario 4: Multiple Buys and Sells
- **Input**: Secondary1Buys = $15,000; SSSA has buys $10,000 + $5,000 and sells $2,000 + $3,000
- **Expected**: Secondary1Buys becomes $10,000 (15,000 - 5,000)
- **Validation**: Net correctly calculated across multiple transactions

#### Scenario 5: Invalid Entry Conditions
- **Input**: RKPlan = '' or TradeDate = 0
- **Expected**: Procedure returns immediately; Secondary1Buys unchanged
- **Validation**: Original DE 741 value preserved

#### Scenario 6: Non-XI Activity
- **Input**: SSSA records exist but with activity code 'II' (Internal In)
- **Expected**: WK001 remains 0; Secondary1Buys becomes 0
- **Validation**: Only 'XI' transactions processed

### Edge Cases
- **Negative Net Activity**: Sells exceed buys (data quality issue?)
- **Zero Amount Transactions**: DE 235 = 0 (should be filtered in source data)
- **Missing Activity Code**: DE 011 is null or blank (if block skipped)

---

## Integration Points

### Calling Context
- **Called By**: Main program via PERFORM 'CHECK.SSSA' statement (Line 38)
- **Call Condition**: `If (Secondary1Buys <> 0)`
- **Purpose**: Only check for reversals when there's actual loan activity

### Database Dependencies
- **SSSA Object**: Must be accessible and queryable
- **Required Fields**:
  - DE 009: Transaction type (B/S)
  - DE 011: Activity code (XI)
  - DE 235: Transaction amount
- **Query Performance**: Assumes indexed access by PLAN+SECURITYID+DATE

### Global Variable Dependencies
- **Read**: RKPlan, TradeDate, Secondary1Buys
- **Write**: Secondary1Buys
- **Risk**: Modifies shared variable; caller must be aware of mutation

---

## Revision History
- **09/25/2024**: Procedure added by Gary Matten to recognize loan reversal activity and net activity correctly when there are BUYS and SELLS
- **Purpose**: Address business requirement to properly account for loan reversals in cash reconciliation

---

## Related Documentation
- [GAP_NewLoanCash Data Dictionary](../GAP_NewLoanCash_DATA_DICTIONARY.md) - See Secondary1Buys and WK001 variable definitions
- [GAP_NewLoanCash Call Graph](../GAP_NewLoanCash_CALL_GRAPH.md) - See procedure call hierarchy
- [GAP_NewLoanCash Comprehensive Documentation](../GAP_NewLoanCash_OVERVIEW.md) - See overall program flow

---

## Business Context

### Problem Solved
Prior to 09/25/2024, the program used the Secondary1Buys amount from the position record (DE 741) directly without checking for reversal activity. This led to overstated cash reconciliation amounts when loans were partially or fully reversed after the initial purchase.

### Solution Implemented
The CHECK.SSSA routine queries the detailed transaction history (SSSA) to find both buy and sell transactions for the same plan/date/security. By calculating the net activity (buys - sells), the program now accurately reflects the true loan exposure requiring cash offset.

### Business Value
- **Accurate Cash Reconciliation**: Prevents overstating cash requirements
- **Reversal Recognition**: Properly handles loan returns and cancellations
- **Audit Trail**: Uses transaction-level detail for verification
- **Data Quality**: Cross-validates position amounts with transaction detail
