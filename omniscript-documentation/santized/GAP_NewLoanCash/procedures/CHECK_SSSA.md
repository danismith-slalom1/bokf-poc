# CHECK.SSSA Routine Documentation

## Routine: CHECK.SSSA
**Program**: GAP_NewLoanCash  
**Lines**: 59-75  
**Purpose**: Detect and calculate net loan activity accounting for reversals  
**Last Updated**: 2026-02-03

---

## Overview

CHECK.SSSA is a subroutine that queries the SSSA (Trust Transaction) database to identify loan reversal activity and calculate the net loan amount. It handles scenarios where loan purchases (buys) are offset by loan sales (sells) on the same trade date, ensuring that cash reconciliation reflects the actual net cash impact rather than gross transaction amounts.

---

## Entry Point

### Invocation
```omniscript
PERFORM 'CHECK.SSSA';
```

**Called From**: Main program, line 38  
**Call Condition**: `IF (Secondary1Buys <> 0)` (line 37)

---

## Parameters

### Input Parameters (via Global Variables)
| Parameter | Type | Source | Purpose |
|-----------|------|--------|---------|
| RKPlan | String | poppobj_de(030) | Plan identifier for filtering |
| TradeDate | Numeric | poppobj_numde(008) | Trade date for filtering |
| Secondary1Buys | Numeric | poppobj_numde(741) | Initial loan amount (to be adjusted) |

### Output Parameters (via Global Variables)
| Parameter | Type | Destination | Purpose |
|-----------|------|-------------|---------|
| Secondary1Buys | Numeric | Modified in place | Net loan amount after reversals |

---

## Algorithm

### Pseudocode
```
ROUTINE CHECK.SSSA:
  IF RKPlan is not empty AND TradeDate is not zero THEN
    SET accumulator WK001 = 0
    
    QUERY SSSA table WHERE:
      PLAN = RKPlan
      SECURITYID = 'POOLLOAN3'
      DATE = TradeDate
    
    FOR EACH activity record:
      IF transaction source = 'XI' THEN
        IF transaction type = 'B' (Buy) THEN
          WK001 = WK001 + transaction amount
        END IF
        
        IF transaction type = 'S' (Sell) THEN
          WK001 = WK001 - transaction amount
        END IF
      END IF
    END FOR
    
    SET Secondary1Buys = WK001
  END IF
  
  RETURN (GOBACK)
END ROUTINE
```

---

## Detailed Logic

### Step 1: Parameter Validation (Line 60)
```omniscript
if (RKPlan <> '') and (TradeDate <> 0);
```

**Purpose**: Ensure valid parameters before querying database

**Validation Checks**:
- RKPlan must not be empty string
- TradeDate must not be zero (invalid date)

**If Validation Fails**:
- Routine exits immediately via GOBACK (line 75)
- Secondary1Buys remains unchanged (original value from field 741)
- No database query performed

**Business Rationale**: Prevents database queries with incomplete parameters that would return no results or cause errors.

---

### Step 2: Initialize Accumulator (Line 61)
```omniscript
WK001 = 0;
```

**Purpose**: Initialize work variable for net amount calculation

**Variable**: WK001 (local to routine)  
**Initial Value**: 0 (zero)  
**Type**: Numeric  
**Purpose**: Accumulator for net buy/sell activity

---

### Step 3: Query Activity Records (Line 62)
```omniscript
sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
```

**Database**: SSSA (TRUSTTRANS.P1)  
**Operation**: Query/View

**Filter Criteria**:
| Field | Value | Purpose |
|-------|-------|---------|
| PLAN | RKPlan | Match same retirement plan |
| SECURITYID | 'POOLLOAN3' | Match pool loan security |
| DATE | TradeDate | Match same trade date |

**Expected Results**: 
- 0 records: No reversal activity (WK001 remains 0, Secondary1Buys unchanged)
- 1+ records: Process each to calculate net amount

---

### Step 4: Process Activity Loop (Lines 63-72)
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

**Iteration**: For each activity record returned by query

#### Step 4a: Filter by Transaction Source (Line 64)
```omniscript
if sssaobj_de(011) = 'XI';
```

**Field**: 011 (Transaction Source)  
**Filter Value**: 'XI'  
**Purpose**: Only process specific transaction source type

**Other Sources**: Ignored (not included in netting calculation)

---

#### Step 4b: Process Buy Transactions (Line 65)
```omniscript
if sssaobj_de(009) = 'B';
   WK001 = WK001 + sssaobj_numde(235);
end;
```

**Transaction Type**: 'B' (Buy)  
**Operation**: Add amount to accumulator  
**Field**: 235 (Transaction Amount)

**Example**:
- WK001 = $0
- Buy transaction: $100,000
- Result: WK001 = $100,000

---

#### Step 4c: Process Sell Transactions (Line 68)
```omniscript
if sssaobj_de(009) = 'S';
   WK001 = WK001 - sssaobj_numde(235);
end;
```

**Transaction Type**: 'S' (Sell)  
**Operation**: Subtract amount from accumulator (reversal)  
**Field**: 235 (Transaction Amount)

**Example**:
- WK001 = $100,000 (from buy)
- Sell transaction: $30,000
- Result: WK001 = $70,000 (net activity)

---

### Step 5: Assign Net Result (Line 73)
```omniscript
Secondary1Buys = WK001;
```

**Purpose**: Update global variable with net loan amount

**Side Effect**: Modifies `Secondary1Buys` global variable  
**Impact**: Main program uses adjusted value for C1 record

**Possible Outcomes**:
- Positive value: Net buy activity (normal case)
- Zero: Complete reversal (no net activity)
- Negative value: Over-reversal (unusual but valid)

---

### Step 6: Return to Caller (Line 75)
```omniscript
GOBACK;
```

**Purpose**: Return control to main program  
**Return Value**: None (results via modified global variable)

---

## Data Flow

### Input Data Flow
```
Main Program → CHECK.SSSA
  - RKPlan (from POPP.030)
  - TradeDate (from POPP.008)
  - Secondary1Buys (from POPP.741)
```

### Processing Data Flow
```
CHECK.SSSA:
  1. Validate parameters
  2. Query SSSA database
  3. Filter transactions (source = 'XI')
  4. Accumulate buys (+) and sells (-)
  5. Calculate net amount in WK001
  6. Assign to Secondary1Buys
```

### Output Data Flow
```
CHECK.SSSA → Main Program
  - Secondary1Buys (modified with net amount)
```

---

## Examples

### Example 1: Simple Buy (No Reversal)
**Input**:
- RKPlan: 'ABC123'
- TradeDate: 20240110
- Secondary1Buys: $100,000

**SSSA Query Results**:
| Type | Source | Amount |
|------|--------|--------|
| B | XI | $100,000 |

**Processing**:
- WK001 = 0
- Process 'B': WK001 = 0 + 100,000 = 100,000
- Secondary1Buys = 100,000

**Output**: Secondary1Buys = $100,000 (unchanged)

---

### Example 2: Partial Reversal
**Input**:
- RKPlan: 'DEF456'
- TradeDate: 20240112
- Secondary1Buys: $100,000

**SSSA Query Results**:
| Type | Source | Amount |
|------|--------|--------|
| B | XI | $100,000 |
| S | XI | $30,000 |

**Processing**:
- WK001 = 0
- Process 'B': WK001 = 0 + 100,000 = 100,000
- Process 'S': WK001 = 100,000 - 30,000 = 70,000
- Secondary1Buys = 70,000

**Output**: Secondary1Buys = $70,000 (reduced by reversal)

---

### Example 3: Full Reversal
**Input**:
- RKPlan: 'GHI789'
- TradeDate: 20240114
- Secondary1Buys: $100,000

**SSSA Query Results**:
| Type | Source | Amount |
|------|--------|--------|
| B | XI | $100,000 |
| S | XI | $100,000 |

**Processing**:
- WK001 = 0
- Process 'B': WK001 = 0 + 100,000 = 100,000
- Process 'S': WK001 = 100,000 - 100,000 = 0
- Secondary1Buys = 0

**Output**: Secondary1Buys = $0 (fully reversed)

**Main Program Impact**: Skipped by BR-004 (zero amount exclusion)

---

### Example 4: Multiple Transactions
**Input**:
- RKPlan: 'JKL012'
- TradeDate: 20240115
- Secondary1Buys: $200,000

**SSSA Query Results**:
| Type | Source | Amount |
|------|--------|--------|
| B | XI | $100,000 |
| B | XI | $50,000 |
| B | XI | $50,000 |
| S | XI | $30,000 |
| S | XI | $20,000 |

**Processing**:
- WK001 = 0
- Process 'B': WK001 = 0 + 100,000 = 100,000
- Process 'B': WK001 = 100,000 + 50,000 = 150,000
- Process 'B': WK001 = 150,000 + 50,000 = 200,000
- Process 'S': WK001 = 200,000 - 30,000 = 170,000
- Process 'S': WK001 = 170,000 - 20,000 = 150,000
- Secondary1Buys = 150,000

**Output**: Secondary1Buys = $150,000 (net of all activity)

---

### Example 5: No Matching Activity
**Input**:
- RKPlan: 'MNO345'
- TradeDate: 20240116
- Secondary1Buys: $50,000

**SSSA Query Results**: (empty - no records)

**Processing**:
- WK001 = 0
- Loop: No iterations (no records)
- Secondary1Buys = 0

**Output**: Secondary1Buys = $0

**Note**: This scenario indicates data mismatch (POPP has buy amount but SSSA has no activity)

---

## Edge Cases

### Edge Case 1: Invalid Parameters
**Input**: RKPlan = '', TradeDate = 0

**Behavior**: 
- Validation fails at line 60
- Routine exits immediately via GOBACK
- Secondary1Buys unchanged

**Business Impact**: Uses original POPP.741 value (no adjustment)

---

### Edge Case 2: Negative Net Amount (Over-Reversal)
**Input**: 
- Secondary1Buys: $100,000
- SSSA: Buy $100K, Sell $120K

**Processing**: WK001 = 100,000 - 120,000 = -20,000

**Output**: Secondary1Buys = -$20,000

**Main Program Impact**: 
- NewLoanUnits = 0 - (-20,000) = +$20,000
- C1 record has positive amount (unusual)

**Business Question**: Is over-reversal valid? Requires business owner clarification.

---

### Edge Case 3: Non-XI Transaction Source
**SSSA Records**:
| Type | Source | Amount |
|------|--------|--------|
| B | XX | $100,000 |
| B | XI | $50,000 |

**Processing**:
- First record (source 'XX'): Skipped (line 64 filter)
- Second record (source 'XI'): WK001 = 50,000

**Output**: Secondary1Buys = $50,000

**Business Logic**: Only 'XI' source transactions are included in netting.

---

## Performance Considerations

### Database Query
- **Operation**: `sssaobj_view()` with 3-field filter
- **Index Recommendation**: PLAN, SECURITYID, DATE
- **Expected Records**: Typically 0-10 records per query
- **Performance**: Low impact (single-row lookup)

### Loop Iterations
- **Typical**: 0-2 iterations (buy + possible sell)
- **Maximum**: Dependent on transaction volume
- **Complexity**: O(n) where n = activity records

### Call Frequency
- **Condition**: Only when Secondary1Buys ≠ 0 (line 37)
- **Typical**: 20-50% of position records (estimate)
- **Impact**: Moderate (adds database query overhead)

---

## Error Handling

### Current Implementation
- **Parameter Validation**: ✓ Implemented (line 60)
- **Database Query Errors**: ✗ Not handled (relies on OMNISCRIPT runtime)
- **Data Type Errors**: ✗ Not handled (assumes valid numeric amounts)

### Potential Errors
1. **Database unavailable**: Runtime exception
2. **Invalid field 235 data**: Type coercion or error
3. **NULL values**: Potential calculation errors

### Recommendations
See [Error Handling Documentation](../GAP_NewLoanCash_ERROR_HANDLING.md) for comprehensive error handling recommendations.

---

## Testing Scenarios

### Test 1: Normal Buy with Reversal
- Setup: Insert buy + sell in SSSA
- Expected: Net amount calculated correctly
- Validation: Compare WK001 to manual calculation

### Test 2: Buy Only (No Reversal)
- Setup: Insert buy only in SSSA
- Expected: Amount unchanged
- Validation: Secondary1Buys = original value

### Test 3: Full Reversal
- Setup: Insert equal buy and sell
- Expected: Secondary1Buys = 0
- Validation: Main program skips record

### Test 4: No Activity Records
- Setup: Empty SSSA for plan/date
- Expected: Secondary1Buys = 0
- Validation: Check for data mismatch warning

### Test 5: Invalid Parameters
- Setup: RKPlan = '' or TradeDate = 0
- Expected: Routine exits early, no change
- Validation: Secondary1Buys = original value

---

## Dependencies

### Upstream Dependencies
- **Main Program**: Provides RKPlan, TradeDate, Secondary1Buys
- **POPP Database**: Source of input parameters
- **SSSA Database**: Source of activity records

### Downstream Impact
- **Main Program**: Uses adjusted Secondary1Buys for C1 record
- **Idempotency**: Adjusted amount written to field 877
- **Cash Reconciliation**: Net amount used in C1 record

---

## Related Business Rules

- **BR-005**: Reversal Netting Logic
- **BR-001**: Security Type Filter (POOLLOAN3)

See [Business Rules Documentation](../GAP_NewLoanCash_BUSINESS_RULES.md) for complete details.

---

## Modification History

| Date | Change | Reason |
|------|--------|--------|
| 09/25/2024 | Added sell transaction handling | GPD-XXXX: Recognize loan reversal activity |
| 12/21/2023 | Initial implementation | Original development |

---

## Maintenance Notes

### Potential Enhancements
1. Add error handling for database query failures
2. Log transactions processed for auditing
3. Add validation for negative net amounts
4. Support additional transaction types beyond B/S

### Change Impact
- Modifying transaction source filter ('XI') affects which records are processed
- Adding new transaction types requires new conditional logic
- Changing accumulation logic affects cash reconciliation accuracy

---

*This procedure documentation provides comprehensive details on the CHECK.SSSA reversal detection routine.*

*Last Updated*: 2026-02-03
