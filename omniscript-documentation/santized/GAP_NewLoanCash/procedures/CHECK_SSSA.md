# Procedure: CHECK.SSSA

**Program**: GAP_NewLoanCash.cbl  
**Lines**: 59-75  
**Last Updated**: February 4, 2026  
**Purpose**: Validate and adjust Secondary1Buys amount by querying Secondary Settlement Activity (SSSA) to account for loan reversals

---

## Overview

The CHECK.SSSA routine queries the Secondary Settlement Activity (SSSA) database to aggregate BUY and SELL transactions for a specific plan and security, correctly netting reversals to calculate the true loan activity requiring cash reconciliation. This routine was added on 09/25/2024 to address incorrect handling when both BUYS and SELLS exist for the same position.

## Input Requirements

**Global Variables (must be set before call)**:
- **RKPlan**: Plan identifier to query
- **TradeDate**: Trade date to filter SSSA records

**Preconditions**:
- RKPlan must be non-empty string
- TradeDate must be valid numeric date (YYYYMMDD format)
- Both conditions validated at entry (Line 60)

## Processing Logic

### Step 1: Validation Check
```omniscript
if (RKPlan <> '') and (TradeDate <> 0);
```
- Ensures both required parameters have valid values
- If validation fails, routine exits immediately via GOBACK
- Prevents unnecessary database queries with incomplete criteria

### Step 2: Initialize Accumulator
```omniscript
WK001 = 0;
```
- **WK001**: Local variable accumulator for netting BUY/SELL amounts
- Reset to zero at start of each routine invocation
- Ensures accurate calculation even with multiple calls

### Step 3: Query Secondary Settlement Activity
```omniscript
sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate);
```
- Queries SSSA database for specific plan/security/date combination
- **SECURITYID**: Hardcoded to 'POOLLOAN3' (loan pool security type)
- Returns all settlement activity records matching criteria
- May return multiple records if both BUYS and SELLS occurred

### Step 4: Process Settlement Records
```omniscript
loop while sssaobj_next();
   if sssaobj_de(011) = 'XI';          // Transaction type check
      if sssaobj_de(009) = 'B';         // BUY transaction
         WK001 = WK001 + sssaobj_numde(235);
      end;
      if sssaobj_de(009) = 'S';         // SELL transaction (reversal)
         WK001 = WK001 - sssaobj_numde(235);
      end; 
   end;
endloop;
```

**Processing Steps**:
1. Iterate through all returned SSSA records
2. Filter for transaction type 'XI' (loan activity indicator)
3. For each XI transaction:
   - **BUY ('B')**: Add transaction amount to accumulator (cash outflow)
   - **SELL ('S')**: Subtract transaction amount from accumulator (reversal/cash inflow)
4. Continue until all records processed

**Key Business Logic**:
- **Netting Calculation**: BUYS - SELLS = Net loan activity requiring cash reconciliation
- **Reversal Handling**: SELL transactions reduce the net loan amount
- **Example Scenarios**:
  - BUY $100,000 only → Net = $100,000
  - BUY $100,000 + SELL $25,000 → Net = $75,000 (partial reversal)
  - BUY $100,000 + SELL $100,000 → Net = $0 (complete reversal, no cash impact)

### Step 5: Update Global Variable
```omniscript
Secondary1Buys = WK001;
```
- Overwrites original Secondary1Buys value with netted amount
- Global variable now reflects true cash reconciliation requirement
- Used by main program logic to generate C1 activity record

### Step 6: Return to Caller
```omniscript
GOBACK;
```
- Standard OMNISCRIPT routine exit
- Returns control to main program after PERFORM statement

## Output

**Modified Global Variables**:
- **Secondary1Buys**: Updated with net BUY/SELL amount from SSSA

**Side Effects**:
- None - routine is read-only on database (no updates)
- No file I/O operations
- No other global variables modified

## Database Operations

**Read Operations**:
- **sssaobj_view()**: Query SSSA records by plan/security/date
- **sssaobj_next()**: Iterate through result set
- **sssaobj_de(011)**: Read transaction type field
- **sssaobj_de(009)**: Read buy/sell indicator field
- **sssaobj_numde(235)**: Read transaction amount field

**Write Operations**: None

## Error Handling

**Validation**:
- Checks for empty RKPlan or zero TradeDate before processing
- Early exit prevents invalid database queries

**Assumptions**:
- SSSA database is available and accessible
- SSSA records exist for the plan/security/date (not validated)
- sssaobj_view() returns empty result set if no matches (handled by loop)

**Unhandled Scenarios**:
- Database connection failures (would propagate to caller)
- Invalid data in SSSA fields (assumed clean data)
- Missing SSSA records (treated as zero activity - acceptable)

## Business Rules

1. **Transaction Type Filtering**: Only 'XI' transactions are loan-related activity
2. **Security Restriction**: Only POOLLOAN3 securities processed (loan pools)
3. **Reversal Recognition**: SELL transactions negate corresponding BUYS
4. **Net Activity**: Final amount represents true cash reconciliation requirement

## Change History

- **09/25/2024 - Gary Matten**: Created routine to recognize loan reversal activity and net activity correctly when there are BUYS and SELLS
- **Initial Implementation**: Previously, program used Secondary1Buys directly from position object without checking for reversals

## Integration Points

**Called By**: Main program loop (Line 38) via `PERFORM 'CHECK.SSSA'`

**Call Conditions**:
- Only invoked when Secondary1Buys <> 0 (optimization - skip if no initial activity)
- Called for each position record being processed

**Dependencies**:
- Requires RKPlan and TradeDate to be set from current position object iteration
- Must be called before comparing Secondary1Buys to PriorCashApplied

## Performance Considerations

**Efficiency**:
- Guarded by non-zero Secondary1Buys check (avoids unnecessary calls)
- Single database query per invocation
- Filters at database level (PLAN, SECURITYID, DATE)
- In-memory aggregation (minimal processing overhead)

**Potential Issues**:
- No query result limit - large result sets could impact performance
- No indexing assumptions documented - query performance depends on SSSA indexes
- Called once per position record - could be expensive for large position datasets

## Testing Scenarios

**Test Cases**:
1. **BUY Only**: Single BUY transaction → Secondary1Buys = BUY amount
2. **BUY + Partial SELL**: BUY $100k + SELL $25k → Secondary1Buys = $75k
3. **BUY + Full SELL**: BUY $100k + SELL $100k → Secondary1Buys = $0
4. **Multiple BUYS**: Two BUYS $50k each → Secondary1Buys = $100k
5. **No XI Records**: Only non-XI transactions → Secondary1Buys = $0
6. **Empty SSSA**: No matching records → Secondary1Buys = $0
7. **Invalid Input**: Empty RKPlan or zero TradeDate → Early exit, Secondary1Buys unchanged

---

**Review Status**: Generated by automated analysis - requires expert validation of transaction type codes and SSSA field interpretations
