# CHECK.SSSA - Secondary Security Sub-Account Reversal Check

## Source Location
- **Lines**: 59-75
- **File**: GAP_NewLoanCash.txt
- **Type**: ROUTINE (called via PERFORM)

## Purpose

Examines Secondary Security Sub-Account (SSSA) transaction records for a specific plan, security, and trade date to calculate the net buy/sell activity. This handles loan reversal scenarios where both buy and sell transactions exist for the same position, ensuring the program uses the correct net amount rather than gross buys.

## Business Context

When loan activity occurs, it is possible for both buys and sells (reversals) to be recorded in the SSSA for the same plan and date. Without this routine, the program would use the gross buy amount from POPP, which could overstate the actual new loan cash. This routine nets the activity to produce an accurate offset amount.

**Added**: 09/25/2024 (per change history) to recognize loan reversal activity and net activity correctly when there are BUYS and SELLS.

---

## Processing Steps

### Step 1: Validate Input (Line 60)
- Check that `RKPlan` is not empty AND `TradeDate` is not zero
- If either is invalid, skip the entire routine (no SSSA lookup)

### Step 2: Initialize Accumulator (Line 61)
- Set `WK001 = 0` (working accumulator for net amount)

### Step 3: Query SSSA Records (Line 62)
- Open SSSA view: `sssaobj_view(PLAN:RKPlan SECURITYID:'POOLLOAN3' DATE:TradeDate)`
- Filters to the specific plan, POOLLOAN3 security, and trade date

### Step 4: Process SSSA Transactions (Lines 63-72)
Loop through all matching SSSA records:

1. **Check transaction type** (Line 64): Only process records where `sssaobj_de(011) = 'XI'`
   - 'XI' is the specific transaction type code for the relevant loan activity

2. **For Buy transactions** (Lines 65-67):
   - If `sssaobj_de(009) = 'B'` (Buy indicator)
   - Add amount: `WK001 = WK001 + sssaobj_numde(235)`

3. **For Sell/Reversal transactions** (Lines 68-70):
   - If `sssaobj_de(009) = 'S'` (Sell indicator)
   - Subtract amount: `WK001 = WK001 - sssaobj_numde(235)`

### Step 5: Update Caller Variable (Line 73)
- Set `Secondary1Buys = WK001` (net amount replaces original POPP value)

### Step 6: Return (Line 75)
- `GOBACK` returns control to the calling point in the main loop

---

## Input Requirements

| Variable | Expected State | Source |
|---|---|---|
| RKPlan | Non-empty string | Set by main loop from `poppobj_de(030)` |
| TradeDate | Non-zero numeric date | Set by main loop from `poppobj_numde(008)` |

## Output / Side Effects

| Variable | Modification | Description |
|---|---|---|
| Secondary1Buys | Overwritten with WK001 | Net buy/sell amount from SSSA records |

**Note**: This routine modifies the global variable `Secondary1Buys`, which is then used by the main loop for the idempotency check and C1 record generation.

## Variables Used

| Variable | Scope | Read/Write | Lines |
|---|---|---|---|
| RKPlan | Global | Read | 60, 62 |
| TradeDate | Global | Read | 60, 62 |
| WK001 | Local | Write/Read | 61, 66, 69, 73 |
| Secondary1Buys | Global | Write | 73 |

## Database Operations

| Operation | Line | Parameters | Purpose |
|---|---|---|---|
| `sssaobj_view` | 62 | PLAN:RKPlan, SECURITYID:POOLLOAN3, DATE:TradeDate | Open SSSA cursor |
| `sssaobj_next` | 63 | (none) | Advance cursor / loop condition |
| `sssaobj_de(011)` | 64 | DE 011 | Read transaction type |
| `sssaobj_de(009)` | 65, 68 | DE 009 | Read buy/sell indicator |
| `sssaobj_numde(235)` | 66, 69 | DE 235 | Read transaction amount |

## SSSA Data Elements

| DE# | Name | Values | Purpose |
|---|---|---|---|
| 011 | Transaction Type | `XI` = Relevant loan activity | Filters to specific transaction type |
| 009 | Buy/Sell Indicator | `B` = Buy, `S` = Sell (Reversal) | Determines accumulation direction |
| 235 | Amount | Numeric | Transaction dollar amount |

## Error Handling

- **Input validation**: Skips processing if RKPlan is empty or TradeDate is zero (Line 60)
- **No error handling** for: sssaobj_view failure, unexpected DE values
- **Edge case**: If no SSSA records match, WK001 stays 0 and Secondary1Buys is set to 0. This could cause the main loop to skip the record (since `Secondary1Buys = 0` fails the output condition). This may be intentional if SSSA is considered the authoritative source.

## Dependencies

| Dependency | Type | Description |
|---|---|---|
| Main loop | Caller | Sets RKPlan and TradeDate before calling |
| sssaobj | Database object | SSSA transaction records |
| Secondary1Buys | Global variable | Modified as output to caller |

## Performance Characteristics

- **Called**: Once per POPP record where `Secondary1Buys <> 0`
- **Loop complexity**: O(M) where M = number of SSSA records for the plan/security/date
- **Database I/O**: One view open + M record reads per call
