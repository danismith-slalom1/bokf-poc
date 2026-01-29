# Main Processing Loop

**Paragraph/Section**: Main Loop (Lines 28-52)  
**Type**: Primary Processing Logic  
**Entry Point**: Program start after initialization

---

## Purpose and Function

This is the main processing loop that queries POPP (Plan Position) database for POOLLOAN3 security positions within a 7-day window, identifies new loan cash activity, generates C1 cash reconciliation records, and updates the source positions to prevent duplicate processing.

---

## Business Logic

### High-Level Flow
1. Query POPP database for POOLLOAN3 positions in date range [SevenDaysAgo, LastBusiness]
2. For each position record:
   - Read plan details, trade date, secondary market buy amount, and prior processing flag
   - Check SSSA for possible reversals if buys exist
   - Compare current buy amount with previously processed amount
   - If amounts differ (indicating new/unprocessed activity):
     - Generate C1 cash activity record
     - Write record to output file
     - Update POPP position with processed amount

### Business Purpose
- **Cash Reconciliation**: Captures the asset side (loan purchases) of cash movement for trust accounting
- **Idempotency**: Prevents duplicate C1 activity by comparing with previously processed amounts
- **Reversal Handling**: Accounts for loan activity reversals by querying secondary transaction system (SSSA)

---

## Step-by-Step Processing Logic

### Step 1: Query Position Records (Line 28)
```omniscript
poppobj_view(securityid:'POOLLOAN3' datelo:SevenDaysAgo datehi:LastBusiness);
```
- **Action**: Query POPP database for all POOLLOAN3 positions with trade dates between SevenDaysAgo and LastBusiness
- **Security ID**: 'POOLLOAN3' represents a pool of loan securities
- **Date Range**: Last 7 calendar days
- **Result**: Cursor positioned at first matching record (if any)

### Step 2: Iterate Through Position Records (Line 29)
```omniscript
loop while poppobj_next();
```
- **Action**: Fetch next position record from query result set
- **Termination**: Loop ends when no more records available
- **Performance Note**: No limit on record count - processes all matching positions

### Step 3: Extract Position Data (Lines 30-33)
```omniscript
RKPlan           = poppobj_de(030);
TradeDate        = poppobj_numde(008);
Secondary1Buys   = poppobj_numde(741);
PriorCashApplied = poppobj_numde(877);
```
- **RKPlan (DE 030)**: 6-character plan identifier (e.g., retirement plan account)
- **TradeDate (DE 008)**: Original trade date of the position (YYYYMMDD)
- **Secondary1Buys (DE 741)**: Dollar amount of secondary market loan purchases
- **PriorCashApplied (DE 877)**: User-defined field storing previously processed amount

**Business Rule**: DE 877 serves as an idempotency flag - if it matches DE 741, this position was already processed

### Step 4: Check for Reversals (Lines 34-36)
```omniscript
If (Secondary1Buys <> 0);
   PERFORM 'CHECK.SSSA';
End;
```
- **Condition**: Only check SSSA if there's actual buy activity (non-zero)
- **Purpose**: Loan transactions may be reversed in the secondary system (SSSA)
- **Effect**: CHECK.SSSA routine recalculates Secondary1Buys based on net SSSA activity
- **Business Significance**: Ensures C1 activity reflects NET loan purchases after reversals

**Important**: Secondary1Buys may be modified by CHECK.SSSA routine before comparison in Step 5

### Step 5: Check for New/Unprocessed Activity (Line 38)
```omniscript
if (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0);
```
- **Condition 1**: PriorCashApplied ≠ Secondary1Buys → Amount has changed or never processed
- **Condition 2**: Secondary1Buys ≠ 0 → Actual activity exists (not zero-dollar position)
- **Logic**: Generate C1 activity ONLY if there's unprocessed, non-zero loan activity

**Idempotency Check**: If amounts match, position was already processed - skip to next record

### Step 6: Re-extract Position Data (Lines 39-42)
```omniscript
RKPlan           = poppobj_de(030);
TradeDate        = poppobj_numde(008);
NewLoanUnits     = 0 - Secondary1Buys;
TrustAccount     = poppobj_de(01510);
```
- **Note**: RKPlan and TradeDate redundantly re-read (already extracted in Step 3)
- **NewLoanUnits**: Negate Secondary1Buys for right-side (AC) cash reconciliation
  - **Business Rule**: Loan purchases are negative cash flow (asset acquisition)
- **TrustAccount (DE 01510)**: 32-character trust account linked to the plan

**Technical Debt**: Redundant re-reading of RKPlan and TradeDate suggests code refactoring needed

### Step 7: Build C1 Activity Record (Lines 43-48)
```omniscript
OcText_Set(Line 1 'C100' 4);
OcText_Set(Line 5 RKPlan 6);
OcText_Set(Line 31 OcFmt(LastBusiness 'Z8') 8);
OcText_Set(Line 40 TrustAccount 32);
OcText_Set(Line 73 '000000000000000    2' 20);
OcText_Set(Line 115 '0' 1);
OcText_Set(Line 116 OcFmt(NewLoanUnits 'Z,12V2-') 15);
OcText_Set(Line 134 '00339' 5);
```

**Record Layout** (Fixed-width format):
| Position | Length | Field | Value | Description |
|----------|--------|-------|-------|-------------|
| 1-4 | 4 | Record Type | 'C100' | Cash activity record identifier |
| 5-10 | 6 | Plan ID | RKPlan | Plan account number |
| 31-38 | 8 | Effective Date | LastBusiness | Date for C1 activity (prior business day) |
| 40-71 | 32 | Trust Account | TrustAccount | Trust account number |
| 73-92 | 20 | Position Indicator | '000000000000000    2' | Position 92 = '2' (right side - AC) |
| 115 | 1 | Flag | '0' | Unknown flag field |
| 116-130 | 15 | Amount | NewLoanUnits | Dollar amount (formatted with commas, 2 decimals, sign) |
| 134-138 | 5 | Transaction Code | '00339' | Specific transaction type code for loan purchases |

**Critical Business Rules**:
- **Position 92 = '2'**: Indicates right side (AC) of cash reconciliation
  - Changed from '1' to '2' per GPD-1704 change on 06/27/2024
- **Transaction Code '00339'**: Standard code for new loan cash offset activity
- **Effective Date = LastBusiness**: Activity dated to prior business day (not trade date)
- **Amount Negated**: Loan purchases reduce cash (negative value)

### Step 8: Write Record to Output File (Line 49)
```omniscript
OcFile1_Write(Line);
```
- **Action**: Append C1 activity record to output file
- **Risk**: No error checking - write failures are silent
- **Impact**: Record becomes part of daily cash reconciliation feed

### Step 9: Update POPP Position (Lines 50-51)
```omniscript
poppobj_setde(denum:877 value:Secondary1Buys);
poppobj_update();
```
- **Action**: Set DE 877 (UDF1) to current Secondary1Buys amount
- **Purpose**: Mark position as processed to prevent duplicate C1 activity on next run
- **Risk**: No error checking - update failures could cause duplicates
- **Business Impact**: If update fails, next run will regenerate C1 activity for this position

**Critical Transaction Integrity Issue**: File write (Step 8) and database update (Step 9) are NOT atomic
- If file write succeeds but DB update fails → Duplicate C1 activity on next run
- If DB update succeeds but file write fails → Lost C1 activity (but idempotency prevents recovery)

### Step 10: Continue to Next Record (Line 52)
```omniscript
endloop;
```
- **Action**: Return to Step 2, fetch next position record
- **Termination**: Loop ends when all matching POPP records processed

---

## Variables Used

### Read Variables
| Variable | Source | Purpose |
|----------|--------|---------|
| SevenDaysAgo | Calculated | Start date for POPP query |
| LastBusiness | Calculated | End date for POPP query, effective date for C1 activity |
| RKPlan | POPP DE 030 | Plan identifier |
| TradeDate | POPP DE 008 | Original trade date |
| Secondary1Buys | POPP DE 741 (may be modified by CHECK.SSSA) | Net loan purchase amount |
| PriorCashApplied | POPP DE 877 | Previously processed amount |
| TrustAccount | POPP DE 01510 | Trust account number |

### Modified Variables
| Variable | Modification | Location |
|----------|--------------|----------|
| Secondary1Buys | **May be recalculated** | CHECK.SSSA routine (lines 34-36) |
| NewLoanUnits | Calculated as `0 - Secondary1Buys` | Line 41 |
| Line | Built with multiple OcText_Set() calls | Lines 43-48 |
| POPP DE 877 | Updated to Secondary1Buys | Line 50 |

---

## Error Conditions and Special Handling

### Expected Error Conditions
1. **No POPP Records Found**: Loop executes zero times, output file empty
   - **Handling**: None (silent completion)
   - **Impact**: May be valid (no new loan activity) or indicate query error

2. **Zero Secondary1Buys**: Position has no buy activity
   - **Handling**: Skipped by condition on line 38
   - **Impact**: No C1 activity generated (correct behavior)

3. **Amounts Already Match**: PriorCashApplied = Secondary1Buys
   - **Handling**: Skipped by condition on line 38 (idempotency check)
   - **Impact**: No duplicate C1 activity generated (correct behavior)

4. **File Write Failure**: OcFile1_Write() fails
   - **Handling**: NONE - no error checking
   - **Impact**: CRITICAL - record lost, no retry, no logging

5. **Database Update Failure**: poppobj_update() fails
   - **Handling**: NONE - no error checking
   - **Impact**: CRITICAL - duplicate C1 activity on next run

### Special Cases

**Reversal Activity**: 
- If SSSA contains sell transactions (XI/S), CHECK.SSSA reduces Secondary1Buys
- Net amount may be zero or negative
- Condition on line 38 prevents processing if final amount is zero

**Multiple Runs on Same Day**:
- First run: PriorCashApplied ≠ Secondary1Buys → generates C1 activity, updates DE 877
- Second run: PriorCashApplied = Secondary1Buys → skips (idempotency works)
- **Exception**: If first run's DB update fails, second run generates duplicate

---

## Performance Considerations

### Database Queries
- **POPP Query**: One query per execution (date range filter)
  - Typical volume: Hundreds of positions per day
  - Performance: GOOD (indexed by security ID and date)

- **SSSA Query**: One query per POPP record with non-zero Secondary1Buys
  - Nested loop pattern: O(n * m) where n = POPP records, m = SSSA records
  - Performance: MODERATE - could be slow with high SSSA activity

### File I/O
- **Sequential Writes**: One write per processed position
  - Performance: EXCELLENT (sequential append)
  - No buffering issues expected

### Optimization Opportunities
1. **Eliminate redundant reads**: RKPlan and TradeDate read twice (lines 30/39, 31/40)
2. **Batch database updates**: Consider committing POPP updates in batches
3. **Optimize SSSA query**: Add date range filter to SSSA query (currently queries all dates)

---

## Dependencies on Other Paragraphs

### Calls to Other Routines
| Routine Called | Location | Purpose |
|---------------|----------|---------|
| CHECK.SSSA | Line 35 | Recalculate Secondary1Buys based on SSSA activity, accounting for reversals |

### Called By
- **Program Entry Point**: This is the main processing logic, called automatically after initialization

---

## Control Flow

```
START
  ↓
Query POPP for POOLLOAN3 positions (SevenDaysAgo to LastBusiness)
  ↓
[LOOP] For each position:
  ↓
  Read: RKPlan, TradeDate, Secondary1Buys, PriorCashApplied
  ↓
  IF Secondary1Buys ≠ 0
    ↓ YES
    PERFORM CHECK.SSSA (may modify Secondary1Buys)
    ↓
  IF (PriorCashApplied ≠ Secondary1Buys) AND (Secondary1Buys ≠ 0)
    ↓ YES
    Calculate: NewLoanUnits = 0 - Secondary1Buys
    ↓
    Build C1 activity record (Line)
    ↓
    Write record to output file
    ↓
    Update POPP DE 877 = Secondary1Buys
    ↓
  [Continue to next POPP record]
  ↓
[END LOOP] No more records
  ↓
END
```

---

## Business Rules Summary

1. **7-Day Lookback Window**: Process positions from last 7 calendar days
2. **POOLLOAN3 Only**: Only process loan pool securities, ignore other security types
3. **Net Activity After Reversals**: Check SSSA for reversals before generating C1 activity
4. **Idempotency**: Use POPP DE 877 to track processed amounts, prevent duplicates
5. **Right-Side Cash Activity**: Position indicator '2' for asset (AC) side of reconciliation
6. **Prior Business Day Dating**: C1 activity dated to LastBusiness, not trade date
7. **Negative Cash Flow**: Loan purchases represented as negative amounts
8. **Transaction Code 00339**: Standard code for new loan cash offset

---

## Change History Impacts

### 06/27/2024 - GPD-1704
- **Change**: Position 92 changed from '1' to '2'
- **Impact**: Corrected classification of cash activity to right side (AC)
- **Location**: Line 45

### 09/25/2024
- **Change**: Enhanced reversal recognition for both BUYS and SELLS
- **Impact**: More accurate net activity calculation in CHECK.SSSA
- **Location**: CHECK.SSSA routine (affects Secondary1Buys used in line 38)

---

## Related Documentation
- [CHECK.SSSA Routine Documentation](./CHECK.SSSA.md)
- [Data Dictionary](../GAP_NewLoanCash_DATA_DICTIONARY.md) - Variable definitions
- [Error Handling Analysis](../GAP_NewLoanCash_ERROR_HANDLING.md) - Missing error checks

---

*AI-Generated Documentation - Review with OmniScript/COBOL experts for accuracy*
