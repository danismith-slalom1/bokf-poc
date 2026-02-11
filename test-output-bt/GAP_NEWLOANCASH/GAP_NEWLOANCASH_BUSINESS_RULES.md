# GAP_NEWLOANCASH Business Rules

## Overview

This document extracts and categorizes all explicit and implicit business rules from the GAP_NewLoanCash program. Rules are organized by domain and include their implementation location in the source code.

---

## 1. Explicit Business Rules

### 1.1 Date Range Rules

| Rule ID | Rule | Implementation | Line(s) |
|---|---|---|---|
| BR-001 | The processing window spans the **last 7 calendar days** from the run date | `SevenDaysAgo = OcDate_AddDays(RunDate, -7)` | 23, 26 |
| BR-002 | The activity date for C1 records is the **last business day** before the run date | `LastBusiness = OcDate_AddBusDays(RunDate, -1)` | 24, 27 |
| BR-003 | If the supplied run date is invalid, **fall back to current system date** | `if OcDate_Valid(RunDate)` with else branch | 22-28 |
| BR-004 | Only **POOLLOAN3** security positions are processed | `poppobj_view(securityid:'POOLLOAN3' ...)` | 31 |

### 1.2 Transaction Processing Rules

| Rule ID | Rule | Implementation | Line(s) |
|---|---|---|---|
| BR-005 | Records with **zero Secondary1Buys** are skipped entirely | `if (Secondary1Buys <> 0)` and `(Secondary1Buys <> 0)` conditions | 37, 40 |
| BR-006 | Records where **PriorCashApplied equals Secondary1Buys** are skipped (already processed) | `if (PriorCashApplied <> Secondary1Buys)` | 40 |
| BR-007 | The C1 offset amount is the **negation** of the buy amount | `NewLoanUnits = 0 - Secondary1Buys` | 43 |
| BR-008 | After processing, the POPP UDF1 field (DE 877) is updated with the Secondary1Buys value as an **idempotency marker** | `poppobj_setde(denum:877 value:Secondary1Buys)` | 54 |

### 1.3 Reversal/Netting Rules

| Rule ID | Rule | Implementation | Line(s) |
|---|---|---|---|
| BR-009 | When Secondary1Buys is non-zero, the program must check SSSA for **reversal activity** | `PERFORM 'CHECK.SSSA'` when `Secondary1Buys <> 0` | 37-39 |
| BR-010 | Only SSSA transactions with type **'XI'** are relevant to new loan activity | `if sssaobj_de(011) = 'XI'` | 64 |
| BR-011 | SSSA **Buy** transactions (indicator 'B') **increase** the net loan amount | `WK001 = WK001 + sssaobj_numde(235)` | 65-67 |
| BR-012 | SSSA **Sell** transactions (indicator 'S') represent **reversals** and **decrease** the net loan amount | `WK001 = WK001 - sssaobj_numde(235)` | 68-70 |
| BR-013 | The **net SSSA amount** replaces the original POPP Secondary1Buys value | `Secondary1Buys = WK001` | 73 |

### 1.4 Output Format Rules

| Rule ID | Rule | Implementation | Line(s) |
|---|---|---|---|
| BR-014 | C1 activity records use record type **'C100'** | `OcText_Set(Line 1 'C100' 4)` | 45 |
| BR-015 | The control field at position 92 must contain **'2'** (corrected from '1' per GPD-1704) | Hardcoded in `'000000000000000    2'` | 49 |
| BR-016 | The transaction code for new loan cash offset is **'00339'** | `OcText_Set(Line 134 '00339' 5)` | 52 |
| BR-017 | The offset amount is formatted as **signed decimal with 12 digits, 2 decimal places, trailing sign** | `OcFmt(NewLoanUnits 'Z,12V2-')` | 51 |
| BR-018 | The processing flag at position 115 is **'0'** | `OcText_Set(Line 115 '0' 1)` | 50 |

---

## 2. Implicit Business Rules (Inferred from Code)

| Rule ID | Rule | Evidence | Confidence |
|---|---|---|---|
| BR-I01 | The program runs **daily** as part of a batch cycle | File naming includes date/time stamps; 7-day lookback window suggests daily processing | 85% |
| BR-I02 | The program is **idempotent** - safe to re-run without creating duplicates | PriorCashApplied vs Secondary1Buys comparison prevents reprocessing | 95% |
| BR-I03 | **SSSA is the authoritative source** for net buy/sell amounts when available | CHECK.SSSA completely replaces POPP's Secondary1Buys with SSSA net amount | 80% |
| BR-I04 | New loan cash creates a **one-sided entry** that needs an offsetting C1 record for reconciliation | Program name "NewLoanCash" + offset logic + record type C100 | 90% |
| BR-I05 | The output file naming convention follows an **organizational standard** with `OTDALY.OMNISCRIPT.C1.` prefix | Structured naming pattern with environment, tool, type, and purpose components | 90% |
| BR-I06 | Only **one C1 offset record per POPP position** is expected | One record written per qualifying POPP record; idempotency prevents reprocessing | 90% |
| BR-I07 | The 7-day lookback provides a **catch-up window** for missed or late-arriving positions | Window is larger than daily processing needs | 85% |

---

## 3. Business Constraints

### 3.1 Data Constraints

| Constraint | Value | Source |
|---|---|---|
| Security ID filter | `POOLLOAN3` only | Hardcoded in poppobj_view |
| SSSA transaction type filter | `XI` only | Hardcoded in CHECK.SSSA |
| Buy/Sell indicators | `B` (buy) and `S` (sell) only | Hardcoded in CHECK.SSSA |
| Plan ID length | 6 characters | Output record position 5-10 |
| Trust Account length | 32 characters | Output record position 40-71 |
| Amount precision | 12 digits, 2 decimal places | Z,12V2- format |
| Maximum amount | 999,999,999,999.99 | Z,12V2- format capacity |
| Date lookback | 7 calendar days | Hardcoded in OcDate_AddDays |

### 3.2 Processing Constraints

| Constraint | Description |
|---|---|
| Sequential processing | POPP records are processed one at a time in view order |
| Single security | Only POOLLOAN3 is processed; no multi-security support |
| Single output file | All C1 records written to one file per run |
| Non-atomic update | File write and POPP update are not transactional |

---

## 4. Security and Compliance

### 4.1 Data Integrity

| Aspect | Status | Description |
|---|---|---|
| Idempotency | **IMPLEMENTED** | UDF1 (DE 877) prevents duplicate processing |
| Audit trail | **PARTIAL** | OcShow logging of filename and dates; no detailed transaction logging |
| Data validation | **MINIMAL** | RunDate validated; other inputs not validated |
| Reversal handling | **IMPLEMENTED** | CHECK.SSSA nets buys against sells |

### 4.2 Access Control

| Aspect | Status | Description |
|---|---|---|
| Database write access | Required | Program updates POPP DE 877 |
| File system access | Required | Program creates output files in $XDAT directory |
| Environment variable access | Required | Reads $XDAT and $RUN-DATE |

### 4.3 Compliance Considerations

| Area | Notes |
|---|---|
| Financial accuracy | The netting of buy/sell activity in CHECK.SSSA ensures accurate cash reconciliation amounts |
| Position 92 correction (GPD-1704) | Regulatory or system requirement to change value from 1 to 2; documented in change history |
| Reconciliation completeness | 7-day lookback window provides catch-up coverage for missed positions |

---

## 5. Business Rule Dependencies

```
BR-004 (POOLLOAN3 filter)
    └── BR-001 (7-day window)
        └── BR-002 (last business day)
            └── BR-005 (non-zero check)
                ├── BR-009 (CHECK.SSSA)
                │   ├── BR-010 (XI filter)
                │   ├── BR-011 (Buy accumulation)
                │   ├── BR-012 (Sell subtraction)
                │   └── BR-013 (Net replacement)
                └── BR-006 (idempotency check)
                    └── BR-007 (negation for offset)
                        ├── BR-014..BR-018 (output format)
                        └── BR-008 (update idempotency marker)
```

---

## 6. Business Rule Index

| Rule ID | Category | Brief Description |
|---|---|---|
| BR-001 | Date Range | 7 calendar day lookback window |
| BR-002 | Date Range | Last business day as activity date |
| BR-003 | Date Range | Fallback to current date if run date invalid |
| BR-004 | Filtering | POOLLOAN3 security only |
| BR-005 | Filtering | Skip zero buy amounts |
| BR-006 | Idempotency | Skip already-processed records |
| BR-007 | Calculation | Negate buy amount for offset |
| BR-008 | Idempotency | Update POPP UDF1 after processing |
| BR-009 | Reversal | Check SSSA for buy/sell netting |
| BR-010 | Reversal | XI transaction type filter |
| BR-011 | Reversal | Buy transactions increase net |
| BR-012 | Reversal | Sell transactions decrease net (reversal) |
| BR-013 | Reversal | Net SSSA amount replaces POPP value |
| BR-014 | Output | C100 record type |
| BR-015 | Output | Position 92 = '2' (GPD-1704) |
| BR-016 | Output | Transaction code 00339 |
| BR-017 | Output | Signed decimal amount format |
| BR-018 | Output | Processing flag = '0' |
