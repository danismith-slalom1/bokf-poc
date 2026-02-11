# GAP_NEWLOANCASH Cross-Reference Documentation

## Overview

This document provides comprehensive cross-reference indexes for all program components, enabling developers to quickly locate information across all documentation artifacts.

---

## 1. Variable Index

| Variable | Type | Declared | Assigned | Read | Documentation |
|---|---|---|---|---|---|
| sd080 | Numeric | L13 | L13 | System | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#sd080) |
| FileName | String | L16 | L16 | L18, L19 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#filename) |
| RunDate | Numeric | L21 | L21 | L22, L23, L24 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#rundate) |
| SevenDaysAgo | Numeric | L23 | L23, L26 | L31 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#sevendaysago) |
| LastBusiness | Numeric | L24 | L24, L27 | L31, L47 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#lastbusiness) |
| RKPlan | String | L33 | L33, L41 | L46, L60, L62 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#rkplan) |
| TradeDate | Numeric | L34 | L34, L42 | L60, L62 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#tradedate) |
| Secondary1Buys | Numeric | L35 | L35, L73 | L37, L40, L43, L54 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#secondary1buys) |
| Secondary1Sells | Numeric | L14 | Never | Never | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#secondary1sells) |
| PriorCashApplied | Numeric | L36 | L36 | L40 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#priorcashapplied) |
| NewLoanUnits | Numeric | L43 | L43 | L51 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#newloanunits) |
| TrustAccount | String | L44 | L44 | L48 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#trustaccount) |
| Line | String | L14 | L45-L52 | L53 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#line) |
| WK001 | Numeric | L61 | L61, L66, L69 | L66, L69, L73 | [Data Dictionary](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#wk001-checksssa) |

---

## 2. Procedure Index

| Procedure | Purpose | Lines | Callers | Callees | Documentation |
|---|---|---|---|---|---|
| Main Program | Initialize, query POPP, process records, write C1 output | 13-57 | (entry point) | CHECK.SSSA | [MAIN_PROCESSING](./procedures/MAIN_PROCESSING.md) |
| CHECK.SSSA | Net buy/sell activity from SSSA records | 59-75 | Main Program (L38) | None | [CHECK_SSSA](./procedures/CHECK_SSSA.md) |

---

## 3. Database Operations Index

### POPP Object (poppobj)

| Operation | Line | Purpose | Documentation |
|---|---|---|---|
| `poppobj_view` | 31 | Open cursor for POOLLOAN3 positions in date range | [Call Graph](./GAP_NEWLOANCASH_CALL_GRAPH.md#database-call-sequence) |
| `poppobj_next` | 32 | Advance cursor (loop condition) | [Call Graph](./GAP_NEWLOANCASH_CALL_GRAPH.md#database-call-sequence) |
| `poppobj_de(030)` | 33, 41 | Read Plan ID | [Data Dictionary - RKPlan](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#rkplan) |
| `poppobj_numde(008)` | 34, 42 | Read Trade Date | [Data Dictionary - TradeDate](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#tradedate) |
| `poppobj_numde(741)` | 35 | Read Secondary1 Buys | [Data Dictionary - Secondary1Buys](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#secondary1buys) |
| `poppobj_numde(877)` | 36 | Read UDF1 (Prior Cash Applied) | [Data Dictionary - PriorCashApplied](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#priorcashapplied) |
| `poppobj_de(01510)` | 44 | Read Trust Account | [Data Dictionary - TrustAccount](./GAP_NEWLOANCASH_DATA_DICTIONARY.md#trustaccount) |
| `poppobj_setde(877)` | 54 | Write idempotency marker | [Error Handling - Atomicity](./GAP_NEWLOANCASH_ERROR_HANDLING.md#51-atomicity-concern) |
| `poppobj_update` | 55 | Persist record update | [Error Handling - Atomicity](./GAP_NEWLOANCASH_ERROR_HANDLING.md#51-atomicity-concern) |

### SSSA Object (sssaobj)

| Operation | Line | Purpose | Documentation |
|---|---|---|---|
| `sssaobj_view` | 62 | Open cursor for plan/security/date | [CHECK_SSSA](./procedures/CHECK_SSSA.md#database-operations) |
| `sssaobj_next` | 63 | Advance cursor (loop condition) | [CHECK_SSSA](./procedures/CHECK_SSSA.md#database-operations) |
| `sssaobj_de(011)` | 64 | Read transaction type (filter for 'XI') | [CHECK_SSSA](./procedures/CHECK_SSSA.md#sssa-data-elements) |
| `sssaobj_de(009)` | 65, 68 | Read buy/sell indicator | [CHECK_SSSA](./procedures/CHECK_SSSA.md#sssa-data-elements) |
| `sssaobj_numde(235)` | 66, 69 | Read transaction amount | [CHECK_SSSA](./procedures/CHECK_SSSA.md#sssa-data-elements) |

---

## 4. File Operations Index

| Operation | Line | File | Mode | Purpose | Documentation |
|---|---|---|---|---|---|
| `OcFile1_Open` | 19 | FileName (C1 activity file) | OUTPUT | Open output file for writing | [Integration Guide](./GAP_NEWLOANCASH_INTEGRATION_GUIDE.md#output-file) |
| `OcFile1_Write` | 53 | (same) | Write | Write C1 activity record | [MAIN_PROCESSING](./procedures/MAIN_PROCESSING.md#step-4-record-processing-loop) |

---

## 5. Business Rule Index

| Rule ID | Brief Description | Implementation Line(s) | Documentation |
|---|---|---|---|
| BR-001 | 7 calendar day lookback | 23, 26 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#11-date-range-rules) |
| BR-002 | Last business day as activity date | 24, 27 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#11-date-range-rules) |
| BR-003 | Fallback to current date | 22-28 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#11-date-range-rules) |
| BR-004 | POOLLOAN3 security filter | 31 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#11-date-range-rules) |
| BR-005 | Skip zero buy amounts | 37, 40 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#12-transaction-processing-rules) |
| BR-006 | Idempotency check | 40 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#12-transaction-processing-rules) |
| BR-007 | Negate for offset | 43 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#12-transaction-processing-rules) |
| BR-008 | Update idempotency marker | 54 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#12-transaction-processing-rules) |
| BR-009 | Check SSSA for reversals | 37-39 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#13-reversalnetting-rules) |
| BR-010 | XI transaction type filter | 64 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#13-reversalnetting-rules) |
| BR-011 | Buy accumulation | 65-67 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#13-reversalnetting-rules) |
| BR-012 | Sell/reversal subtraction | 68-70 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#13-reversalnetting-rules) |
| BR-013 | Net replaces original | 73 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#13-reversalnetting-rules) |
| BR-014 | C100 record type | 45 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#14-output-format-rules) |
| BR-015 | Position 92 = '2' | 49 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#14-output-format-rules) |
| BR-016 | Transaction code 00339 | 52 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#14-output-format-rules) |
| BR-017 | Signed decimal format | 51 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#14-output-format-rules) |
| BR-018 | Processing flag = '0' | 50 | [Business Rules](./GAP_NEWLOANCASH_BUSINESS_RULES.md#14-output-format-rules) |

---

## 6. Error Handling Index

| Error Condition | Line | Handling | Risk Level | Documentation |
|---|---|---|---|---|
| Invalid RunDate | 22-28 | Fallback to current date | LOW | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#11-implemented-error-handling) |
| File open failure | 19 | None | HIGH | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#12-missing-error-handling) |
| File write failure | 53 | None | HIGH | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#12-missing-error-handling) |
| POPP view failure | 31 | None | MEDIUM | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#12-missing-error-handling) |
| SSSA view failure | 62 | None | MEDIUM | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#12-missing-error-handling) |
| POPP update failure | 54-55 | None | HIGH | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#12-missing-error-handling) |
| Missing $XDAT | 16 | None | MEDIUM | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#12-missing-error-handling) |
| Empty RKPlan | 60 | CHECK.SSSA skips | LOW | [CHECK_SSSA](./procedures/CHECK_SSSA.md#error-handling) |
| TradeDate = 0 | 60 | CHECK.SSSA skips | LOW | [CHECK_SSSA](./procedures/CHECK_SSSA.md#error-handling) |
| Non-atomic write/update | 53-55 | None (gap) | HIGH | [Error Handling](./GAP_NEWLOANCASH_ERROR_HANDLING.md#51-atomicity-concern) |

---

## 7. Built-in Function Index

| Function | Lines Used | Category | Purpose |
|---|---|---|---|
| `OcLVar_Define` | 14-15 | Variable | Declare local variables |
| `OcText_string` | 16 | String | Concatenate strings |
| `OCTEXT_GETENV` | 16 | Environment | Read environment variable |
| `octext_getenv` | 21 | Environment | Read environment variable (lowercase) |
| `octext_tonum` | 21 | Conversion | Text to numeric conversion |
| `OcFmt` / `OcFMT` | 17, 47, 51 | Formatting | Format numeric to string |
| `OcDate_Current` | 17, 26, 27 | Date | Get current system date |
| `OcTime_Current` | 17 | Time | Get current system time |
| `OcDate_Valid` | 22 | Date | Validate date value |
| `OcDate_AddDays` | 23, 26 | Date | Add calendar days |
| `OcDate_AddBusDays` | 24, 27 | Date | Add business days |
| `OcShow` | 18, 29 | Display | Log/display values |
| `OcFile1_Open` | 19 | File I/O | Open file |
| `OcFile1_Write` | 53 | File I/O | Write to file |
| `OcText_Set` | 45-52 | String | Set text at position |
