# GAP_NEWLOANCASH Call Graph

## Overview

This document maps all procedure call relationships, control flow paths, and execution order within the GAP_NewLoanCash program. The program has a simple call structure with one main processing flow and one subroutine.

---

## Main Program Flow

```
1. MAIN PROGRAM (Lines 13-57)
   ├── INITIALIZATION (Lines 13-19)
   │   ├── Set sd080 = 99999999
   │   ├── OcLVar_Define (declare all variables)
   │   ├── Construct FileName from environment
   │   ├── OcShow(FileName)
   │   └── OcFile1_Open(FileName, OUTPUT)
   │
   ├── DATE RANGE CALCULATION (Lines 21-29)
   │   ├── Read RunDate from $RUN-DATE
   │   ├── IF OcDate_Valid(RunDate)
   │   │   ├── SevenDaysAgo = RunDate - 7 days
   │   │   └── LastBusiness = RunDate - 1 business day
   │   └── ELSE
   │       ├── SevenDaysAgo = CurrentDate - 7 days
   │       └── LastBusiness = CurrentDate - 1 business day
   │   └── OcShow(RunDate, SevenDaysAgo, LastBusiness)
   │
   ├── RECORD PROCESSING LOOP (Lines 31-57)
   │   ├── poppobj_view(POOLLOAN3, SevenDaysAgo..LastBusiness)
   │   └── LOOP WHILE poppobj_next()
   │       ├── Read: RKPlan, TradeDate, Secondary1Buys, PriorCashApplied
   │       ├── IF Secondary1Buys <> 0
   │       │   └── PERFORM 'CHECK.SSSA' ──────────────────┐
   │       └── IF PriorCashApplied <> Secondary1Buys      │
   │           AND Secondary1Buys <> 0                     │
   │           ├── Re-read: RKPlan, TradeDate              │
   │           ├── Calculate: NewLoanUnits = 0 - Secondary1Buys
   │           ├── Read: TrustAccount                      │
   │           ├── Build Line (C1 record)                  │
   │           ├── OcFile1_Write(Line)                     │
   │           ├── poppobj_setde(877, Secondary1Buys)      │
   │           └── poppobj_update()                        │
   │                                                       │
2. ROUTINE 'CHECK.SSSA' (Lines 59-75) ◄───────────────────┘
   ├── IF RKPlan <> '' AND TradeDate <> 0
   │   ├── WK001 = 0
   │   ├── sssaobj_view(Plan:RKPlan, SecurityID:POOLLOAN3, Date:TradeDate)
   │   └── LOOP WHILE sssaobj_next()
   │       └── IF sssaobj_de(011) = 'XI'
   │           ├── IF sssaobj_de(009) = 'B' (Buy)
   │           │   └── WK001 = WK001 + sssaobj_numde(235)
   │           └── IF sssaobj_de(009) = 'S' (Sell/Reversal)
   │               └── WK001 = WK001 - sssaobj_numde(235)
   │   └── Secondary1Buys = WK001
   └── GOBACK
```

---

## Call Hierarchy Summary

| Caller | Callee | Line | Condition | Frequency |
|---|---|---|---|---|
| Main Program | CHECK.SSSA | 38 | `Secondary1Buys <> 0` | Once per qualifying POPP record |

---

## Database Call Sequence

| Order | Operation | Line | Object | Parameters |
|---|---|---|---|---|
| 1 | `poppobj_view` | 31 | POPP | securityid:POOLLOAN3, datelo:SevenDaysAgo, datehi:LastBusiness |
| 2 | `poppobj_next` | 32 | POPP | (cursor advance, loop condition) |
| 3 | `poppobj_de(030)` | 33 | POPP | Read RKPlan |
| 4 | `poppobj_numde(008)` | 34 | POPP | Read TradeDate |
| 5 | `poppobj_numde(741)` | 35 | POPP | Read Secondary1Buys |
| 6 | `poppobj_numde(877)` | 36 | POPP | Read PriorCashApplied |
| 7 | `sssaobj_view` | 62 | SSSA | Plan:RKPlan, SecurityID:POOLLOAN3, Date:TradeDate |
| 8 | `sssaobj_next` | 63 | SSSA | (cursor advance, loop condition) |
| 9 | `sssaobj_de(011)` | 64 | SSSA | Read transaction type |
| 10 | `sssaobj_de(009)` | 65, 68 | SSSA | Read buy/sell indicator |
| 11 | `sssaobj_numde(235)` | 66, 69 | SSSA | Read amount |
| 12 | `poppobj_de(030)` | 41 | POPP | Re-read RKPlan |
| 13 | `poppobj_numde(008)` | 42 | POPP | Re-read TradeDate |
| 14 | `poppobj_de(01510)` | 44 | POPP | Read TrustAccount |
| 15 | `poppobj_setde(877)` | 54 | POPP | Write Secondary1Buys to UDF1 |
| 16 | `poppobj_update` | 55 | POPP | Persist POPP record update |

---

## Loop Structures

| Loop | Location | Condition | Nesting |
|---|---|---|---|
| Main POPP loop | Lines 32-57 | `poppobj_next()` (until no more records) | Top level |
| SSSA sub-loop | Lines 63-72 | `sssaobj_next()` (until no more records) | Inside CHECK.SSSA, called from main loop |

---

## Conditional Execution Paths

| Condition | Line | True Path | False Path |
|---|---|---|---|
| `OcDate_Valid(RunDate)` | 22 | Use RunDate for date calc | Use OcDate_Current() for date calc |
| `Secondary1Buys <> 0` | 37 | PERFORM CHECK.SSSA | Skip SSSA check |
| `PriorCashApplied <> Secondary1Buys AND Secondary1Buys <> 0` | 40 | Build and write C1 record, update POPP | Skip (already processed or zero) |
| `RKPlan <> '' AND TradeDate <> 0` | 60 | Execute SSSA lookup | Skip (invalid key data) |
| `sssaobj_de(011) = 'XI'` | 64 | Process transaction | Skip non-XI transactions |
| `sssaobj_de(009) = 'B'` | 65 | Add amount to WK001 | Check for sell |
| `sssaobj_de(009) = 'S'` | 68 | Subtract amount from WK001 | Skip (neither buy nor sell) |

---

## Exit Points

| Exit | Type | Condition |
|---|---|---|
| Main program end | Normal termination | After POPP loop exhausts all records (Line 57) |
| CHECK.SSSA GOBACK | Return to caller | Line 75 |
