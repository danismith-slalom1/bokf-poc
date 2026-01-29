# GAP_NewLoanCash Call Graph

## Overview

This OmniScript program has a simple two-level call hierarchy:
- **Main Program**: Initialization and main processing loop
- **Subroutine**: CHECK.SSSA (called conditionally from main loop)

---

## Program Entry Points

### Main Program Entry
- **Entry Point**: Line 11 (program start after comment header)
- **Initialization Section**: Lines 11-26
- **Main Processing Loop**: Lines 28-52
- **Termination**: Implicit (end of script)

---

## Call Hierarchy Tree

```
MAIN PROGRAM (Lines 11-52)
│
├─── INITIALIZATION (Lines 11-26)
│    │
│    ├─── Set sd080 = 99999999 (Line 11)
│    ├─── Declare variables (Line 12)
│    ├─── Construct FileName (Lines 13-14)
│    ├─── Display FileName (Line 15)
│    ├─── Open output file (Line 16)
│    ├─── Read RunDate from environment (Line 18)
│    ├─── Validate and calculate dates (Lines 19-25)
│    └─── Display calculated dates (Line 26)
│
└─── MAIN PROCESSING LOOP (Lines 28-52)
     │
     ├─── Query POPP for POOLLOAN3 positions (Line 28)
     │
     └─── FOR EACH POPP RECORD (Lines 29-52)
          │
          ├─── Read position data (Lines 30-33)
          │    ├─── RKPlan (DE 030)
          │    ├─── TradeDate (DE 008)
          │    ├─── Secondary1Buys (DE 741)
          │    └─── PriorCashApplied (DE 877)
          │
          ├─── CONDITIONAL CALL → CHECK.SSSA (Lines 34-36)
          │    │  Condition: IF Secondary1Buys ≠ 0
          │    │  Purpose: Recalculate net amount from SSSA
          │    │  Effect: May modify Secondary1Buys
          │    │
          │    └─── CHECK.SSSA ROUTINE (Lines 54-70) ◄─┐
          │         │                                    │
          │         ├─── Validate inputs (Line 57)      │
          │         ├─── Initialize WK001 = 0 (Line 58) │
          │         ├─── Query SSSA (Line 58)           │
          │         │                                    │
          │         └─── FOR EACH SSSA RECORD (Lines 59-67) │
          │              │                               │
          │              ├─── IF DE011 = 'XI' (Line 60) │
          │              │    │                          │
          │              │    ├─── IF DE009 = 'B' (Line 61) │
          │              │    │    └─── WK001 += Amount (Line 63) │
          │              │    │                          │
          │              │    └─── IF DE009 = 'S' (Line 64) │
          │              │         └─── WK001 -= Amount (Line 66) │
          │              │                               │
          │              └─── Update Secondary1Buys = WK001 (Line 69) │
          │                   GOBACK to caller (Line 70) ────────────┘
          │
          ├─── CONDITIONAL PROCESSING (Lines 38-51)
          │    │  Condition: IF (PriorCashApplied ≠ Secondary1Buys)
          │    │             AND (Secondary1Buys ≠ 0)
          │    │
          │    ├─── Re-read position data (Lines 39-42)
          │    ├─── Calculate NewLoanUnits (Line 41)
          │    ├─── Build C1 record (Lines 43-48)
          │    ├─── Write to output file (Line 49)
          │    └─── Update POPP DE 877 (Lines 50-51)
          │
          └─── Continue to next POPP record (Line 52)
```

---

## Detailed Call Graph

### Level 1: Main Program

**Function**: Main processing logic  
**Location**: Lines 11-52  
**Calls**: CHECK.SSSA (conditionally)  
**Called By**: Program entry point

**Processing Flow**:
1. Initialize program variables and environment
2. Open output file
3. Query POPP database
4. FOR each position record:
   - Read position data
   - IF Secondary1Buys ≠ 0: CALL CHECK.SSSA
   - IF unprocessed and non-zero: Generate C1 activity
5. Implicit termination

**Exit Points**:
- Normal: End of loop (line 52)
- Error: Potential failure at file open (line 16) - no explicit handling

---

### Level 2: CHECK.SSSA Routine

**Function**: Recalculate Secondary1Buys from SSSA activity  
**Location**: Lines 54-70  
**Calls**: None  
**Called By**: Main loop (line 35)

**Processing Flow**:
1. Validate RKPlan and TradeDate
2. Initialize accumulator (WK001 = 0)
3. Query SSSA for matching transactions
4. FOR each SSSA record:
   - IF 'XI' indicator and 'B' type: Add amount
   - IF 'XI' indicator and 'S' type: Subtract amount
5. Update Secondary1Buys with net amount
6. GOBACK to caller

**Entry Condition**: Secondary1Buys ≠ 0  
**Exit Points**:
- Early exit: If RKPlan empty or TradeDate zero (line 57)
- Normal exit: GOBACK at line 70

---

## Call Frequency Analysis

### CHECK.SSSA Invocation Frequency

**Trigger**: For each POPP record where Secondary1Buys ≠ 0

**Typical Scenario** (7-day window):
- POPP records retrieved: 100-500 positions
- Positions with non-zero Secondary1Buys: 50-200 (50% typical)
- **CHECK.SSSA calls per run**: 50-200

**Worst Case Scenario**:
- POPP records retrieved: 10,000 positions (large date range or data error)
- Positions with non-zero Secondary1Buys: 10,000 (100%)
- **CHECK.SSSA calls per run**: 10,000 (nested query performance concern)

**Performance Impact**:
- Each CHECK.SSSA call executes one SSSA database query
- Nested loop pattern: O(N) SSSA queries where N = POPP records with non-zero buys
- SSSA query returns 1-10 records typically

---

## Parameter Passing

### CHECK.SSSA Parameters

**Implicit Parameters** (global variables):
- **RKPlan** (input): Plan identifier for SSSA query
- **TradeDate** (input): Trade date for SSSA query
- **Secondary1Buys** (input/output): Initial value from POPP, replaced with net SSSA amount

**No Formal Parameters**: OmniScript uses global variable scope

**Parameter Validation**:
- RKPlan: Must not be empty string
- TradeDate: Must not be zero
- Validation performed at line 57

---

## Control Flow Paths

### Path 1: Normal Processing with SSSA Adjustment
```
MAIN → Read POPP (Secondary1Buys = 50000)
    → CHECK.SSSA (recalculates to 40000)
    → Idempotency check (PriorCashApplied ≠ 40000)
    → Generate C1 activity for $40,000
    → Update POPP DE 877 = 40000
    → Continue to next record
```

### Path 2: Processing Without SSSA (Zero Amount)
```
MAIN → Read POPP (Secondary1Buys = 0)
    → Skip CHECK.SSSA (line 34 condition fails)
    → Idempotency check (line 38 condition fails: Secondary1Buys = 0)
    → Skip C1 generation
    → Continue to next record
```

### Path 3: Already Processed (Idempotency)
```
MAIN → Read POPP (Secondary1Buys = 50000, PriorCashApplied = 50000)
    → CHECK.SSSA (recalculates to 50000, no change)
    → Idempotency check (PriorCashApplied = Secondary1Buys)
    → Skip C1 generation (already processed)
    → Continue to next record
```

### Path 4: Full Reversal in SSSA
```
MAIN → Read POPP (Secondary1Buys = 50000)
    → CHECK.SSSA (recalculates to 0 due to full reversal)
    → Idempotency check (line 38 condition fails: Secondary1Buys = 0)
    → Skip C1 generation (net activity is zero)
    → Continue to next record
```

### Path 5: No POPP Records Found
```
MAIN → Query POPP (no matching records)
    → Loop executes zero times
    → Output file empty
    → Program terminates
```

---

## Recursion Analysis

**Recursion Present**: NO  
**Call Depth**: Maximum 2 levels (Main → CHECK.SSSA)  
**No Circular Dependencies**: CHECK.SSSA does not call back to main or itself

---

## Performance Call Graph

```
┌─────────────────────────────────────────────────────────┐
│ MAIN PROGRAM                                            │
│ Complexity: O(N) where N = POPP records                │
└────────────┬────────────────────────────────────────────┘
             │
             ├─── OcFile1_Open() [1 call per run]
             │    Cost: Low (file system operation)
             │
             ├─── poppobj_view() [1 call per run]
             │    Cost: Medium (database query with date range)
             │    Returns: N records
             │
             └─── FOR EACH of N records:
                  │
                  ├─── poppobj_de(), poppobj_numde() [4 reads per record]
                  │    Cost: Low (cursor reads)
                  │
                  ├─── CHECK.SSSA [~50% of records]
                  │    │
                  │    ├─── sssaobj_view() [1 query per call]
                  │    │    Cost: Medium (database query)
                  │    │    Returns: M records (typically 1-10)
                  │    │
                  │    └─── FOR EACH of M records:
                  │         │
                  │         └─── sssaobj_de(), sssaobj_numde() [2 reads per record]
                  │              Cost: Low (cursor reads)
                  │
                  ├─── OcText_Set() [8 calls per processed record]
                  │    Cost: Low (string operations)
                  │
                  ├─── OcFile1_Write() [1 call per processed record]
                  │    Cost: Low (sequential file write)
                  │
                  └─── poppobj_update() [1 call per processed record]
                       Cost: Medium (database update)

TOTAL COMPLEXITY: O(N * M) where:
- N = POPP records with Secondary1Buys ≠ 0 (typically 50-200)
- M = SSSA records per POPP record (typically 1-10)
- Worst case: N * M = 10,000 * 100 = 1,000,000 operations
```

---

## Call Graph Summary

| Routine | Type | Called By | Calls | Call Frequency | Performance Impact |
|---------|------|-----------|-------|---------------|-------------------|
| **MAIN PROGRAM** | Entry Point | System | CHECK.SSSA | Once per run | O(N) database reads |
| **CHECK.SSSA** | Subroutine | Main loop | None | 0 to N times | O(N * M) nested queries |

---

## Dependencies and Coupling

### External Dependencies
- **OmniScript Runtime**: All Oc*() functions
- **POPP Database**: poppobj_*() functions, requires read/write access
- **SSSA Database**: sssaobj_*() functions, requires read access
- **File System**: OcFile1_*() functions, requires write access to $XDAT directory
- **Environment Variables**: $XDAT, $RUN-DATE

### Database Objects Accessed
| Object | Operations | Data Elements |
|--------|-----------|---------------|
| **POPP** (Plan Position) | view, next, de, numde, setde, update | 030, 008, 741, 877, 01510 |
| **SSSA** (Settlement Activity) | view, next, de, numde | 009, 011, 235 |

### Coupling Analysis
- **Main → CHECK.SSSA**: Tight coupling via global variables (RKPlan, TradeDate, Secondary1Buys)
- **Main → POPP**: Tight coupling (direct database access)
- **CHECK.SSSA → SSSA**: Tight coupling (direct database access)
- **Main → File System**: Medium coupling (file I/O through OmniScript API)

---

## Error Propagation

### Error Flow Analysis

**File Open Failure** (Line 16):
```
Main → OcFile1_Open() [FAILS]
     → No error handling
     → Program continues with invalid file handle
     → File writes fail silently
```

**POPP Query Failure** (Line 28):
```
Main → poppobj_view() [FAILS]
     → No error handling
     → Loop may execute zero times or return errors
     → Silent failure possible
```

**SSSA Query Failure** (Line 58):
```
CHECK.SSSA → sssaobj_view() [FAILS]
           → No error handling
           → Loop may execute zero times
           → WK001 remains 0
           → Secondary1Buys incorrectly set to 0
           → Wrong C1 activity amount or skipped
```

**POPP Update Failure** (Line 51):
```
Main → poppobj_update() [FAILS]
     → No error handling
     → DE 877 not updated
     → Next run generates DUPLICATE C1 activity
     → Data integrity violation
```

---

## Optimization Recommendations

1. **Batch SSSA Queries**: 
   - Collect all (Plan, Date) pairs from POPP loop
   - Execute single query for all plans
   - Build lookup table
   - Eliminate nested query pattern
   - **Benefit**: Reduce database roundtrips from O(N) to O(1)

2. **Eliminate Redundant Reads**:
   - Remove duplicate reads of RKPlan and TradeDate (lines 39-40)
   - **Benefit**: Reduce cursor reads by 2 per processed record

3. **Add Early Exit**:
   - If POPP query returns zero records, log and exit
   - **Benefit**: Clear indication of no-data scenario vs query failure

4. **Add Call Depth Logging**:
   ```omniscript
   n.POPPCount = 0;
   n.SSSACallCount = 0;
   n.C1RecordCount = 0;
   /* ... track counts ... */
   OcShow('Processed: ' n.POPPCount ' POPP, ' n.SSSACallCount ' SSSA calls, ' n.C1RecordCount ' C1 records');
   ```

---

## Call Graph Metrics

| Metric | Value | Notes |
|--------|-------|-------|
| **Total Routines** | 2 | Main + CHECK.SSSA |
| **Call Depth** | 2 levels | Main → CHECK.SSSA |
| **Cyclomatic Complexity** | Low | Simple linear flow with conditionals |
| **Database Queries** | 1 + N | 1 POPP + N SSSA (where N ≤ POPP records) |
| **Database Updates** | M | M = processed records (where M ≤ POPP records) |
| **File Operations** | 1 + M | 1 open + M writes |

---

*AI-Generated Documentation - Review with OmniScript/COBOL experts for accuracy*
