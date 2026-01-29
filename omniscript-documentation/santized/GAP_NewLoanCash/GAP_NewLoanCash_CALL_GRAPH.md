# GAP_NewLoanCash Call Graph

## Program Overview
- **Program Name**: GAP_NewLoanCash
- **Purpose**: Process new loan cash offset activity for C1 reconciliation
- **Main Entry Point**: Program initialization (line 11)
- **Primary Loop**: POPP record processing (lines 28-51)
- **Subroutines**: 1 (CHECK.SSSA)

---

## Call Hierarchy Diagram

```
Program Start (Line 11)
│
├─ Variable Initialization (Lines 11-14)
│  ├─ OcText_string() - Construct filename
│  ├─ OCTEXT_GETENV('$XDAT') - Get environment variable
│  ├─ OcFMT() - Format date/time
│  ├─ OcDate_Current() - Get current date
│  └─ OcTime_Current() - Get current time
│
├─ OcShow(FileName) - Display filename (Line 15)
│
├─ OcFile1_Open() - Open output file (Line 16)
│
├─ Date Calculation (Lines 18-26)
│  ├─ octext_tonum() - Convert environment variable
│  ├─ octext_getenv('$RUN-DATE') - Get run date
│  ├─ OcDate_Valid() - Validate date (Line 19)
│  ├─ OcDate_AddDays() - Calculate 7 days ago (Lines 20, 23)
│  ├─ OcDate_AddBusDays() - Calculate last business day (Lines 21, 24)
│  └─ OcShow() - Display dates (Line 26)
│
├─ Main Processing Loop (Lines 28-51)
│  │
│  ├─ poppobj_view() - Query POPP records (Line 28)
│  │
│  └─ loop while poppobj_next() (Line 29)
│     │
│     ├─ poppobj_de(030) - Get RKPlan (Line 30)
│     ├─ poppobj_numde(008) - Get TradeDate (Line 31)
│     ├─ poppobj_numde(741) - Get Secondary1Buys (Line 32)
│     ├─ poppobj_numde(877) - Get PriorCashApplied (Line 33)
│     │
│     ├─ IF Secondary1Buys <> 0 (Line 33)
│     │  └─ PERFORM 'CHECK.SSSA' ────┐ (Line 34)
│     │                               │
│     ├─ IF (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0) (Line 36)
│     │  │
│     │  ├─ poppobj_de(030) - Get RKPlan again (Line 37)
│     │  ├─ poppobj_numde(008) - Get TradeDate again (Line 38)
│     │  ├─ Calculate NewLoanUnits (Line 39)
│     │  ├─ poppobj_de(01510) - Get TrustAccount (Line 40)
│     │  │
│     │  ├─ Format C1 Record (Lines 41-47)
│     │  │  ├─ OcText_Set() - Set record type (Line 41)
│     │  │  ├─ OcText_Set() - Set RKPlan (Line 42)
│     │  │  ├─ OcText_Set() - Set LastBusiness date (Line 43)
│     │  │  │  └─ OcFmt() - Format date
│     │  │  ├─ OcText_Set() - Set TrustAccount (Line 44)
│     │  │  ├─ OcText_Set() - Set position indicator (Line 45)
│     │  │  ├─ OcText_Set() - Set flag (Line 46)
│     │  │  ├─ OcText_Set() - Set NewLoanUnits (Line 47)
│     │  │  │  └─ OcFmt() - Format amount
│     │  │  └─ OcText_Set() - Set activity code (Line 48)
│     │  │
│     │  ├─ OcFile1_Write(Line) - Write C1 record (Line 49)
│     │  ├─ poppobj_setde(877, Secondary1Buys) - Update UDF1 (Line 50)
│     │  └─ poppobj_update() - Commit POPP update (Line 51)
│     │
│     └─ endloop (Line 52)
│
└─ Program End (Line 70 - implicit)
│
│
┌───────────────────────────────────────────────────────────┐
│                                                           │
│  SUBROUTINE: CHECK.SSSA (Lines 55-69)                   │
│  Called from: Line 34                                     │
│  Call condition: Secondary1Buys <> 0                     │
│                                                           │
│  ├─ IF (RKPlan <> '') and (TradeDate <> 0) (Line 56)    │
│  │  │                                                     │
│  │  ├─ WK001 = 0 - Initialize accumulator (Line 56)     │
│  │  │                                                     │
│  │  ├─ sssaobj_view() - Query SSSA (Line 57)            │
│  │  │  Parameters: PLAN, SECURITYID:'POOLLOAN3', DATE   │
│  │  │                                                     │
│  │  └─ loop while sssaobj_next() (Line 58)              │
│  │     │                                                  │
│  │     ├─ sssaobj_de(011) - Get activity code           │
│  │     │                                                  │
│  │     ├─ IF activity code = 'XI' (Line 59)             │
│  │     │  │                                               │
│  │     │  ├─ IF sssaobj_de(009) = 'B' (Line 60)         │
│  │     │  │  └─ WK001 = WK001 + sssaobj_numde(235)      │
│  │     │  │     (Add buy transactions)                   │
│  │     │  │                                               │
│  │     │  └─ IF sssaobj_de(009) = 'S' (Line 63)         │
│  │     │     └─ WK001 = WK001 - sssaobj_numde(235)      │
│  │     │        (Subtract sell/reversal transactions)    │
│  │     │                                                  │
│  │     └─ endloop (Line 66)                              │
│  │                                                        │
│  ├─ Secondary1Buys = WK001 - Update result (Line 68)    │
│  │                                                        │
│  └─ GOBACK - Return to caller (Line 69)                 │
│                                                           │
└───────────────────────────────────────────────────────────┘
```

---

## Execution Flow Summary

### Phase 1: Initialization (Lines 11-16)
1. Set system variable `sd080 = 99999999`
2. Define all program variables with OcLVar_Define
3. Construct output filename using environment variables and current date/time
4. Display filename
5. Open output file for writing

### Phase 2: Date Range Calculation (Lines 18-26)
1. Retrieve run date from `$RUN-DATE` environment variable
2. Validate run date
3. Calculate date range:
   - **Valid run date**: Use RunDate for calculations
   - **Invalid run date**: Use current date as fallback
4. Calculate `SevenDaysAgo` (7 calendar days before run date)
5. Calculate `LastBusiness` (1 business day before run date)
6. Display calculated dates

### Phase 3: Main Processing Loop (Lines 28-51)
1. Query POPP database for:
   - Security: 'POOLLOAN3'
   - Date range: SevenDaysAgo to LastBusiness
2. For each POPP record:
   a. Extract plan, trade date, and loan amounts
   b. **IF Secondary1Buys > 0**: Call CHECK.SSSA to verify/adjust amount
   c. **IF amount differs from prior processing AND amount > 0**:
      - Construct formatted C1 activity record
      - Write record to output file
      - Update POPP field 877 with processed amount
      - Commit POPP update

### Phase 4: SSSA Verification (CHECK.SSSA Routine, Lines 55-69)
1. Validate input parameters (RKPlan and TradeDate)
2. Query SSSA database for matching plan/security/date
3. Loop through SSSA records:
   - Filter for 'XI' activity type
   - Accumulate buy transactions (add amounts)
   - Accumulate sell/reversal transactions (subtract amounts)
4. Update Secondary1Buys with net amount
5. Return to main processing loop

### Phase 5: Program Termination (Implicit)
- No explicit cleanup or file close operations
- OmniScript runtime handles file closure

---

## Loop Structures

### Loop 1: Main POPP Processing (Lines 28-52)
- **Type**: Database iterator loop
- **Condition**: `while poppobj_next()`
- **Iteration**: One iteration per POPP record in date range
- **Typical Iterations**: Varies (7 days of position records)
- **Termination**: When no more POPP records to process
- **Exit Point**: Line 52 (endloop)

### Loop 2: SSSA Verification (Lines 58-66)
- **Type**: Database iterator loop (nested within CHECK.SSSA routine)
- **Condition**: `while sssaobj_next()`
- **Iteration**: One iteration per matching SSSA record
- **Typical Iterations**: 1-10 per plan/date combination
- **Termination**: When no more SSSA records to process
- **Exit Point**: Line 66 (endloop)
- **Context**: Called conditionally from main loop

---

## Conditional Execution Paths

### Path 1: CHECK.SSSA Invocation (Line 33-34)
**Condition**: `If (Secondary1Buys <> 0)`
- **True Path**: Execute CHECK.SSSA routine to verify amount
- **False Path**: Skip SSSA verification (no loan activity)
- **Business Logic**: Only verify amounts when loan activity exists

### Path 2: C1 Record Generation (Line 36-51)
**Condition**: `if (PriorCashApplied <> Secondary1Buys) and (Secondary1Buys <> 0)`
- **True Path**: Generate C1 record and update POPP
- **False Path**: Skip processing (already processed or no activity)
- **Business Logic**: Idempotency check - only process new or changed amounts

### Path 3: Date Calculation (Lines 19-25)
**Condition**: `if OcDate_Valid(RunDate)`
- **True Path**: Use RunDate from environment for calculations
- **False Path**: Use OcDate_Current() as fallback
- **Business Logic**: Graceful fallback if environment variable missing or invalid

### Path 4: SSSA Processing (Line 56)
**Condition**: `if (RKPlan <> '') and (TradeDate <> 0)`
- **True Path**: Query SSSA and process records
- **False Path**: Return immediately without processing
- **Business Logic**: Input validation for SSSA query

### Path 5: Buy Transaction Accumulation (Lines 60-62)
**Condition**: `if sssaobj_de(009) = 'B'`
- **True Path**: Add transaction amount to accumulator
- **False Path**: Check for sell transaction
- **Business Logic**: Buy transactions increase loan amounts

### Path 6: Sell Transaction Accumulation (Lines 63-65)
**Condition**: `if sssaobj_de(009) = 'S'`
- **True Path**: Subtract transaction amount from accumulator
- **False Path**: Skip transaction
- **Business Logic**: Sell transactions decrease loan amounts (reversals)

### Path 7: Activity Type Filter (Line 59)
**Condition**: `if sssaobj_de(011) = 'XI'`
- **True Path**: Process buy/sell transactions
- **False Path**: Skip non-XI activity types
- **Business Logic**: Only 'XI' activity is relevant for loan cash reconciliation

---

## Database Operations Summary

### Read Operations
1. **POPP Query** (Line 28): Retrieve position records for POOLLOAN3 in date range
   - Fields Read: 030, 008, 741, 877, 01510
2. **POPP Record Iteration** (Line 29): Fetch next record
3. **SSSA Query** (Line 57): Retrieve settlement activity for plan/security/date
   - Fields Read: 009, 011, 235
4. **SSSA Record Iteration** (Line 58): Fetch next record

### Write Operations
1. **File Write** (Line 49): Write formatted C1 record to output file
2. **POPP Update** (Lines 50-51): Update field 877 with processed amount and commit

---

## External Function Calls

### OmniScript Framework Functions
- **Date/Time Functions**: OcDate_Current, OcTime_Current, OcDate_Valid, OcDate_AddDays, OcDate_AddBusDays
- **Text Functions**: OcText_string, OcText_Set, octext_tonum, octext_getenv
- **Formatting Functions**: OcFmt, OCFMT
- **Display Functions**: OcShow
- **File Functions**: OcFile1_Open, OcFile1_Write
- **Database Functions**: poppobj_view, poppobj_next, poppobj_de, poppobj_numde, poppobj_setde, poppobj_update
- **SSSA Functions**: sssaobj_view, sssaobj_next, sssaobj_de, sssaobj_numde
- **Variable Functions**: OcLVar_Define

---

## Entry and Exit Points

### Entry Point
- **Line 11**: Program starts with `sd080 = 99999999` initialization
- **Invocation**: Called by OmniScript scheduler or manually

### Exit Points
- **Implicit**: Program ends after main loop completion (Line 52)
- **No explicit EXIT or STOP statements**
- **CHECK.SSSA Exit**: Line 69 (GOBACK returns to caller)

---

## Procedure Call Relationships

### Main Program
- **Calls**: CHECK.SSSA (conditionally, line 34)
- **Called By**: System/Scheduler
- **Execution Context**: Main program thread

### CHECK.SSSA Routine
- **Calls**: None (leaf procedure)
- **Called By**: Main program loop (line 34)
- **Execution Context**: Synchronous subroutine call
- **Call Frequency**: Once per POPP record where Secondary1Buys > 0

---

## Nested Call Depth

**Maximum Call Depth**: 2 levels
- Level 0: System/Scheduler → GAP_NewLoanCash (Main Program)
- Level 1: Main Program → CHECK.SSSA

**Call Graph Complexity**: Low (single subroutine, no recursion)

---

## Parallelization Opportunities

### Not Parallelizable
- **Main POPP Loop**: Must process sequentially due to database updates (poppobj_update)
- **SSSA Verification**: Depends on POPP record data (called per record)

### Theoretically Parallelizable
- **Date Calculations**: Independent operations, but trivial overhead
- **Multiple Plans**: If processing multiple plans, could partition by plan (requires architectural changes)

**Current Architecture**: Single-threaded sequential processing (standard for OmniScript)

---

## Error Propagation

### CHECK.SSSA Errors
- **Behavior**: If CHECK.SSSA fails silently (e.g., database error), caller may use incorrect Secondary1Buys value
- **Impact**: Could generate incorrect C1 records
- **Mitigation**: No explicit error handling; relies on OmniScript runtime exceptions

### Database Query Errors
- **Behavior**: poppobj_view or sssaobj_view failures would likely terminate program
- **Impact**: No C1 records generated for this run
- **Mitigation**: OmniScript runtime error handling (not in script)

### File Write Errors
- **Behavior**: OcFile1_Write failure would likely terminate program
- **Impact**: Incomplete output file, POPP may be partially updated
- **Mitigation**: No transactional rollback mechanism evident in code

---

## Performance Characteristics

### Database Operations
- **POPP Query**: Single query at start, then iterator (efficient)
- **SSSA Queries**: One per POPP record with Secondary1Buys > 0 (could be many)
- **Bottleneck**: SSSA queries if many POPP records have loan activity

### File I/O
- **Output File**: Sequential writes, one per processed record (efficient)
- **Buffering**: Assumed handled by OcFile1_Write

### Overall Performance
- **Time Complexity**: O(P * S) where P = POPP records, S = avg SSSA records per POPP
- **Typical Runtime**: Depends on 7-day data volume
- **Optimization**: Could batch SSSA queries or use database joins (requires framework support)

---

## Call Graph Dependencies

### Required External Systems
1. **POPP Database**: Must be accessible and contain current position data
2. **SSSA Database**: Must be accessible and contain settlement activity data
3. **File System**: $XDAT directory must be writable
4. **Environment Variables**: $XDAT and $RUN-DATE must be set

### Data Dependencies
- **CHECK.SSSA depends on**: RKPlan, TradeDate from POPP record
- **C1 record generation depends on**: Secondary1Buys (potentially modified by CHECK.SSSA)

---

**AI-Generated Documentation Notice**: This call graph was generated using AI analysis and should be reviewed by OmniScript experts for accuracy.

**Last Updated**: 2026-01-23
**Program Version**: Includes GPD-1704 correction and reversal handling (09/25/2024)
