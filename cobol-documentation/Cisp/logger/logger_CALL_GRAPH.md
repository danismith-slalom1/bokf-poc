# LOGGER Call Graph

**Program**: LOGGER  
**Repository**: lauryndbrown/Cisp  
**Last Updated**: January 20, 2026  
**Purpose**: Visual representation of control flow and PERFORM hierarchy in the LOGGER program

---

## Overview

The LOGGER program implements a simple **dispatcher pattern** with no nested PERFORM calls. The MAIN-PROCEDURE evaluates an operation flag and performs exactly one handler procedure based on the flag value. There are no recursive calls, no loops with PERFORMs, and no conditional PERFORM statements.

**Control Flow Characteristics**:
- **Entry Point**: PROCEDURE DIVISION USING (called by external programs)
- **Dispatcher**: MAIN-PROCEDURE (EVALUATE-based dispatch)
- **Handlers**: Four leaf-level procedures (no further PERFORMs)
- **Exit Point**: GOBACK (returns to calling program)

---

## Call Graph Diagram

### Text-Based Call Graph

```
External Calling Program
    ↓ [CALL or LINKAGE]
PROCEDURE DIVISION USING LS-LOG-OPERATION-FLAG, LS-LOG-RECORD
    ↓
MAIN-PROCEDURE
    ├─[WHEN "OPEN"]────→ PERFORM LOG-INIT-PROCEDURE
    ├─[WHEN "CLOSE"]───→ PERFORM LOG-CLOSE-PROCEDURE
    ├─[WHEN "ADD"]─────→ PERFORM LOG-WRITE-TO-PROCEDURE
    └─[WHEN OTHER]─────→ PERFORM LOG-FLAG-ERROR-PROCEDURE
    ↓
GOBACK (return to caller)
```

### Visual Call Hierarchy

```
                    ┌──────────────────────────────────┐
                    │   External Calling Program       │
                    └────────────┬─────────────────────┘
                                 │ CALL/LINKAGE
                    ┌────────────▼─────────────────────┐
                    │      MAIN-PROCEDURE              │
                    │   (EVALUATE dispatcher)          │
                    └────────────┬─────────────────────┘
                                 │
               ┌─────────────────┼─────────────────┐
               │                 │                 │
    ┌──────────▼────────┐   ┌───▼────────┐   ┌───▼────────────┐   ┌────────────────┐
    │ LOG-INIT-PROCEDURE│   │LOG-CLOSE   │   │LOG-WRITE-TO    │   │LOG-FLAG-ERROR  │
    │   (WHEN "OPEN")   │   │PROCEDURE   │   │PROCEDURE       │   │PROCEDURE       │
    │                   │   │(WHEN       │   │(WHEN "ADD")    │   │(WHEN OTHER)    │
    │ • OPEN file       │   │"CLOSE")    │   │                │   │                │
    │ • WRITE init rec  │   │            │   │ • ADD to ID    │   │ • DISPLAY err  │
    │                   │   │ • WRITE rec│   │ • MOVE data    │   │                │
    │                   │   │ • CLOSE    │   │ • WRITE rec    │   │                │
    └───────────────────┘   └────────────┘   └────────────────┘   └────────────────┘
             │                      │                  │                     │
             └──────────────────────┴──────────────────┴─────────────────────┘
                                    │
                            ┌───────▼────────┐
                            │    GOBACK      │
                            └────────────────┘
```

---

## Call Graph Details

### MAIN-PROCEDURE (Entry Point)

**Purpose**: Dispatcher that routes to appropriate handler based on operation flag

**Calls To**:
1. **LOG-INIT-PROCEDURE** - Condition: `LS-LOG-OPERATION-FLAG = "OPEN"`
2. **LOG-CLOSE-PROCEDURE** - Condition: `LS-LOG-OPERATION-FLAG = "CLOSE"`
3. **LOG-WRITE-TO-PROCEDURE** - Condition: `LS-LOG-OPERATION-FLAG = "ADD"`
4. **LOG-FLAG-ERROR-PROCEDURE** - Condition: `LS-LOG-OPERATION-FLAG = OTHER`

**Called By**: External programs via LINKAGE SECTION interface

**Call Type**: Sequential (one and only one handler performed per invocation)

**Return**: GOBACK (returns control to calling program)

---

### LOG-INIT-PROCEDURE (Handler)

**Purpose**: Initialize logging session and write first entry

**Calls To**: None (leaf-level procedure)

**Called By**: MAIN-PROCEDURE when flag = "OPEN"

**Operations**:
1. MOVE file path to WS-LOG-FILE-NAME
2. OPEN OUTPUT LOG-FILE
3. MOVE 1 to LOG-RECORD-ID
4. MOVE "LOG-INIT-PROCEDURE" to LOG-RECORD-FUNCTION-NAME
5. MOVE "Starting Program!" to LOG-RECORD-MESSAGE
6. WRITE LOG-RECORD

**Side Effects**: Opens file, writes first record

---

### LOG-WRITE-TO-PROCEDURE (Handler)

**Purpose**: Append log entry to file

**Calls To**: None (leaf-level procedure)

**Called By**: MAIN-PROCEDURE when flag = "ADD"

**Operations**:
1. ADD 1 to LOG-RECORD-ID
2. MOVE LS-LOG-RECORD-FUNCTION-NAME to LOG-RECORD-FUNCTION-NAME
3. MOVE LS-LOG-RECORD-MESSAGE to LOG-RECORD-MESSAGE
4. WRITE LOG-RECORD

**Side Effects**: Increments counter, writes record

---

### LOG-CLOSE-PROCEDURE (Handler)

**Purpose**: Write closing entry and close file

**Calls To**: None (leaf-level procedure)

**Called By**: MAIN-PROCEDURE when flag = "CLOSE"

**Operations**:
1. ADD 1 to LOG-RECORD-ID
2. MOVE "LOGGER:LOG-CLOSE-PROCEDURE" to LOG-RECORD-FUNCTION-NAME
3. MOVE "Closed logging file" to LOG-RECORD-MESSAGE
4. WRITE LOG-RECORD
5. CLOSE LOG-FILE

**Side Effects**: Increments counter, writes record, closes file

---

### LOG-FLAG-ERROR-PROCEDURE (Handler)

**Purpose**: Handle invalid operation flags

**Calls To**: None (leaf-level procedure)

**Called By**: MAIN-PROCEDURE when flag is invalid

**Operations**:
1. DISPLAY "READ FLAG ERROR"

**Side Effects**: Console output only

---

## Execution Patterns

### Pattern 1: Normal Logging Session

```
Call Sequence:
1. MAIN-PROCEDURE → LOG-INIT-PROCEDURE      [LS-LOG-OPERATION-FLAG = "OPEN"]
2. MAIN-PROCEDURE → LOG-WRITE-TO-PROCEDURE  [LS-LOG-OPERATION-FLAG = "ADD"] (repeated 0-N times)
3. MAIN-PROCEDURE → LOG-CLOSE-PROCEDURE     [LS-LOG-OPERATION-FLAG = "CLOSE"]

Example:
CALL LOGGER("OPEN", ...)       → MAIN-PROCEDURE → LOG-INIT-PROCEDURE
CALL LOGGER("ADD", record1)    → MAIN-PROCEDURE → LOG-WRITE-TO-PROCEDURE
CALL LOGGER("ADD", record2)    → MAIN-PROCEDURE → LOG-WRITE-TO-PROCEDURE
CALL LOGGER("ADD", record3)    → MAIN-PROCEDURE → LOG-WRITE-TO-PROCEDURE
CALL LOGGER("CLOSE", ...)      → MAIN-PROCEDURE → LOG-CLOSE-PROCEDURE
```

### Pattern 2: Error Handling

```
Call Sequence:
1. MAIN-PROCEDURE → LOG-FLAG-ERROR-PROCEDURE [LS-LOG-OPERATION-FLAG = invalid]

Example:
CALL LOGGER("INVALID", ...) → MAIN-PROCEDURE → LOG-FLAG-ERROR-PROCEDURE
CALL LOGGER("open", ...)    → MAIN-PROCEDURE → LOG-FLAG-ERROR-PROCEDURE  [lowercase]
CALL LOGGER("DELETE", ...)  → MAIN-PROCEDURE → LOG-FLAG-ERROR-PROCEDURE  [unsupported]
```

### Pattern 3: Empty Log Session

```
Call Sequence:
1. MAIN-PROCEDURE → LOG-INIT-PROCEDURE      [LS-LOG-OPERATION-FLAG = "OPEN"]
2. MAIN-PROCEDURE → LOG-CLOSE-PROCEDURE     [LS-LOG-OPERATION-FLAG = "CLOSE"]

Result: Log file with 2 entries (init + close, no ADD operations)
```

---

## Control Flow Properties

### No Recursion
- **No procedure calls itself** (directly or indirectly)
- **No circular dependencies** in call graph
- **Safe**: No stack overflow risk

### No Nested PERFORMs
- **Single-level hierarchy**: MAIN-PROCEDURE → Handler
- **No handler calls other handlers**
- **Simplicity**: Easy to trace execution flow

### Mutually Exclusive Paths
- **EVALUATE ensures exactly one handler executes** per invocation
- **No parallel execution**
- **Deterministic**: Behavior fully determined by operation flag

### No Loops
- **No PERFORM UNTIL** statements
- **No PERFORM VARYING** statements
- **Linear execution**: Each procedure executes once per call

### No Conditional PERFORMs
- **No PERFORM ... IF ...** constructs
- **EVALUATE dispatching only**
- **Predictable branching**

---

## Data Flow Through Call Graph

### Flow 1: Initialization (OPEN)

```
Calling Program
    ↓ LS-LOG-OPERATION-FLAG = "OPEN"
MAIN-PROCEDURE (reads LS-LOG-OPERATION-FLAG)
    ↓ PERFORM LOG-INIT-PROCEDURE
LOG-INIT-PROCEDURE
    ↓ Sets WS-LOG-FILE-NAME
    ↓ Opens LOG-FILE
    ↓ Initializes LOG-RECORD-ID = 1
    ↓ Sets LOG-RECORD-FUNCTION-NAME = "LOG-INIT-PROCEDURE"
    ↓ Sets LOG-RECORD-MESSAGE = "Starting Program!"
    ↓ WRITE LOG-RECORD → LOG-FILE
    ↓
Returns to MAIN-PROCEDURE → GOBACK → Calling Program
```

### Flow 2: Write Operation (ADD)

```
Calling Program
    ↓ LS-LOG-OPERATION-FLAG = "ADD"
    ↓ LS-LOG-RECORD-FUNCTION-NAME = [caller's function]
    ↓ LS-LOG-RECORD-MESSAGE = [caller's message]
MAIN-PROCEDURE (reads LS-LOG-OPERATION-FLAG)
    ↓ PERFORM LOG-WRITE-TO-PROCEDURE
LOG-WRITE-TO-PROCEDURE
    ↓ Increments LOG-RECORD-ID
    ↓ Copies LS-LOG-RECORD-FUNCTION-NAME → LOG-RECORD-FUNCTION-NAME
    ↓ Copies LS-LOG-RECORD-MESSAGE → LOG-RECORD-MESSAGE
    ↓ WRITE LOG-RECORD → LOG-FILE
    ↓
Returns to MAIN-PROCEDURE → GOBACK → Calling Program
```

### Flow 3: Close Operation (CLOSE)

```
Calling Program
    ↓ LS-LOG-OPERATION-FLAG = "CLOSE"
MAIN-PROCEDURE (reads LS-LOG-OPERATION-FLAG)
    ↓ PERFORM LOG-CLOSE-PROCEDURE
LOG-CLOSE-PROCEDURE
    ↓ Increments LOG-RECORD-ID
    ↓ Sets LOG-RECORD-FUNCTION-NAME = "LOGGER:LOG-CLOSE-PROCEDURE"
    ↓ Sets LOG-RECORD-MESSAGE = "Closed logging file"
    ↓ WRITE LOG-RECORD → LOG-FILE
    ↓ CLOSE LOG-FILE
    ↓
Returns to MAIN-PROCEDURE → GOBACK → Calling Program
```

### Flow 4: Error Path (Invalid Flag)

```
Calling Program
    ↓ LS-LOG-OPERATION-FLAG = [invalid value]
MAIN-PROCEDURE (reads LS-LOG-OPERATION-FLAG)
    ↓ PERFORM LOG-FLAG-ERROR-PROCEDURE
LOG-FLAG-ERROR-PROCEDURE
    ↓ DISPLAY "READ FLAG ERROR" → Console
    ↓
Returns to MAIN-PROCEDURE → GOBACK → Calling Program
```

---

## State Transitions

### File State Transitions Through Call Graph

```
[Closed] ──OPEN──→ [Open] ──ADD(s)──→ [Open] ──CLOSE──→ [Closed]
   ↑                  ↓                   ↓                  ↓
   └──────────────────┴───────────────────┴──────────────────┘
   
Handlers:
OPEN  → LOG-INIT-PROCEDURE  → Transition: Closed → Open
ADD   → LOG-WRITE-TO        → Maintain: Open → Open
CLOSE → LOG-CLOSE-PROCEDURE → Transition: Open → Closed
OTHER → LOG-FLAG-ERROR      → No state change
```

### Record ID State Transitions

```
[Uninitialized] ──OPEN──→ [ID=1] ──ADD──→ [ID=2] ──ADD──→ [ID=3] ... ──CLOSE──→ [ID=N]
                            ↓               ↓               ↓                      ↓
                        LOG-INIT      LOG-WRITE-TO    LOG-WRITE-TO         LOG-CLOSE
                         (sets 1)      (increments)    (increments)        (increments)
```

---

## Performance Characteristics

### Call Depth
- **Maximum Call Depth**: 2 levels
  - Level 1: MAIN-PROCEDURE
  - Level 2: Handler procedure (LOG-INIT, LOG-WRITE-TO, LOG-CLOSE, or LOG-FLAG-ERROR)
- **Stack Usage**: Minimal (shallow call stack)

### Call Frequency
- **Per Session**:
  - MAIN-PROCEDURE: N times (once per operation)
  - LOG-INIT-PROCEDURE: 1 time
  - LOG-WRITE-TO-PROCEDURE: 0 to infinity times (depends on log volume)
  - LOG-CLOSE-PROCEDURE: 1 time
  - LOG-FLAG-ERROR-PROCEDURE: 0+ times (only on errors)

### Critical Path
- **Hottest Path**: MAIN-PROCEDURE → LOG-WRITE-TO-PROCEDURE
  - Called most frequently in typical usage
  - Performance-critical for high-volume logging

---

## Error Propagation

### No Error Return Path
- **Issue**: Handlers don't return error codes to MAIN-PROCEDURE
- **Impact**: MAIN-PROCEDURE can't distinguish success from failure
- **Result**: Calling program always receives successful return

### Silent Failures
- **File Errors**: OPEN, WRITE, CLOSE failures not propagated
- **Invalid State**: State errors (e.g., WRITE to closed file) not detected
- **Error Handler**: LOG-FLAG-ERROR displays message but returns normally

---

## Recommendations for Improvement

### Add Return Codes
```cobol
01 LS-RETURN-CODE PIC S9(4) COMP.  *> Add to LINKAGE SECTION

MAIN-PROCEDURE.
    EVALUATE LS-LOG-OPERATION-FLAG
    WHEN "OPEN"
        PERFORM LOG-INIT-PROCEDURE
        MOVE WS-INIT-STATUS TO LS-RETURN-CODE
    ...
```

### Add Error Callback
```cobol
*> Allow caller to register error handler
PERFORM CALLER-ERROR-HANDLER IF WS-ERROR-OCCURRED.
```

### Implement State Tracking
```cobol
01 WS-LOG-FILE-STATE PIC X(6).
   88 WS-LOG-CLOSED VALUE 'CLOSED'.
   88 WS-LOG-OPEN VALUE 'OPEN'.

LOG-INIT-PROCEDURE.
    IF WS-LOG-OPEN
        DISPLAY 'Error: Log file already open'
        ...
```

---

## Related Documentation

- **[logger_INDEX.md](logger_INDEX.md)** - Program structure overview
- **[paragraphs/MAIN-PROCEDURE.md](paragraphs/MAIN-PROCEDURE.md)** - Dispatcher documentation
- **[paragraphs/LOG-INIT-PROCEDURE.md](paragraphs/LOG-INIT-PROCEDURE.md)** - Initialization handler
- **[paragraphs/LOG-WRITE-TO-PROCEDURE.md](paragraphs/LOG-WRITE-TO-PROCEDURE.md)** - Write handler
- **[paragraphs/LOG-CLOSE-PROCEDURE.md](paragraphs/LOG-CLOSE-PROCEDURE.md)** - Close handler
- **[paragraphs/LOG-FLAG-ERROR-PROCEDURE.md](paragraphs/LOG-FLAG-ERROR-PROCEDURE.md)** - Error handler
- **[logger_VARIABLE_MUTATIONS.md](logger_VARIABLE_MUTATIONS.md)** - Variable state changes
- **[logger_ERROR_HANDLING.md](logger_ERROR_HANDLING.md)** - Error analysis

---

## Expert Review Status

**⚠️ AI-GENERATED DOCUMENTATION**: This call graph was generated by AI and requires expert review.

**Review Checklist**:
- [ ] Verify call graph completeness (no missed PERFORMs)
- [ ] Confirm control flow interpretation is accurate
- [ ] Validate state transition model
- [ ] Review performance analysis for accuracy
- [ ] Verify recommendations are appropriate

**Expert Notes**: _[To be filled during review]_
