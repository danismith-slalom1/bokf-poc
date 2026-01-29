# TOKENIZER Program - Variable Mutation Analysis

## Program Information
- **Program Name**: TOKENIZER
- **Repository**: Cisp
- **Generated**: 2026-01-20

---

## Overview

This document tracks variables that are modified in multiple locations throughout the program execution. Understanding these mutation patterns is critical for:
- Debugging state-related issues
- Refactoring and modernization efforts
- Understanding program control flow
- Identifying potential race conditions or unexpected mutations

---

## Global Variable Mutation Patterns

### 1. WS-IN-LISP-RECORD (Modified in 4 procedures)
**Picture**: X(200)  
**Purpose**: Central data buffer for LISP content processing

#### Mutation Timeline (Execution Order)

1. **Initialization: FILE-HANDLING-PROCEDURE**
   - **Line**: ~131
   - **Operation**: `MOVE IN-LISP-RECORD TO WS-IN-LISP-RECORD`
   - **Trigger**: First non-comment line read from file
   - **State Transition**: Empty/undefined → First LISP line
   - **Business Logic**: Initialize buffer with first valid LISP content

2. **Concatenation: APPEND-LISP-PROCEDURE**
   - **Line**: ~110-114
   - **Operation**: `STRING WS-IN-LISP-RECORD(1:WS-TEMP-NUM) ... IN-LISP-RECORD ... INTO WS-IN-LISP-RECORD`
   - **Trigger**: Each subsequent non-comment line read from file
   - **State Transition**: Single line → Multi-line concatenated string
   - **Business Logic**: Accumulate all LISP lines into single string
   - **Complexity**: Builds composite string through repeated concatenation

3. **Formatting: FORMAT-LISP-PROCEDURE**
   - **Line**: Multiple (187-190, via sub-procedures 252-280)
   - **Operation**: Multiple STRING operations adding spaces around parentheses
   - **Trigger**: Before tokenization
   - **State Transition**: Raw LISP → Formatted with spaced parentheses
   - **Business Logic**: Transform `(foo)` → `( foo )` for clean tokenization
   - **Complexity**: String rebuilt multiple times (once per parenthesis)

4. **Consumption: TOKENIZE-LISP-PROCEDURE**
   - **Line**: 159
   - **Operation**: `UNSTRING WS-IN-LISP-RECORD ... INTO LS-SYMBOL`
   - **Trigger**: Tokenization phase
   - **State Transition**: Formatted string → Read-only source for tokenization
   - **Business Logic**: Source string split into tokens (not modified, but consumed)

#### Mutation Concerns
- **Multiple STRING operations**: Performance concern (expensive in COBOL)
- **Buffer overflow risk**: 200-byte limit may be insufficient for complex files
- **State complexity**: Variable serves multiple roles (accumulator, formatter, source)

#### Refactoring Recommendations
1. **Consider separate buffers** for raw vs formatted content
2. **Pre-calculate required size** before formatting to avoid repeated STRING ops
3. **Implement overflow detection** before file reading

---

### 2. WS-LISP-LENGTH (Modified in 6 procedures)
**Picture**: 9(10)  
**Purpose**: Tracks current actual length of LISP content

#### Mutation Timeline (Execution Order)

1. **Initialization: CALC-LISP-LENGTH**
   - **Line**: 216
   - **Operation**: `MOVE 0 TO WS-LISP-LENGTH`, then incremented
   - **Trigger**: Called from FILE-HANDLING-PROCEDURE (first record)
   - **State Transition**: Undefined → Initial record length
   - **Business Logic**: Calculate true length excluding trailing spaces

2. **Update: APPEND-LISP-PROCEDURE**
   - **Line**: 114
   - **Operation**: `ADD WS-LISP-LENGTH TO WS-TEMP-NUM`
   - **Trigger**: Each concatenated line
   - **State Transition**: Single record length → Cumulative length
   - **Business Logic**: Track total accumulated length

3. **Recalculation: FORMAT-LISP-PROCEDURE**
   - **Line**: 183, 191
   - **Operation**: `PERFORM CALC-LISP-LENGTH`, then `ADD 1 TO WS-LISP-LENGTH`
   - **Trigger**: Before formatting loop
   - **State Transition**: Concatenated length → Pre-format length
   - **Business Logic**: Establish loop boundary

4. **Dynamic Updates: FORMAT-ADD-LEFT-SPACE**
   - **Line**: 254
   - **Operation**: `ADD 1 TO WS-LISP-LENGTH`
   - **Trigger**: Space added left of parenthesis
   - **State Transition**: Length N → Length N+1
   - **Business Logic**: Extend loop boundary as spaces inserted

5. **Dynamic Updates: FORMAT-ADD-RIGHT-SPACE**
   - **Line**: 261
   - **Operation**: `ADD 1 TO WS-LISP-LENGTH`
   - **Trigger**: Space added right of parenthesis
   - **State Transition**: Length N → Length N+1
   - **Business Logic**: Extend loop boundary as spaces inserted

6. **Dynamic Updates: FORMAT-ADD-BOTH-SPACES**
   - **Line**: 273
   - **Operation**: `ADD 2 TO WS-LISP-LENGTH`
   - **Trigger**: Spaces added both sides of parenthesis
   - **State Transition**: Length N → Length N+2
   - **Business Logic**: Extend loop boundary for two spaces

#### Mutation Concerns
- **Dynamic boundary**: Loop limit changes during iteration (can cause infinite loop if logic error)
- **Multiple responsibility**: Serves as both accumulator and loop control
- **Synchronization**: Must stay in sync with WS-IN-LISP-RECORD actual length

#### Refactoring Recommendations
1. **Separate concerns**: Use different variables for accumulation vs formatting loop control
2. **Validate consistency**: Add assertions that calculated length matches actual string length
3. **Constant boundary**: Calculate final length before formatting loop

---

### 3. WS-FORMAT-STR-INDEX (Modified in 8+ locations)
**Picture**: 9(10)  
**Purpose**: Primary loop index for character-by-character iteration

#### Mutation Timeline (Execution Order)

1. **Initialization: FORMAT-LISP-PROCEDURE**
   - **Line**: 185 or 191
   - **Operation**: `MOVE 1 TO WS-FORMAT-STR-INDEX` or `ADD 3 TO WS-FORMAT-STR-INDEX`
   - **Trigger**: Before formatting loop
   - **State Transition**: Undefined → 1 or 3 (depending on leading paren)
   - **Business Logic**: Set starting position

2. **Loop Increment: FORMAT-LISP-PROCEDURE**
   - **Line**: 192 (PERFORM VARYING)
   - **Operation**: `BY 1` automatic increment
   - **Trigger**: Each loop iteration
   - **State Transition**: Position N → Position N+1
   - **Business Logic**: Move to next character

3. **Temporary Decrement: FORMAT-CHECK-PAREN-PROCEDURE**
   - **Line**: 236
   - **Operation**: `SUBTRACT 1 FROM WS-FORMAT-STR-INDEX`
   - **Trigger**: Check character left of parenthesis
   - **State Transition**: Current → Previous position (temporary)

4. **Temporary Increment: FORMAT-CHECK-PAREN-PROCEDURE**
   - **Line**: 240
   - **Operation**: `ADD 2 TO WS-FORMAT-STR-INDEX`
   - **Trigger**: Check character right of parenthesis
   - **State Transition**: Previous → Next position (temporary)

5. **Restoration: FORMAT-CHECK-PAREN-PROCEDURE**
   - **Line**: 244
   - **Operation**: `SUBTRACT 1 FROM WS-FORMAT-STR-INDEX`
   - **Trigger**: Restore after right check
   - **State Transition**: Next → Current position (restored)

6. **Space Addition Adjustment: FORMAT-ADD-LEFT-SPACE**
   - **Line**: 253
   - **Operation**: `ADD 1 TO WS-FORMAT-STR-INDEX`
   - **Trigger**: After inserting left space
   - **State Transition**: Current → Skip inserted space
   - **Business Logic**: Account for newly inserted character

7. **Space Addition Adjustment: FORMAT-ADD-RIGHT-SPACE**
   - **Line**: 260
   - **Operation**: `ADD 1 TO WS-FORMAT-STR-INDEX`
   - **Trigger**: After inserting right space
   - **State Transition**: Current → Skip inserted space

8. **Space Addition Adjustment: FORMAT-ADD-BOTH-SPACES**
   - **Line**: 272
   - **Operation**: `ADD 1 TO WS-FORMAT-STR-INDEX`
   - **Trigger**: After inserting both spaces
   - **State Transition**: Current → Skip inserted spaces
   - **Note**: Only adds 1, but loop will auto-increment by 1 more (total 2)

9. **Length Calculation: CALC-LISP-LENGTH**
   - **Line**: 218 (PERFORM VARYING)
   - **Operation**: Loop from 1 to WS-MAX-LISP-LENGTH
   - **Trigger**: Called from multiple procedures
   - **State Transition**: 1 → 200 (or until condition met)

#### Mutation Concerns
- **Complex mutation pattern**: Incremented, decremented, restored, adjusted
- **Shared across procedures**: Used by multiple procedures with different semantics
- **Loop control critical**: Errors could cause infinite loop or skip characters
- **Offset errors**: Difficult to track with multiple adjustments

#### Refactoring Recommendations
1. **Separate index variables**: Use different variables for different loops
2. **Const within procedures**: Use local copies for temporary manipulation
3. **Simplify FORMAT-CHECK-PAREN logic**: Avoid in-place index manipulation

---

### 4. WS-COUNT (Modified in 4 procedures)
**Picture**: 9(10)  
**Purpose**: General-purpose loop counter

#### Mutation Timeline (Execution Order)

1. **Symbol Length Loop: CAL-LENGTH-ALL-SYMBOLS**
   - **Line**: 85 (PERFORM VARYING)
   - **Operation**: Loop from 1 to 100
   - **Trigger**: After tokenization
   - **State Transition**: 1 → 100
   - **Business Logic**: Index for LS-SYMBOL-LEN array

2. **Character Scan: CALC-LENGTH-SYMBOL**
   - **Line**: 90, 92 (inspection via WS-COUNT index)
   - **Operation**: Read `LS-SYMBOL(WS-COUNT)(WS-PARSE-STR-INDEX:1)`
   - **Trigger**: Called from CAL-LENGTH-ALL-SYMBOLS
   - **State Transition**: No mutation, used as array index
   - **Business Logic**: Which symbol to measure

3. **Tokenization Loop: TOKENIZE-LISP-PROCEDURE**
   - **Line**: 156 (PERFORM VARYING)
   - **Operation**: Loop from 1 to 100 (or until WS-FLAG)
   - **Trigger**: During tokenization
   - **State Transition**: 1 → actual_token_count
   - **Business Logic**: Index for LS-SYMBOL array

4. **Display Loop: PRINT-SYMBOL-TABLE**
   - **Line**: 171, 173 (PERFORM VARYING)
   - **Operation**: Loop from 1 to LS-SYMBOL-TABLE-SIZE
   - **Trigger**: Debug output (D directive)
   - **State Transition**: 1 → LS-SYMBOL-TABLE-SIZE
   - **Business Logic**: Display each token

#### Mutation Concerns
- **Reused across contexts**: Same variable for different loops (acceptable pattern)
- **No concurrency**: Sequential execution prevents conflicts
- **Array index**: Must stay within bounds (1-100)

#### Refactoring Recommendations
1. **Rename for clarity**: Use WS-SYMBOL-INDEX or WS-TOKEN-INDEX for semantic meaning
2. **Validate bounds**: Ensure never exceeds 100 (array limit)

---

### 5. LS-SYMBOL-TABLE-SIZE (Modified in 1 procedure, critical output)
**Picture**: 9(4)  
**Location**: LINKAGE SECTION  
**Purpose**: Return actual token count to caller

#### Mutation Timeline

1. **Initialization: TOKENIZE-LISP-PROCEDURE**
   - **Line**: 155
   - **Operation**: `MOVE 0 TO LS-SYMBOL-TABLE-SIZE`
   - **State Transition**: Undefined/previous value → 0
   - **Business Logic**: Reset before counting

2. **Incremental Updates: TOKENIZE-LISP-PROCEDURE**
   - **Line**: 162
   - **Operation**: `ADD 1 TO LS-SYMBOL-TABLE-SIZE`
   - **Trigger**: Each non-empty token extracted
   - **State Transition**: N → N+1
   - **Business Logic**: Count valid tokens
   - **Frequency**: 0 to 100 times (depends on LISP content)

#### Mutation Concerns
- **Critical output**: Caller depends on accurate count
- **Early termination**: Loop may exit before 100 (empty token found)
- **Single responsibility**: Clean, simple mutation pattern

#### Refactoring Recommendations
- No refactoring needed - clean, well-defined mutation pattern

---

## Cross-Procedure State Transitions

### File Reading → Tokenization Flow

```
FILE-HANDLING-PROCEDURE:
  WS-IN-LISP-RECORD = "(defun foo (x) (+ x 1))"
  WS-LISP-LENGTH = 24
  WS-TEMP-NUM = 24

↓

TOKENIZE-LISP-PROCEDURE → FORMAT-LISP-PROCEDURE:
  WS-IN-LISP-RECORD = "( defun foo ( x ) ( + x 1 ) )"
  WS-LISP-LENGTH = 31  (increased by 7 spaces)
  WS-FORMAT-STR-INDEX = 32

↓

TOKENIZE-LISP-PROCEDURE (UNSTRING):
  LS-SYMBOL(1) = "("
  LS-SYMBOL(2) = "defun"
  ...
  LS-SYMBOL(13) = ")"
  LS-SYMBOL-TABLE-SIZE = 13

↓

CAL-LENGTH-ALL-SYMBOLS:
  LS-SYMBOL-LEN(1) = 1
  LS-SYMBOL-LEN(2) = 5
  ...
  LS-SYMBOL-LEN(13) = 1
```

---

## Potential Race Conditions and Concerns

### 1. Loop Boundary Mutations (High Risk)
**Variables**: WS-FORMAT-STR-INDEX, WS-LISP-LENGTH  
**Risk**: Loop condition `UNTIL WS-FORMAT-STR-INDEX > WS-LISP-LENGTH` where both sides change
**Scenario**: If FORMAT-ADD-* procedures incorrectly update index, could cause:
- Infinite loop (index never exceeds length)
- Skipped characters (index jumps too far)
- Buffer overflow (length exceeds 200)

**Mitigation**: 
- 2000-byte WS-PAREN-TEMP-STR provides overflow protection
- Each FORMAT-ADD-* carefully adjusts both index and length

### 2. Shared Index Variable (Medium Risk)
**Variable**: WS-FORMAT-STR-INDEX  
**Risk**: Used by multiple procedures with different semantics
**Scenario**: If called procedure doesn't properly restore state, could corrupt caller's loop
**Current Safety**: FORMAT-CHECK-PAREN-PROCEDURE carefully restores index after temporary changes

**Mitigation**:
- Use local copies for temporary manipulation
- Add assertions to verify state consistency

### 3. Array Bounds (Low Risk)
**Variables**: WS-COUNT (as LS-SYMBOL index)  
**Risk**: Exceeding 100-element array capacity
**Current Safety**: Loops explicitly limited to 100
**Scenario**: If loop logic changed, could write beyond array

**Mitigation**:
- Fixed loop limits (UNTIL WS-COUNT = 100)
- Empty token detection (WS-FLAG) provides early termination

---

## Summary and Recommendations

### Variables Requiring Careful Monitoring
1. **WS-IN-LISP-RECORD**: Most mutated variable (4 procedures)
2. **WS-LISP-LENGTH**: Complex mutation pattern during formatting (6 procedures)
3. **WS-FORMAT-STR-INDEX**: Shared loop index with temporary modifications (8+ locations)

### Recommended Refactoring Priorities

#### Priority 1: Separate Formatting Concerns
```
Current: WS-IN-LISP-RECORD used for raw, accumulated, and formatted content
Proposed: 
  - WS-RAW-LISP-RECORD (accumulation)
  - WS-FORMATTED-LISP-RECORD (formatting output)
```

#### Priority 2: Stabilize Loop Boundaries
```
Current: WS-LISP-LENGTH modified during loop
Proposed:
  - Calculate maximum possible length before loop
  - Use constant boundary
  - Track insertions separately
```

#### Priority 3: Local Index Variables
```
Current: WS-FORMAT-STR-INDEX shared across procedures
Proposed:
  - WS-FORMAT-INDEX (formatting loop)
  - WS-CHECK-INDEX (temporary calculations)
  - WS-LENGTH-INDEX (length calculations)
```

### Testing Recommendations
1. **Stress test formatting**: Deeply nested expressions with many parentheses
2. **Boundary testing**: Expressions near 200-byte limit
3. **Edge cases**: Empty files, comment-only files, single-character tokens
4. **State verification**: Add debug displays showing variable states at each mutation point

---

*This mutation analysis is AI-generated and should be reviewed by COBOL experts for accuracy. Last updated: 2026-01-20*
