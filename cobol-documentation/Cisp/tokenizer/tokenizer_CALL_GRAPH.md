# TOKENIZER Program - Call Graph and Control Flow

## Program Information
- **Program Name**: TOKENIZER
- **Repository**: Cisp
- **Generated**: 2026-01-20

---

## Main Program Flow

```
MAIN-PROCEDURE
├── PERFORM FILE-HANDLING-PROCEDURE
├── PERFORM TOKENIZE-LISP-PROCEDURE
│   ├── PERFORM FORMAT-LISP-PROCEDURE
│   │   ├── PERFORM CALC-LISP-LENGTH (multiple times)
│   │   ├── PERFORM VARYING loop over WS-FORMAT-STR-INDEX
│   │   │   ├── PERFORM FORMAT-PAREN-SPACE-PROCEDURE
│   │   │   │   ├── PERFORM FORMAT-CHECK-PAREN-PROCEDURE
│   │   │   │   └── PERFORM (conditionally):
│   │   │   │       ├── FORMAT-ADD-BOTH-SPACES
│   │   │   │       ├── FORMAT-ADD-RIGHT-SPACE
│   │   │   │       └── FORMAT-ADD-LEFT-SPACE
│   │   └── CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD
│   ├── UNSTRING operation (tokenization)
│   └── CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD
├── PERFORM CAL-LENGTH-ALL-SYMBOLS
│   └── PERFORM VARYING loop over WS-COUNT
│       └── PERFORM CALC-LENGTH-SYMBOL (100 times)
└── GOBACK (terminate program)
```

---

## Detailed Paragraph Call Hierarchy

### Level 1: Main Entry Point

#### MAIN-PROCEDURE
- **Called by**: External caller (CISP program)
- **Calls**:
  1. FILE-HANDLING-PROCEDURE
  2. TOKENIZE-LISP-PROCEDURE
  3. CAL-LENGTH-ALL-SYMBOLS
  4. PRINT-SYMBOL-TABLE (commented out with D directive)
- **Control Flow**: Sequential execution, then GOBACK
- **Business Purpose**: Orchestrates the complete tokenization workflow

---

### Level 2: Primary Processing Procedures

#### FILE-HANDLING-PROCEDURE
- **Called by**: MAIN-PROCEDURE
- **Calls**:
  1. CALC-LISP-LENGTH (called twice)
  2. APPEND-LISP-PROCEDURE (in loop)
  3. CALL 'LOGGER' (external program)
- **Loop Structure**: PERFORM UNTIL WS-LISP-EOF="Y"
- **Control Flow**: 
  - Opens LISP file
  - Reads first record
  - Loops through remaining records (calls APPEND-LISP-PROCEDURE)
  - Closes file
  - Logs completion
- **Business Purpose**: Read entire LISP file into memory, excluding comments

#### TOKENIZE-LISP-PROCEDURE
- **Called by**: MAIN-PROCEDURE
- **Calls**:
  1. FORMAT-LISP-PROCEDURE
  2. CALL 'LOGGER' (external program)
- **Loop Structure**: PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT = 100 OR WS-FLAG
- **Control Flow**:
  - Formats LISP string (adds spaces around parentheses)
  - Tokenizes via UNSTRING with space delimiter
  - Stores tokens in LS-SYMBOL array
  - Logs completion
- **Business Purpose**: Split formatted LISP string into individual tokens

#### CAL-LENGTH-ALL-SYMBOLS
- **Called by**: MAIN-PROCEDURE
- **Calls**: CALC-LENGTH-SYMBOL (in loop)
- **Loop Structure**: PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT = 100
- **Control Flow**: Iterates through all 100 symbol slots, calculates each length
- **Business Purpose**: Calculate actual length of each token (excluding trailing spaces)

---

### Level 3: Secondary Processing Procedures

#### APPEND-LISP-PROCEDURE
- **Called by**: FILE-HANDLING-PROCEDURE (in loop)
- **Calls**: CALC-LISP-LENGTH
- **Control Flow**:
  - Calculates length of current record
  - If not comment:
    - If first record: Direct copy to WS-IN-LISP-RECORD
    - Else: Concatenate to existing WS-IN-LISP-RECORD
    - Updates accumulated length in WS-TEMP-NUM
- **Business Purpose**: Concatenate multi-line LISP file into single string

#### FORMAT-LISP-PROCEDURE
- **Called by**: TOKENIZE-LISP-PROCEDURE
- **Calls**:
  1. CALC-LISP-LENGTH
  2. FORMAT-PAREN-SPACE-PROCEDURE (in loop, conditionally)
  3. CALL 'LOGGER' (external program)
- **Loop Structure**: PERFORM VARYING WS-FORMAT-STR-INDEX... UNTIL WS-FORMAT-STR-INDEX > WS-LISP-LENGTH
- **Control Flow**:
  - Calculates string length
  - Handles special case: opening parenthesis without space
  - Loops through each character:
    - On "(" or ")" → calls FORMAT-PAREN-SPACE-PROCEDURE
  - Logs completion
- **Business Purpose**: Add spaces around all parentheses for clean tokenization

#### CALC-LENGTH-SYMBOL
- **Called by**: CAL-LENGTH-ALL-SYMBOLS (in loop)
- **Calls**: None
- **Loop Structure**: PERFORM VARYING WS-PARSE-STR-INDEX FROM 1 BY 1 UNTIL WS-PARSE-HAS-ENDED OR WS-PARSE-STR-INDEX > 100
- **Control Flow**:
  - Resets parse flags
  - Iterates character-by-character through symbol
  - Stops at first space or position 100
  - Stores length in WS-PARSE-EXPRESSION-LEN
- **Business Purpose**: Calculate actual character length of individual token

---

### Level 4: Utility and Helper Procedures

#### CALC-LISP-LENGTH
- **Called by**: APPEND-LISP-PROCEDURE, FILE-HANDLING-PROCEDURE, FORMAT-LISP-PROCEDURE
- **Calls**: None
- **Loop Structure**: PERFORM VARYING WS-FORMAT-STR-INDEX FROM 1 BY 1 UNTIL WS-FORMAT-STR-INDEX = WS-MAX-LISP-LENGTH
- **Control Flow**:
  - Resets length counters and comment flag
  - Iterates through WS-CALC-LENGTH-STR:
    - If ";" detected → sets comment flag, exits
    - If non-space → increments length, adds accumulated spaces
    - If space → increments space counter
- **Business Purpose**: Calculate actual length excluding trailing spaces and detect comments

#### FORMAT-PAREN-SPACE-PROCEDURE
- **Called by**: FORMAT-LISP-PROCEDURE (in loop, on "(" or ")")
- **Calls**:
  1. FORMAT-CHECK-PAREN-PROCEDURE
  2. Conditionally one of:
     - FORMAT-ADD-BOTH-SPACES
     - FORMAT-ADD-RIGHT-SPACE
     - FORMAT-ADD-LEFT-SPACE
- **Control Flow**:
  - Checks both sides of parenthesis
  - Calls appropriate spacing procedure based on flags
- **Business Purpose**: Dispatch to correct space-adding procedure

#### FORMAT-CHECK-PAREN-PROCEDURE
- **Called by**: FORMAT-PAREN-SPACE-PROCEDURE
- **Calls**: None
- **Control Flow**:
  - Checks character to left of parenthesis (sets WS-PAREN-LEFT-YES if not space)
  - Checks character to right of parenthesis (sets WS-PAREN-RIGHT-YES if not space)
  - Restores index to original position
- **Business Purpose**: Determine which side(s) of parenthesis need spacing

#### FORMAT-ADD-LEFT-SPACE
- **Called by**: FORMAT-PAREN-SPACE-PROCEDURE (conditionally)
- **Calls**: None
- **Control Flow**:
  - Builds string: [before paren] + " " + [paren to end]
  - Increments index and length
- **Business Purpose**: Insert space before parenthesis

#### FORMAT-ADD-RIGHT-SPACE
- **Called by**: FORMAT-PAREN-SPACE-PROCEDURE (conditionally)
- **Calls**: None
- **Control Flow**:
  - Builds string: [up to paren] + " " + [after paren]
  - Increments index and length
- **Business Purpose**: Insert space after parenthesis

#### FORMAT-ADD-BOTH-SPACES
- **Called by**: FORMAT-PAREN-SPACE-PROCEDURE (conditionally)
- **Calls**: None
- **Control Flow**:
  - Saves parenthesis character
  - Builds string: [before paren] + " " + [paren] + " " + [after paren]
  - Increments index and length by 2
- **Business Purpose**: Insert spaces on both sides of parenthesis

---

### Level 5: Debugging and Utility Procedures (Currently Not Called in Main Flow)

#### PRINT-SYMBOL-TABLE
- **Called by**: MAIN-PROCEDURE (commented out with D directive - debug only)
- **Calls**: None (just DISPLAY statements)
- **Loop Structure**: PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT GREATER THAN LS-SYMBOL-TABLE-SIZE
- **Control Flow**: Displays each symbol with its index and length
- **Business Purpose**: Debug output to verify tokenization results

#### RESET-PARSE-FLAGS-PROCEDURE
- **Called by**: None (utility procedure not currently invoked)
- **Calls**: None
- **Control Flow**: Resets all parse-related flags and counters to initial state
- **Business Purpose**: Reset parsing state (reserved for future use)

#### PRINT-PARSE-FLAGS-PROCEDURE
- **Called by**: None (utility procedure not currently invoked)
- **Calls**: None (just DISPLAY statements)
- **Control Flow**: Displays current parse flag values
- **Business Purpose**: Debug output for parsing state (reserved for future use)

---

## Loop Structures

### File Reading Loop (FILE-HANDLING-PROCEDURE)
```
PERFORM UNTIL WS-LISP-EOF="Y"
    READ LISP-FILE
        AT END: SET WS-LISP-EOF = "Y"
        NOT AT END: PERFORM APPEND-LISP-PROCEDURE
    END-READ
END-PERFORM
```
- **Purpose**: Read all lines from LISP file
- **Termination**: End-of-file flag set
- **Iteration**: One loop per line in file

### Formatting Loop (FORMAT-LISP-PROCEDURE)
```
PERFORM VARYING WS-FORMAT-STR-INDEX FROM [initial] BY 1 
    UNTIL WS-FORMAT-STR-INDEX > WS-LISP-LENGTH
    [Character-by-character processing]
    EVALUATE WS-IN-LISP-RECORD(WS-FORMAT-STR-INDEX:1)
        WHEN "(" OR ")" → PERFORM FORMAT-PAREN-SPACE-PROCEDURE
    END-EVALUATE
END-PERFORM
```
- **Purpose**: Add spaces around every parenthesis
- **Termination**: Processed entire string length
- **Iteration**: One loop per character (length increases dynamically as spaces added)

### Tokenization Loop (TOKENIZE-LISP-PROCEDURE)
```
PERFORM VARYING WS-COUNT FROM 1 BY 1 
    UNTIL WS-COUNT = 100 OR WS-FLAG
    UNSTRING WS-IN-LISP-RECORD DELIMITED BY ALL ' ' 
        INTO LS-SYMBOL(WS-COUNT)
    IF LS-SYMBOL(WS-COUNT) = SPACES THEN SET WS-FLAG-YES
    ELSE ADD 1 TO LS-SYMBOL-TABLE-SIZE
    END-IF
END-PERFORM
```
- **Purpose**: Split string into tokens
- **Termination**: 100 tokens reached OR empty token found
- **Iteration**: One loop per token (UNSTRING auto-advances pointer)

### Symbol Length Loop (CAL-LENGTH-ALL-SYMBOLS)
```
PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT = 100
    PERFORM CALC-LENGTH-SYMBOL
    MOVE WS-PARSE-EXPRESSION-LEN TO LS-SYMBOL-LEN(WS-COUNT)
END-PERFORM
```
- **Purpose**: Calculate length for all symbol slots
- **Termination**: All 100 slots processed
- **Iteration**: Fixed 100 iterations

### Character Length Loop (CALC-LENGTH-SYMBOL)
```
PERFORM VARYING WS-PARSE-STR-INDEX FROM 1 BY 1 
    UNTIL WS-PARSE-HAS-ENDED OR WS-PARSE-STR-INDEX > 100
    IF LS-SYMBOL(WS-COUNT)(WS-PARSE-STR-INDEX:1) = " " 
        THEN SET WS-PARSE-HAS-ENDED TO TRUE
    ELSE ADD 1 TO WS-PARSE-EXPRESSION-LEN
    END-IF
END-PERFORM
```
- **Purpose**: Count non-space characters in symbol
- **Termination**: Space found OR 100 characters checked
- **Iteration**: One loop per character in symbol

---

## External Program Calls

### CALL 'LOGGER'
- **Called from**:
  - FILE-HANDLING-PROCEDURE (after file read completion)
  - TOKENIZE-LISP-PROCEDURE (after tokenization completion)
  - FORMAT-LISP-PROCEDURE (after formatting completion)
- **Parameters**:
  - WS-LOG-OPERATION-FLAG (INPUT: always "ADD")
  - WS-LOG-RECORD (INPUT: function name + message)
- **Purpose**: Audit trail logging for major processing milestones

---

## Error Handling

### File Operations
- **READ with AT END clause**: Sets WS-LISP-EOF flag, loop terminates gracefully
- **No explicit error handling**: File OPEN/CLOSE failures not caught (assumes file exists)

### Array Bounds
- **Fixed 100-element limit**: Tokenization stops at 100 tokens OR when spaces found
- **No overflow detection**: If LISP has >100 tokens, remainder silently ignored

### Length Calculations
- **200-byte buffer limit**: Input lines truncated if exceed 200 characters
- **2000-byte format buffer**: Provides safety margin for space insertion

---

## Execution Path Examples

### Example 1: Simple LISP Expression `(+ 1 2)`

1. **FILE-HANDLING-PROCEDURE**:
   - Opens file, reads `(+ 1 2)`
   - CALC-LISP-LENGTH → WS-LISP-LENGTH = 7
   - Stores in WS-IN-LISP-RECORD = `(+ 1 2)`
   - Closes file

2. **TOKENIZE-LISP-PROCEDURE**:
   - **FORMAT-LISP-PROCEDURE**:
     - Initial check: `(+` → adds space → `( + 1 2)`
     - Loop: processes `)` → adds space → `( + 1 2 )`
     - Result: WS-IN-LISP-RECORD = `( + 1 2 )`
   - **UNSTRING operation**:
     - Token 1: `(`
     - Token 2: `+`
     - Token 3: `1`
     - Token 4: `2`
     - Token 5: `)`
     - LS-SYMBOL-TABLE-SIZE = 5

3. **CAL-LENGTH-ALL-SYMBOLS**:
   - Calculates lengths: [1, 1, 1, 1, 1, 0, 0, ...]
   - Stores in LS-SYMBOL-LEN array

### Example 2: Multi-line with Comment

Input file:
```
; This is a comment
(defun foo ()
  (+ 1 2))
```

1. **FILE-HANDLING-PROCEDURE**:
   - Read line 1: `; This is a comment` → CALC-LISP-LENGTH sets WS-IS-COMMENT-YES → skipped
   - Read line 2: `(defun foo ()` → not comment → WS-IN-LISP-RECORD = `(defun foo ()`
   - Read line 3: `  (+ 1 2))` → APPEND-LISP-PROCEDURE concatenates → WS-IN-LISP-RECORD = `(defun foo ()(+ 1 2))`

2. **FORMAT-LISP-PROCEDURE**: Adds spaces around all 6 parentheses
3. **Tokenization**: Splits into ~10 tokens

---

## Performance Considerations

### Loop Complexity
- **FILE-HANDLING**: O(n) where n = lines in file
- **FORMAT-LISP**: O(m) where m = character length (increases as spaces added)
- **TOKENIZE**: O(k) where k = number of tokens (max 100)
- **Length Calculation**: O(100 × avg_token_length)

### String Operations
- **STRING statement**: Used 5 times (expensive COBOL operation)
- **UNSTRING**: Used once (efficient tokenization)
- **MOVE**: Used extensively (fast operation)

### Optimization Opportunities
1. **Pre-calculate buffer size**: Avoid dynamic length tracking
2. **Single-pass formatting**: Combine length calc and formatting
3. **Dynamic array sizing**: Use LS-SYMBOL-TABLE-SIZE instead of fixed 100 loop

---

## Technical Debt

1. **Unused procedures**: RESET-PARSE-FLAGS-PROCEDURE, PRINT-PARSE-FLAGS-PROCEDURE defined but never called
2. **Unused variables**: WS-PARSE-STR-CHAR, WS-PARSE-EXPRESSION-START, WS-PARSE-EXPRESSION-END defined but unused
3. **Debug statements**: Multiple DISPLAY statements commented with D directive (should be removed or formalized)
4. **Magic numbers**: 100, 200, 2000 hard-coded throughout
5. **LS-SYMBOL-LENGTH not set**: Linkage parameter defined but never populated

---

*This call graph is AI-generated and should be reviewed by COBOL experts for accuracy. Last updated: 2026-01-20*
