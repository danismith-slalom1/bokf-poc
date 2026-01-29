# CALC-LISP-LENGTH

## Overview
**Program**: TOKENIZER  
**Location**: Lines 214-228  
**Purpose**: Calculate actual length of LISP string, excluding trailing spaces and detecting comments

---

## Business Purpose
Utility procedure that scans a LISP string to determine its true character length (excluding trailing spaces) and identifies comment lines. Critical for proper string manipulation and loop boundaries throughout the tokenization process.

---

## Input Requirements
- **WS-CALC-LENGTH-STR**: String to analyze (typically copied from IN-LISP-RECORD or WS-IN-LISP-RECORD)
- String should not exceed 200 characters (WS-MAX-LISP-LENGTH constant)

---

## Processing Steps

### 1. Initialize Counters and Flags
```cobol
MOVE 0 TO WS-LISP-LENGTH
MOVE 0 TO WS-NUM-LENGTH-ADD
SET WS-IS-COMMENT-YES TO FALSE
```
- **WS-LISP-LENGTH**: Actual character count (result)
- **WS-NUM-LENGTH-ADD**: Accumulator for consecutive spaces
- **WS-IS-COMMENT**: Flag for comment detection

### 2. Character-by-Character Analysis
```cobol
PERFORM VARYING WS-FORMAT-STR-INDEX FROM 1 BY 1 
    UNTIL WS-FORMAT-STR-INDEX = WS-MAX-LISP-LENGTH
```
Loop processes each character (max 200 iterations)

### 3. Comment Detection
```cobol
IF WS-CALC-LENGTH-STR(WS-FORMAT-STR-INDEX:1) EQUAL ";" THEN
    SET WS-IS-COMMENT-YES TO TRUE
```
- **Condition**: Semicolon character found
- **Action**: Set comment flag, implicitly exit loop (no further processing)
- **LISP Rule**: Semicolon starts single-line comment

### 4. Non-Space Character Processing
```cobol
ELSE IF NOT WS-CALC-LENGTH-STR(WS-FORMAT-STR-INDEX:1) EQUALS " " THEN
    ADD 1 TO WS-LISP-LENGTH
    ADD WS-NUM-LENGTH-ADD TO WS-LISP-LENGTH
    MOVE 0 TO WS-NUM-LENGTH-ADD
```
- **Condition**: Character is not space (and not comment)
- **Actions**:
  1. Increment length by 1 (current character)
  2. Add accumulated space count (embedded spaces)
  3. Reset space accumulator
- **Effect**: Counts character and all preceding contiguous spaces

### 5. Space Character Processing
```cobol
ELSE
    ADD 1 TO WS-NUM-LENGTH-ADD
END-IF
```
- **Condition**: Character is space
- **Action**: Increment space accumulator
- **Effect**: Spaces counted only if followed by non-space character

---

## Algorithm Explanation

### Trailing Space Elimination
The algorithm cleverly handles trailing spaces by using a **delayed counting** strategy:

1. **Space encountered**: Increment WS-NUM-LENGTH-ADD (tentative space count)
2. **Non-space encountered**: Add accumulated spaces + 1 to WS-LISP-LENGTH
3. **Another space**: Start accumulating again
4. **End of string**: Accumulated spaces NOT added (trailing spaces ignored)

### Example Execution

**Input**: `"(foo  bar  )"` (12 chars with trailing spaces)

| Index | Char | Action | WS-NUM-LENGTH-ADD | WS-LISP-LENGTH | Comment |
|-------|------|--------|-------------------|----------------|---------|
| 1 | `(` | Non-space | 0 | 1 | First char |
| 2 | `f` | Non-space | 0 | 2 | - |
| 3 | `o` | Non-space | 0 | 3 | - |
| 4 | `o` | Non-space | 0 | 4 | - |
| 5 | ` ` | Space | 1 | 4 | Accumulate |
| 6 | ` ` | Space | 2 | 4 | Accumulate |
| 7 | `b` | Non-space | 0 | 7 | Add 2+1=3 |
| 8 | `a` | Non-space | 0 | 8 | - |
| 9 | `r` | Non-space | 0 | 9 | - |
| 10 | ` ` | Space | 1 | 9 | Accumulate |
| 11 | ` ` | Space | 2 | 9 | Accumulate |
| 12 | `)` | Non-space | 0 | 12 | Add 2+1=3 |
| 13-200 | ` ` | Space | 1...188 | 12 | Trailing (ignored) |

**Result**: WS-LISP-LENGTH = 12 (excluding trailing spaces)

---

## Output and Side Effects

### Modified Variables
- **WS-LISP-LENGTH**: Set to actual string length (1 to 200)
- **WS-NUM-LENGTH-ADD**: Contains count of trailing spaces (not used further)
- **WS-IS-COMMENT**: Set to TRUE if ";" found, FALSE otherwise
- **WS-FORMAT-STR-INDEX**: Final loop counter value

### Return Values (via shared variables)
- **WS-LISP-LENGTH**: Primary output - string length
- **WS-IS-COMMENT**: Secondary output - comment flag

---

## Dependencies

### Called Paragraphs
- None (leaf procedure - calls no other paragraphs)

### Data Dependencies
- **WS-CALC-LENGTH-STR**: Must be populated before call
- **WS-MAX-LISP-LENGTH**: Constant (78-level VALUE 200)

---

## Error Handling
- **No explicit error handling**
- **Assumption**: Input string is valid COBOL string (may contain SPACES)
- **Edge cases handled**:
  - Empty string: WS-LISP-LENGTH = 0
  - All spaces: WS-LISP-LENGTH = 0
  - Comment line: WS-IS-COMMENT = TRUE, length calculation stops

---

## Business Rules

### LISP Comment Syntax
- **Comment indicator**: Semicolon (`;`) character
- **Scope**: From `;` to end of line (not just trailing spaces)
- **Processing**: Entire line discarded if comment detected

### Length Calculation Rules
1. **Include**: All non-space characters
2. **Include**: Embedded spaces (between non-space characters)
3. **Exclude**: Leading spaces (none expected in calling context)
4. **Exclude**: Trailing spaces (any spaces after last non-space char)
5. **Exclude**: Everything after ";" (comment content)

---

## Performance Considerations

### Time Complexity
- **Best case**: O(n) where n = position of ";" (comment detection exits early)
- **Typical case**: O(n) where n = position of last non-space character
- **Worst case**: O(200) - always scans full WS-MAX-LISP-LENGTH

### Optimization Opportunities
1. **Early exit on trailing spaces**: Could stop after certain consecutive spaces
2. **Length-bounded loop**: Use INSPECT to find last non-space, loop only to that point
3. **INSPECT TALLYING**: Use COBOL intrinsic for counting (may be faster)

---

## Usage Context

### Called From Multiple Procedures
1. **APPEND-LISP-PROCEDURE**: Calculate record length before concatenation
2. **FILE-HANDLING-PROCEDURE**: Calculate length of first and subsequent records
3. **FORMAT-LISP-PROCEDURE**: Determine string length before formatting loop

### Typical Usage Pattern
```cobol
MOVE <source-string> TO WS-CALC-LENGTH-STR
PERFORM CALC-LISP-LENGTH
IF NOT WS-IS-COMMENT-YES THEN
    [Process string using WS-LISP-LENGTH]
END-IF
```

---

## Test Cases

### Test 1: Normal String
- **Input**: `"(foo bar)       "` (13 chars content, 7 trailing spaces)
- **Output**: WS-LISP-LENGTH = 13, WS-IS-COMMENT = FALSE

### Test 2: Comment Line
- **Input**: `"; This is comment"`
- **Output**: WS-LISP-LENGTH = 0, WS-IS-COMMENT = TRUE

### Test 3: Empty String
- **Input**: `"                "` (all spaces)
- **Output**: WS-LISP-LENGTH = 0, WS-IS-COMMENT = FALSE

### Test 4: No Trailing Spaces
- **Input**: `"(+ 1 2)"`
- **Output**: WS-LISP-LENGTH = 7, WS-IS-COMMENT = FALSE

### Test 5: Embedded Spaces
- **Input**: `"(foo    bar)"` (4 spaces between foo and bar)
- **Output**: WS-LISP-LENGTH = 12, WS-IS-COMMENT = FALSE

---

## Maintenance Notes

### Known Limitations
1. **Fixed maximum**: Always scans 200 characters regardless of actual length
2. **Comment syntax**: Only supports single-line comments (`;`)
3. **No validation**: Doesn't verify balanced parentheses or syntax
4. **No escape handling**: Semicolon in string literal treated as comment

### Potential Enhancements
1. Optimize loop to exit at last non-space character
2. Support block comments (e.g., `#| ... |#` in some LISP dialects)
3. Handle escaped semicolons in string literals
4. Return both length and comment flag explicitly (not just via shared variables)

### Relationship to Other Procedures
- **Critical utility**: Used by 3 different calling procedures
- **Stateless**: No side effects beyond output variables
- **Reusable**: Can be called multiple times safely

---

*This documentation is AI-generated and should be reviewed by COBOL experts for accuracy.*
