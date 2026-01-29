# TOKENIZER Program - Comprehensive Documentation

## Executive Summary

**TOKENIZER** is a COBOL subroutine that transforms LISP source files into structured token arrays for processing by the CISP interpreter. It reads LISP files, filters comments, formats code by adding spaces around parentheses, and tokenizes the result into a symbol table for semantic analysis.

**Key Capabilities:**
- File I/O with comment filtering
- String formatting for tokenization
- Space-delimited token extraction
- Symbol length calculation
- Audit logging integration

**Business Value:**
- Enables COBOL-based LISP interpretation
- Provides clean token separation for parser
- Maintains audit trail of processing steps
- Supports up to 100 tokens per file

---

## Table of Contents

1. [Program Overview](#program-overview)
2. [Architecture and Design](#architecture-and-design)
3. [Data Structures](#data-structures)
4. [Processing Flow](#processing-flow)
5. [Key Algorithms](#key-algorithms)
6. [External Dependencies](#external-dependencies)
7. [Error Handling and Limitations](#error-handling-and-limitations)
8. [Performance Characteristics](#performance-characteristics)
9. [Maintenance Guide](#maintenance-guide)
10. [Testing Recommendations](#testing-recommendations)

---

## Program Overview

### Identification
- **Program ID**: TOKENIZER
- **Author**: lauryn brown
- **Purpose**: Tokenize LISP input file
- **Repository**: Cisp (https://github.com/lauryndbrown/Cisp)
- **Language**: COBOL (GnuCOBOL compatible)
- **Type**: Callable subroutine

### Business Context
TOKENIZER is a core component of the CISP LISP interpreter system. It serves as the lexical analysis phase, converting raw LISP source code into a structured format suitable for parsing and execution. The program is designed to work with traditional LISP s-expression syntax.

### Program Type
- **Entry Point**: PROCEDURE DIVISION USING (called from CISP main program)
- **Termination**: GOBACK (returns to caller)
- **Execution Mode**: Subroutine/subprogram

---

## Architecture and Design

### Design Pattern: Pipeline Processing

The program follows a **pipeline architecture** with four distinct stages:

```
1. FILE INPUT     →  2. FORMATTING    →  3. TOKENIZATION  →  4. LENGTH CALC
   (Raw LISP)        (Spaced parens)     (Token array)        (Token lengths)
```

### Architectural Components

#### 1. **File Handling Layer**
- **Responsibility**: Read LISP source file into memory
- **Components**: FILE-HANDLING-PROCEDURE, APPEND-LISP-PROCEDURE
- **Output**: Concatenated single-line LISP string (comments removed)

#### 2. **Formatting Layer**
- **Responsibility**: Prepare string for clean tokenization
- **Components**: FORMAT-LISP-PROCEDURE, FORMAT-PAREN-SPACE-PROCEDURE, FORMAT-ADD-* procedures
- **Output**: LISP string with spaces around all parentheses

#### 3. **Tokenization Layer**
- **Responsibility**: Split formatted string into discrete tokens
- **Components**: TOKENIZE-LISP-PROCEDURE (UNSTRING operation)
- **Output**: LS-SYMBOL array with tokens, LS-SYMBOL-TABLE-SIZE with count

#### 4. **Measurement Layer**
- **Responsibility**: Calculate actual length of each token
- **Components**: CAL-LENGTH-ALL-SYMBOLS, CALC-LENGTH-SYMBOL
- **Output**: LS-SYMBOL-LEN array with token lengths

### Utility Components

#### Length Calculation Utility
- **Component**: CALC-LISP-LENGTH
- **Shared by**: File handling, formatting, and append procedures
- **Purpose**: Calculate string length and detect comments

#### Logging Integration
- **Component**: CALL 'LOGGER'
- **Frequency**: 3 audit points (file, format, tokenize)
- **Purpose**: Create audit trail for debugging and monitoring

---

## Data Structures

### Input Parameters (LINKAGE SECTION)

| Parameter | Type | Direction | Purpose |
|-----------|------|-----------|---------|
| LS-LISP-FILE-NAME | X(100) | IN | File path to LISP source |
| LS-SYMBOL-LENGTH | 9(4) | OUT | Total token count (not populated) |
| LS-LISP-SYMBOLS | Group | OUT | Token table structure |
| ├─ LS-SYMBOL-TABLE-SIZE | 9(4) | OUT | Actual number of tokens |
| ├─ LS-SYMBOL(100) | X(50) | OUT | Token strings |
| └─ LS-SYMBOL-LEN(100) | 9(2) | OUT | Token lengths |

### Working Storage Organization

#### File Handling Variables
- WS-LISP-NAME (X(100)) - Dynamic file assignment
- WS-IN-LISP-RECORD (X(200)) - Main content buffer
- WS-LISP-EOF (X) - End-of-file flag
- WS-TEMP-NUM (9(10)) - Accumulator for concatenation

#### Formatting Variables
- WS-LISP-LENGTH (9(10)) - Current string length
- WS-FORMAT-STR-INDEX (9(10)) - Loop index
- WS-PAREN-TEMP-STR (X(2000)) - Temporary buffer for string manipulation
- WS-PAREN-LEFT (X) - Left spacing flag
- WS-PAREN-RIGHT (X) - Right spacing flag

#### Parsing Variables
- STRING-PTR (9(10)) - UNSTRING pointer
- WS-COUNT (9(10)) - Loop counter
- WS-FLAG (A(1)) - Tokenization end flag
- WS-PARSE-STR-INDEX (9(5)) - Character index
- WS-PARSE-EXPRESSION-LEN (9(5)) - Calculated token length

#### Logging Variables
- WS-LOG-OPERATION-FLAG (X(5)) - Logger operation ("ADD")
- WS-LOG-RECORD-FUNCTION-NAME (X(40)) - Procedure name
- WS-LOG-RECORD-MESSAGE (X(100)) - Log message

### File Section
- **FD LISP-FILE**: Line sequential organization
- **IN-LISP-RECORD** (X(200)): Input buffer for file reading

---

## Processing Flow

### Main Execution Sequence

```
MAIN-PROCEDURE
├─ 1. PERFORM FILE-HANDLING-PROCEDURE
│   ├─ OPEN INPUT LISP-FILE
│   ├─ READ first record
│   │   └─ PERFORM CALC-LISP-LENGTH (detect comment, measure)
│   ├─ PERFORM UNTIL WS-LISP-EOF="Y"
│   │   └─ PERFORM APPEND-LISP-PROCEDURE
│   │       └─ PERFORM CALC-LISP-LENGTH
│   ├─ CLOSE LISP-FILE
│   └─ CALL 'LOGGER' (log completion)
│
├─ 2. PERFORM TOKENIZE-LISP-PROCEDURE
│   ├─ PERFORM FORMAT-LISP-PROCEDURE
│   │   ├─ PERFORM CALC-LISP-LENGTH
│   │   ├─ Handle leading parenthesis (special case)
│   │   ├─ PERFORM VARYING loop (each character)
│   │   │   └─ On "(" or ")" → PERFORM FORMAT-PAREN-SPACE-PROCEDURE
│   │   │       ├─ PERFORM FORMAT-CHECK-PAREN-PROCEDURE
│   │   │       └─ PERFORM (conditionally):
│   │   │           ├─ FORMAT-ADD-BOTH-SPACES
│   │   │           ├─ FORMAT-ADD-RIGHT-SPACE
│   │   │           └─ FORMAT-ADD-LEFT-SPACE
│   │   └─ CALL 'LOGGER' (log formatting)
│   ├─ UNSTRING WS-IN-LISP-RECORD → LS-SYMBOL array
│   └─ CALL 'LOGGER' (log tokenization)
│
├─ 3. PERFORM CAL-LENGTH-ALL-SYMBOLS
│   └─ PERFORM VARYING (100 iterations)
│       └─ PERFORM CALC-LENGTH-SYMBOL
│
└─ 4. GOBACK (return to caller)
```

### Data Flow Diagram

```
Input File
    ↓
[IN-LISP-RECORD] (line by line)
    ↓
WS-CALC-LENGTH-STR → [CALC-LISP-LENGTH] → WS-LISP-LENGTH, WS-IS-COMMENT
    ↓
[WS-IN-LISP-RECORD] (accumulated, no comments)
    ↓
WS-PAREN-TEMP-STR ↔ [FORMAT-LISP-PROCEDURE] ↔ WS-IN-LISP-RECORD (spaced)
    ↓
[UNSTRING] → [LS-SYMBOL array] + LS-SYMBOL-TABLE-SIZE
    ↓
[CALC-LENGTH-SYMBOL] → [LS-SYMBOL-LEN array]
    ↓
Output to Caller (LS-LISP-SYMBOLS structure)
```

---

## Key Algorithms

### 1. Length Calculation with Trailing Space Elimination

**Purpose**: Calculate true string length, excluding trailing spaces

**Algorithm** (CALC-LISP-LENGTH):
```
Initialize: length = 0, space_accumulator = 0, is_comment = false
For each character (1 to 200):
    If character = ";" then
        Set is_comment = true
        Exit loop
    Else if character ≠ space then
        Add 1 to length
        Add space_accumulator to length
        Reset space_accumulator
    Else (character = space)
        Increment space_accumulator
End loop
Result: length (spaces at end not counted)
```

**Key Insight**: Delayed counting - spaces only added to length when followed by non-space

### 2. Dynamic String Formatting with Loop Extension

**Purpose**: Add spaces around parentheses while iterating

**Algorithm** (FORMAT-LISP-PROCEDURE):
```
Calculate initial_length
index = 1
If first char is "(" without space: add space, index = 3, length += 1

While index ≤ length:
    If char at index is "(" or ")":
        Check left: if not space, set left_flag
        Check right: if not space, set right_flag
        
        If both flags:
            Insert spaces both sides
            index += 1, length += 2
        Else if right_flag:
            Insert space right
            index += 1, length += 1
        Else if left_flag:
            Insert space left
            index += 1, length += 1
    
    index += 1 (automatic loop increment)
End while
```

**Key Insight**: Loop boundary (length) increases dynamically as spaces inserted

### 3. Space-Delimited Tokenization

**Purpose**: Split formatted string into tokens

**Algorithm** (TOKENIZE-LISP-PROCEDURE):
```
pointer = 1
symbol_count = 0
flag = false

For index = 1 to 100 (or until flag = true):
    UNSTRING source DELIMITED BY ALL ' '
        INTO symbol(index)
        WITH POINTER pointer
    
    If symbol(index) = spaces then
        flag = true (no more tokens)
    Else
        symbol_count += 1
    End if
End loop

Result: symbol array(1..symbol_count)
```

**Key Insight**: COBOL UNSTRING auto-advances pointer, ALL ' ' collapses multiple spaces

### 4. Character-Level Length Measurement

**Purpose**: Calculate token length excluding padding spaces

**Algorithm** (CALC-LENGTH-SYMBOL):
```
length = 0
ended = false
index = 1

While not ended AND index ≤ 100:
    If symbol(count)(index:1) = space then
        ended = true
    Else
        length += 1
    End if
    index += 1
End while

Result: length (first space marks end)
```

**Key Insight**: COBOL fixed-length strings padded with spaces; scan until first space

---

## External Dependencies

### 1. LOGGER Program
- **Type**: External COBOL program (CALL 'LOGGER')
- **Purpose**: Centralized audit logging
- **Called From**: 
  - FILE-HANDLING-PROCEDURE
  - TOKENIZE-LISP-PROCEDURE
  - FORMAT-LISP-PROCEDURE
- **Parameters**:
  - WS-LOG-OPERATION-FLAG (INPUT): Always "ADD"
  - WS-LOG-RECORD (INPUT): Function name + message
- **Frequency**: 3 calls per tokenization operation
- **Failure Impact**: If LOGGER unavailable, program will abend (no error handling)

### 2. LISP Source File
- **Type**: Line sequential text file
- **Format**: LISP s-expressions with optional comments
- **Location**: Specified by LS-LISP-FILE-NAME parameter
- **Requirements**:
  - Must exist and be readable
  - Single-line or multi-line LISP code
  - Comments start with ";"
- **Failure Impact**: File not found → COBOL runtime error (program abends)

### 3. Calling Program (CISP)
- **Type**: COBOL main program
- **Relationship**: CISP calls TOKENIZER as subroutine
- **Contract**:
  - CISP provides: LS-LISP-FILE-NAME, empty LS-LISP-SYMBOLS structure
  - TOKENIZER returns: populated LS-SYMBOL array, LS-SYMBOL-TABLE-SIZE
- **Coordination**: Array size (100) must match between programs

---

## Error Handling and Limitations

### Explicit Error Handling
- **None** - Program has no explicit error handling logic
- Relies on COBOL runtime error handling (abends on failure)

### Implicit Safety Mechanisms
1. **AT END clause**: Handles empty files gracefully
2. **WS-FLAG termination**: Prevents processing beyond last token
3. **Fixed array bounds**: Loops capped at 100 iterations
4. **Large temp buffer**: 2000-byte WS-PAREN-TEMP-STR prevents formatting overflow

### Known Limitations

#### 1. Buffer Size Constraints
- **WS-IN-LISP-RECORD**: 200-byte limit
- **Impact**: Complex LISP expressions truncated
- **Workaround**: None implemented

#### 2. Token Count Limit
- **LS-SYMBOL array**: Fixed 100-element capacity
- **Impact**: Tokens beyond 100 silently ignored
- **Detection**: Check if LS-SYMBOL-TABLE-SIZE = 100 (may indicate overflow)

#### 3. String Literal Handling
- **Issue**: Quoted strings with spaces split into multiple tokens
- **Example**: `"hello world"` → 2 tokens: `"hello` and `world"`
- **Impact**: Incorrect tokenization for string literals
- **Workaround**: LISP code should avoid spaced string literals (not realistic)

#### 4. Comment Syntax
- **Supported**: Single-line comments (`;`)
- **Not Supported**: Block comments, nested comments, mid-line comments
- **Example**: `(+ 1 2) ; comment` → entire line becomes comment

#### 5. File I/O Robustness
- **No FILE STATUS checking**: Open/read failures cause abends
- **No path validation**: Assumes file exists
- **No permission checking**: Assumes read access
- **Recommendation**: Add FILE STATUS handling for production use

#### 6. Unset Output Parameter
- **LS-SYMBOL-LENGTH**: Defined in LINKAGE but never set
- **Impact**: Calling program receives undefined value
- **Fix**: Add `MOVE LS-SYMBOL-TABLE-SIZE TO LS-SYMBOL-LENGTH` before GOBACK

---

## Performance Characteristics

### Time Complexity

| Phase | Complexity | Notes |
|-------|-----------|-------|
| File Reading | O(n) | n = lines in file |
| Length Calculation | O(m × k) | m = records, k = avg length (200) |
| Formatting | O(p²) worst | p = string length (can double) |
| Tokenization | O(t) | t = tokens (max 100) |
| Length Calculation | O(100 × 50) | Fixed: 100 tokens × 50 avg chars |

**Overall**: O(n + m×k + p² + t) ≈ **O(p²)** dominated by formatting

### Space Complexity
- **Static**: ~4.5 KB WORKING-STORAGE
- **File buffer**: 200 bytes
- **Temp buffer**: 2000 bytes (largest allocation)
- **Symbol table**: 100 × 50 = 5000 bytes (LINKAGE)
- **Total**: ~12 KB approximate

### Performance Bottlenecks

1. **STRING Operations** (most expensive)
   - 5 different STRING procedures
   - Each rebuilds entire string
   - Multiple invocations for each parenthesis
   - **Optimization**: Pre-calculate size, single-pass format

2. **Repeated Length Calculations**
   - CALC-LISP-LENGTH called multiple times on same data
   - Always scans full 200 characters
   - **Optimization**: Cache calculated lengths

3. **Fixed-Size Scans**
   - Length calculation always loops to 200
   - Token length always loops to 100
   - **Optimization**: Early termination on known length

### Scalability Limits
- **Maximum file size**: ~200 bytes concatenated content
- **Maximum tokens**: 100
- **Maximum token size**: 50 characters
- **Maximum file lines**: Unlimited (concatenated to 200 bytes)

---

## Maintenance Guide

### Common Maintenance Tasks

#### 1. Increase Token Capacity
**Change**: Support > 100 tokens
**Steps**:
1. Update array OCCURS clause in both TOKENIZER and CISP
2. Modify loop boundaries (WS-COUNT UNTIL condition)
3. Update documentation
**Impact**: Requires recompilation of both programs

#### 2. Increase Buffer Size
**Change**: Support LISP files > 200 bytes
**Steps**:
1. Increase WS-IN-LISP-RECORD PIC X(nnn)
2. Increase IN-LISP-RECORD PIC X(nnn)
3. Increase WS-CALC-LENGTH-STR PIC X(nnn)
4. Update WS-MAX-LISP-LENGTH constant
5. Increase WS-PAREN-TEMP-STR (at least 10× buffer size)
**Impact**: Memory usage increases proportionally

#### 3. Add Error Handling
**Change**: Trap file errors instead of abending
**Steps**:
1. Add FILE STATUS clause to SELECT statement
2. Declare WS-FILE-STATUS PIC XX
3. Check status after OPEN, READ, CLOSE
4. Add error reporting logic
**Impact**: More robust, user-friendly error messages

#### 4. Fix String Literal Tokenization
**Change**: Keep quoted strings as single tokens
**Steps**:
1. Modify FORMAT-LISP-PROCEDURE to detect quotes
2. Add WS-IN-STRING flag
3. Skip space insertion while in string
4. Handle escaped quotes
**Impact**: Correct tokenization for string literals

### Code Modification Guidelines

#### Safe Changes
- Increasing buffer sizes (within COBOL limits)
- Adding new debug DISPLAY statements (use D directive)
- Adding logging calls
- Renaming variables (with global find/replace)

#### Risky Changes
- Modifying loop boundaries (test thoroughly)
- Changing STRING operation logic (verify with examples)
- Altering index manipulation in FORMAT-CHECK-PAREN
- Removing "unused" variables (may be referenced dynamically)

### Testing After Changes
1. **Sanity tests**: Simple expressions `(+ 1 2)`
2. **Nested tests**: Deep nesting `((((a))))`
3. **Boundary tests**: Expressions near buffer limits
4. **Comment tests**: Lines with comments
5. **Regression tests**: Compare output with known-good results

---

## Testing Recommendations

### Unit Test Cases

#### Test Suite 1: Length Calculation
```
Input: "(foo)    "
Expected: WS-LISP-LENGTH = 5, WS-IS-COMMENT = FALSE

Input: "; comment"
Expected: WS-IS-COMMENT = TRUE

Input: "  hello  world  "
Expected: WS-LISP-LENGTH = 13 (includes embedded spaces)
```

#### Test Suite 2: Formatting
```
Input: "(foo)"
Expected: "( foo )"

Input: "((a)(b))"
Expected: "( ( a ) ( b ) )"

Input: " ( foo ) "
Expected: " ( foo ) " (no change)
```

#### Test Suite 3: Tokenization
```
Input: "( + 1 2 )"
Expected: 
  LS-SYMBOL(1) = "("
  LS-SYMBOL(2) = "+"
  LS-SYMBOL(3) = "1"
  LS-SYMBOL(4) = "2"
  LS-SYMBOL(5) = ")"
  LS-SYMBOL-TABLE-SIZE = 5
```

#### Test Suite 4: Symbol Lengths
```
Input symbols: ["(", "defun", "foo", ")"]
Expected lengths: [1, 5, 3, 1]
```

### Integration Test Cases

#### Test 1: Simple Function
**File**: `(defun square (x) (* x x))`
**Expected**: 11 tokens

#### Test 2: Multi-line with Comments
**File**:
```
; Calculate factorial
(defun factorial (n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))
```
**Expected**: ~20 tokens, comment line excluded

#### Test 3: Empty File
**File**: (empty)
**Expected**: LS-SYMBOL-TABLE-SIZE = 0

#### Test 4: Comment-Only File
**File**: `; This is a comment`
**Expected**: LS-SYMBOL-TABLE-SIZE = 0

### Stress Test Cases

1. **Maximum tokens**: File with exactly 100 tokens
2. **Overflow tokens**: File with >100 tokens (verify truncation)
3. **Maximum length**: File reaching 200-byte limit
4. **Deep nesting**: 50 levels of nested parentheses
5. **Extreme spacing**: Multiple consecutive spaces/tabs

---

## Appendices

### A. Related Documentation
- [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md) - Complete variable reference
- [tokenizer_CALL_GRAPH.md](tokenizer_CALL_GRAPH.md) - Procedure relationships
- [tokenizer_VARIABLE_MUTATIONS.md](tokenizer_VARIABLE_MUTATIONS.md) - State change analysis
- [tokenizer_MERMAID_DIAGRAMS.md](tokenizer_MERMAID_DIAGRAMS.md) - Visual representations
- [paragraphs/](paragraphs/) - Individual procedure documentation

### B. LISP Syntax Reference
- **S-Expression**: `(function arg1 arg2 ...)`
- **Atom**: `symbol`, `123`, `"string"`
- **List**: `(a b c)`
- **Comment**: `; comment text`
- **Common functions**: `defun`, `lambda`, `let`, `if`, `+`, `-`, `*`, `/`

### C. COBOL Compiler Requirements
- **GnuCOBOL** or equivalent COBOL-85+ compiler
- **Features used**:
  - UNSTRING with POINTER
  - STRING with DELIMITED BY SIZE
  - Nested EVALUATE statements
  - 88-level condition names
  - VARYING phrase on PERFORM
  - CALL verb for external programs

### D. Glossary
- **Token**: Atomic lexical unit (symbol, number, operator, parenthesis)
- **S-Expression**: Symbolic expression (LISP syntax)
- **Atom**: Indivisible LISP value (not a list)
- **Chunk**: Fixed-size portion of data for processing
- **UNSTRING**: COBOL verb for parsing delimited strings
- **STRING**: COBOL verb for concatenating strings

---

## Revision History

| Date | Version | Author | Changes |
|------|---------|--------|---------|
| 2026-01-20 | 1.0 | AI (Claude) | Initial comprehensive documentation |

---

*This comprehensive documentation is AI-generated and should be reviewed by COBOL experts for accuracy. It synthesizes information from data dictionary, call graph, variable mutation analysis, and source code inspection.*
