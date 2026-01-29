# TOKENIZER Program - Visual Diagrams (Mermaid)

## Program Information
- **Program Name**: TOKENIZER
- **Repository**: Cisp
- **Generated**: 2026-01-20

---

## Purpose

This document contains **mandatory** Mermaid visual representations of the TOKENIZER program structure. These diagrams are essential for:
- Understanding program flow without reading raw COBOL
- Knowledge transfer to developers unfamiliar with COBOL
- Identifying refactoring opportunities
- Documenting dependencies for modernization
- Compliance and audit documentation

All diagrams render automatically in GitHub, GitLab, VS Code (with Mermaid extension), and most modern Markdown viewers.

---

## 1. Program Flow Diagram

This flowchart shows the main processing logic from program start to end, including decision points and major processing sections.

```mermaid
flowchart TD
    Start([MAIN-PROCEDURE Start]) --> FileHandling[FILE-HANDLING-PROCEDURE<br/>Read LISP File]
    FileHandling --> OpenFile[OPEN INPUT LISP-FILE]
    OpenFile --> ReadFirst{READ First Record}
    ReadFirst -->|AT END| SetEOF1[Set WS-LISP-EOF]
    ReadFirst -->|NOT AT END| CalcLen1[CALC-LISP-LENGTH]
    CalcLen1 --> CheckComment1{Is Comment?}
    CheckComment1 -->|Yes| ReadLoop
    CheckComment1 -->|No| InitBuffer[Initialize WS-IN-LISP-RECORD]
    
    InitBuffer --> ReadLoop{WS-LISP-EOF = Y?}
    SetEOF1 --> ReadLoop
    ReadLoop -->|Yes| CloseFile[CLOSE LISP-FILE]
    ReadLoop -->|No| ReadNext{READ Next Record}
    
    ReadNext -->|AT END| SetEOF2[Set WS-LISP-EOF = Y]
    SetEOF2 --> ReadLoop
    ReadNext -->|NOT AT END| AppendProc[APPEND-LISP-PROCEDURE<br/>Concatenate Line]
    AppendProc --> ReadLoop
    
    CloseFile --> LogFile[Log File Completion]
    LogFile --> Tokenize[TOKENIZE-LISP-PROCEDURE]
    
    Tokenize --> Format[FORMAT-LISP-PROCEDURE<br/>Add Spaces Around Parens]
    Format --> Unstring[UNSTRING<br/>Split by Spaces]
    Unstring --> CountTokens[Count Valid Tokens<br/>LS-SYMBOL-TABLE-SIZE]
    CountTokens --> LogToken[Log Tokenization]
    
    LogToken --> CalcLengths[CAL-LENGTH-ALL-SYMBOLS<br/>Calculate Token Lengths]
    CalcLengths --> LoopSymbols[Loop 100 Times]
    LoopSymbols --> CalcSymLen[CALC-LENGTH-SYMBOL<br/>for Each Token]
    CalcSymLen --> StoreLens[Store in LS-SYMBOL-LEN]
    
    StoreLens --> End([GOBACK<br/>Return to Caller])
    
    style Start fill:#90EE90
    style End fill:#FFB6C1
    style FileHandling fill:#87CEEB
    style Tokenize fill:#87CEEB
    style CalcLengths fill:#87CEEB
    style Format fill:#FFE4B5
```

**Key Insights**:
- Three main processing phases: File I/O → Tokenization → Length calculation
- Comment detection in file reading prevents invalid data from entering pipeline
- Fixed 100-iteration loop for length calculation (processes all array slots)
- No error handling paths (all errors cause runtime abend)

---

## 2. PERFORM Hierarchy (Call Graph)

This graph shows all paragraph call relationships, illustrating the program's modular structure.

```mermaid
graph TD
    Main[MAIN-PROCEDURE] --> FileProc[FILE-HANDLING-PROCEDURE]
    Main --> TokenProc[TOKENIZE-LISP-PROCEDURE]
    Main --> CalcAll[CAL-LENGTH-ALL-SYMBOLS]
    Main -.-> PrintSymbols[PRINT-SYMBOL-TABLE<br/><i>Debug Only</i>]
    
    FileProc --> CalcLen1[CALC-LISP-LENGTH]
    FileProc --> AppendProc[APPEND-LISP-PROCEDURE]
    FileProc --> Logger1[CALL LOGGER<br/><i>External</i>]
    
    AppendProc --> CalcLen2[CALC-LISP-LENGTH]
    
    TokenProc --> FormatProc[FORMAT-LISP-PROCEDURE]
    TokenProc --> Logger2[CALL LOGGER<br/><i>External</i>]
    
    FormatProc --> CalcLen3[CALC-LISP-LENGTH]
    FormatProc --> FormatParen[FORMAT-PAREN-SPACE-PROCEDURE]
    FormatProc --> Logger3[CALL LOGGER<br/><i>External</i>]
    
    FormatParen --> CheckParen[FORMAT-CHECK-PAREN-PROCEDURE]
    FormatParen --> AddBoth[FORMAT-ADD-BOTH-SPACES]
    FormatParen --> AddRight[FORMAT-ADD-RIGHT-SPACE]
    FormatParen --> AddLeft[FORMAT-ADD-LEFT-SPACE]
    
    CalcAll --> CalcSymLen[CALC-LENGTH-SYMBOL]
    
    style Main fill:#FFD700
    style FileProc fill:#87CEEB
    style TokenProc fill:#87CEEB
    style CalcAll fill:#87CEEB
    style Logger1 fill:#FF6347
    style Logger2 fill:#FF6347
    style Logger3 fill:#FF6347
    style PrintSymbols fill:#D3D3D3
```

**Key Insights**:
- Main procedure orchestrates 3 primary operations (clean separation of concerns)
- CALC-LISP-LENGTH is shared utility (called from 3 different procedures)
- Formatting has deepest nesting (4 levels: Main → Tokenize → Format → FormatParen → Add*)
- External dependency on LOGGER (3 audit points)
- Debug procedure (PRINT-SYMBOL-TABLE) isolated from main flow

---

## 3. Data Flow Diagram

This flowchart illustrates how data moves through the program from input file to output symbol table.

```mermaid
flowchart LR
    Input[(LISP-FILE<br/>Source Code)] --> ReadBuf[IN-LISP-RECORD<br/>Line Buffer]
    ReadBuf --> CalcStr[WS-CALC-LENGTH-STR<br/>Length Analysis]
    CalcStr --> Comment{Is Comment<br/>Semicolon?}
    Comment -->|Yes| Skip[Skip Line]
    Comment -->|No| Accum[WS-IN-LISP-RECORD<br/>Accumulated Buffer]
    
    Accum --> Format[WS-PAREN-TEMP-STR<br/>Formatting Buffer]
    Format --> Formatted[WS-IN-LISP-RECORD<br/>Spaced Parens]
    Formatted --> Unstring[UNSTRING<br/>Operation]
    
    Unstring --> SymArray[LS-SYMBOL Array<br/>100 Tokens]
    Unstring --> SymCount[LS-SYMBOL-TABLE-SIZE<br/>Token Count]
    
    SymArray --> LenCalc[Length<br/>Calculation]
    LenCalc --> LenArray[LS-SYMBOL-LEN Array<br/>Token Lengths]
    
    SymArray --> Output1[Output to Caller]
    SymCount --> Output1
    LenArray --> Output1
    Output1 --> Caller[CISP Program]
    
    style Input fill:#90EE90
    style ReadBuf fill:#FFE4B5
    style CalcStr fill:#FFE4B5
    style Accum fill:#87CEEB
    style Format fill:#FFE4B5
    style Formatted fill:#87CEEB
    style SymArray fill:#FFD700
    style SymCount fill:#FFD700
    style LenArray fill:#FFD700
    style Caller fill:#FFB6C1
```

**Key Insights**:
- Data flows through 4 distinct buffers (IN-LISP-RECORD → WS-IN-LISP-RECORD → WS-PAREN-TEMP-STR → LS-SYMBOL)
- Comment detection acts as filter (prevents invalid data from progressing)
- Single input file produces 3 outputs (symbol array, count, lengths)
- No intermediate file I/O (all processing in memory)

---

## 4. File I/O Operations Timeline

This sequence diagram shows all file operations in execution order, illustrating the read-process-close pattern.

```mermaid
sequenceDiagram
    participant M as MAIN-PROCEDURE
    participant F as FILE-HANDLING-PROCEDURE
    participant LF as LISP-FILE
    participant A as APPEND-LISP-PROCEDURE
    participant L as LOGGER
    
    M->>F: PERFORM
    F->>LF: OPEN INPUT
    LF-->>F: File Opened
    
    F->>LF: READ (first record)
    alt AT END
        LF-->>F: EOF Flag Set
    else NOT AT END
        LF-->>F: IN-LISP-RECORD
        F->>F: CALC-LISP-LENGTH
        alt Not Comment
            F->>F: Initialize WS-IN-LISP-RECORD
        end
    end
    
    loop Until EOF
        F->>LF: READ (next record)
        alt AT END
            LF-->>F: EOF Flag Set
        else NOT AT END
            LF-->>F: IN-LISP-RECORD
            F->>A: PERFORM APPEND-LISP-PROCEDURE
            A->>A: CALC-LISP-LENGTH
            alt Not Comment
                A->>A: Concatenate to WS-IN-LISP-RECORD
            end
            A-->>F: Return
        end
    end
    
    F->>LF: CLOSE
    LF-->>F: File Closed
    
    F->>L: CALL LOGGER (completion)
    L-->>F: Log Written
    
    F-->>M: Return (WS-IN-LISP-RECORD populated)
```

**Key Insights**:
- Single file opened (LISP-FILE) - no output files
- Read operations: 1 initial + N loop iterations (N = line count)
- Comment filtering happens inline during reading (not post-processing)
- LOGGER called after file closed (ensures all I/O complete)
- File handle lifecycle: OPEN → READ (multiple) → CLOSE (clean pattern)

---

## 5. Formatting Algorithm State Diagram

This state diagram shows how the formatting procedure processes parentheses to add appropriate spacing.

```mermaid
stateDiagram-v2
    [*] --> Initialize: FORMAT-LISP-PROCEDURE Called
    Initialize --> CalcLength: Calculate Initial Length
    CalcLength --> CheckLeading: Check First Character
    
    CheckLeading --> SpecialCase: First Char = "(" No Space
    CheckLeading --> MainLoop: Otherwise
    SpecialCase --> MainLoop: Add Space After "("
    
    MainLoop --> CheckChar: Evaluate Current Character
    CheckChar --> OtherChar: Not "(" or ")"
    CheckChar --> ParenFound: "(" or ")"
    
    OtherChar --> NextChar: Continue Loop
    
    ParenFound --> CheckSides: FORMAT-CHECK-PAREN-PROCEDURE
    CheckSides --> CheckLeft: Examine Left Character
    CheckLeft --> CheckRight: Examine Right Character
    CheckRight --> Determine: Determine Spacing Needs
    
    Determine --> BothSpaces: Left & Right Not Space
    Determine --> RightSpace: Only Right Not Space
    Determine --> LeftSpace: Only Left Not Space
    Determine --> NoSpace: Both Sides Have Space
    
    BothSpaces --> AddBoth: FORMAT-ADD-BOTH-SPACES
    RightSpace --> AddRight: FORMAT-ADD-RIGHT-SPACE
    LeftSpace --> AddLeft: FORMAT-ADD-LEFT-SPACE
    NoSpace --> NextChar
    
    AddBoth --> UpdateLength: Length += 2, Index += 1
    AddRight --> UpdateLength: Length += 1, Index += 1
    AddLeft --> UpdateLength: Length += 1, Index += 1
    
    UpdateLength --> NextChar: Continue Loop
    
    NextChar --> LoopCheck: Index > Length?
    LoopCheck --> MainLoop: No
    LoopCheck --> LogComplete: Yes
    
    LogComplete --> [*]: Return to Caller
    
    note right of CheckSides
        Dynamic loop:
        Length increases
        as spaces added
    end note
    
    note right of UpdateLength
        Index adjusted
        to skip new space
    end note
```

**Key Insights**:
- State machine handles 4 spacing scenarios (both, left, right, none)
- Dynamic termination condition (length increases during processing)
- Special case handling for leading parenthesis (optimization)
- Index manipulation ensures newly inserted spaces aren't re-processed

---

## 6. Token Extraction Process

This flowchart details the tokenization logic using COBOL's UNSTRING operation.

```mermaid
flowchart TD
    Start([TOKENIZE-LISP-PROCEDURE]) --> Init[Initialize:<br/>STRING-PTR = 1<br/>COUNT = 0<br/>FLAG = FALSE]
    Init --> Loop{WS-COUNT < 100<br/>AND NOT FLAG?}
    
    Loop -->|No| LogCompletion[Log Completion]
    Loop -->|Yes| Unstring[UNSTRING<br/>WS-IN-LISP-RECORD<br/>DELIMITED BY ALL ' '<br/>INTO LS-SYMBOL]
    
    Unstring --> AutoPointer[STRING-PTR<br/>Auto-Advanced]
    AutoPointer --> CheckEmpty{LS-SYMBOL<br/>= SPACES?}
    
    CheckEmpty -->|Yes| SetFlag[SET WS-FLAG-YES]
    CheckEmpty -->|No| Increment[ADD 1 TO<br/>LS-SYMBOL-TABLE-SIZE]
    
    SetFlag --> Loop
    Increment --> NextIteration[WS-COUNT += 1]
    NextIteration --> Loop
    
    LogCompletion --> Return([Return to Caller])
    
    style Start fill:#90EE90
    style Unstring fill:#FFD700
    style CheckEmpty fill:#FFE4B5
    style Return fill:#FFB6C1
```

**Key Insights**:
- UNSTRING is single operation (COBOL handles all parsing)
- `ALL ' '` delimiter collapses multiple spaces (treats as single delimiter)
- STRING-PTR automatically advanced by UNSTRING (tracks position)
- Early termination on SPACES (indicates no more tokens)
- Maximum 100 tokens (hard limit)

---

## 7. Variable Mutation Timeline

This diagram shows the lifecycle and state transitions of the primary data buffer (WS-IN-LISP-RECORD).

```mermaid
stateDiagram-v2
    [*] --> Uninitialized: Program Start
    
    Uninitialized --> FirstLine: FILE-HANDLING-PROCEDURE<br/>READ First Non-Comment
    
    FirstLine --> SingleLine: MOVE IN-LISP-RECORD<br/>TO WS-IN-LISP-RECORD
    
    SingleLine --> MultiLine: APPEND-LISP-PROCEDURE<br/>Concatenate Additional Lines
    
    MultiLine --> MultiLine: Loop Until EOF<br/>STRING Operations
    
    MultiLine --> Concatenated: All Lines Read<br/>File Closed
    
    Concatenated --> Formatting: FORMAT-LISP-PROCEDURE<br/>Add Spaces Around Parens
    
    Formatting --> Formatted: Multiple STRING Operations<br/>Parens Now Spaced
    
    Formatted --> Tokenizing: TOKENIZE-LISP-PROCEDURE<br/>UNSTRING Operation
    
    Tokenizing --> Consumed: Tokens Extracted<br/>Buffer Read-Only
    
    Consumed --> [*]: GOBACK
    
    note right of MultiLine
        Length tracked in
        WS-TEMP-NUM
    end note
    
    note right of Formatting
        Length dynamically
        increases during
        processing
    end note
    
    note right of Tokenizing
        Source for UNSTRING
        Not modified
    end note
```

**Key Insights**:
- Buffer serves 3 distinct roles: accumulator, formatter target, tokenization source
- State transitions are one-way (no rollback or undo)
- Formatting phase most complex (multiple intermediate states)
- Final state is read-only consumption (buffer not modified by UNSTRING)

---

## 8. Symbol Length Calculation Flow

This flowchart shows the character-level scanning used to calculate actual token length.

```mermaid
flowchart TD
    Start([CALC-LENGTH-SYMBOL Called]) --> Init[Initialize:<br/>WS-PARSE-EXPRESSION-LEN = 0<br/>WS-PARSE-HAS-ENDED = FALSE]
    Init --> Loop{Index ≤ 100<br/>AND NOT Ended?}
    
    Loop -->|No| Store[MOVE Length TO<br/>LS-SYMBOL-LEN]
    Loop -->|Yes| GetChar[Get Character at<br/>LS-SYMBOL Index]
    
    GetChar --> CheckSpace{Character<br/>= Space?}
    
    CheckSpace -->|Yes| SetEnd[SET<br/>WS-PARSE-HAS-ENDED]
    CheckSpace -->|No| AddLen[ADD 1 TO<br/>WS-PARSE-EXPRESSION-LEN]
    
    SetEnd --> Loop
    AddLen --> NextChar[Index += 1]
    NextChar --> Loop
    
    Store --> Return([Return to Caller])
    
    style Start fill:#90EE90
    style GetChar fill:#FFE4B5
    style CheckSpace fill:#FFD700
    style Return fill:#FFB6C1
```

**Key Insights**:
- Scans character-by-character until space found
- Fixed-length COBOL strings padded with spaces (need trimming)
- Maximum 100 characters scanned per token
- Length stored immediately after calculation (used by caller)

---

## Diagram Usage Guide

### Viewing Diagrams
- **GitHub/GitLab**: Native Mermaid rendering (automatic)
- **VS Code**: Install "Markdown Preview Mermaid Support" extension
- **Online**: https://mermaid.live (paste Mermaid code)
- **Export**: Mermaid Live Editor allows PNG/SVG export

### Editing Diagrams
1. Copy Mermaid code block
2. Paste into Mermaid Live Editor
3. Edit syntax (real-time preview)
4. Validate rendering
5. Copy back into documentation

### Mermaid Resources
- **Documentation**: https://mermaid.js.org/intro/
- **Syntax Guide**: https://mermaid.js.org/intro/syntax-reference.html
- **Examples**: https://mermaid.js.org/ecosystem/integrations.html

---

## Cross-References

These diagrams complement the following documentation:
- [tokenizer_COMPREHENSIVE_DOC.md](tokenizer_COMPREHENSIVE_DOC.md) - Narrative description
- [tokenizer_CALL_GRAPH.md](tokenizer_CALL_GRAPH.md) - Detailed call hierarchy
- [tokenizer_VARIABLE_MUTATIONS.md](tokenizer_VARIABLE_MUTATIONS.md) - State analysis
- [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md) - Variable details

---

*These Mermaid diagrams are AI-generated and should be reviewed by COBOL experts for accuracy. Last updated: 2026-01-20*
