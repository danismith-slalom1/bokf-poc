# LOGGER Mermaid Diagrams

**Program**: LOGGER  
**Repository**: lauryndbrown/Cisp  
**Last Updated**: January 20, 2026  
**Purpose**: Visual diagrams for understanding LOGGER program structure and flow

---

## Table of Contents

1. [Program Structure Diagram](#program-structure-diagram)
2. [Call Graph (Flowchart)](#call-graph-flowchart)
3. [Normal Logging Session Sequence](#normal-logging-session-sequence)
4. [State Machine Diagram](#state-machine-diagram)
5. [Data Flow Diagram](#data-flow-diagram)
6. [Error Handling Flow](#error-handling-flow)
7. [Record Structure](#record-structure)

---

## Program Structure Diagram

```mermaid
graph TB
    PROG[LOGGER Program]
    
    subgraph "IDENTIFICATION DIVISION"
        PID[PROGRAM-ID: LOGGER<br/>Author: Lauryn Brown<br/>Purpose: Log activity]
    end
    
    subgraph "ENVIRONMENT DIVISION"
        FC[FILE-CONTROL<br/>LOG-FILE: OPTIONAL, DYNAMIC<br/>LINE SEQUENTIAL]
    end
    
    subgraph "DATA DIVISION"
        FS[FILE SECTION<br/>LOG-RECORD 150 bytes]
        WS[WORKING-STORAGE<br/>WS-LOG-FILE-NAME 20 bytes]
        LS[LINKAGE SECTION<br/>LS-LOG-OPERATION-FLAG<br/>LS-LOG-RECORD]
    end
    
    subgraph "PROCEDURE DIVISION"
        MAIN[MAIN-PROCEDURE<br/>Dispatcher]
        INIT[LOG-INIT-PROCEDURE<br/>OPEN Handler]
        WRITE[LOG-WRITE-TO-PROCEDURE<br/>ADD Handler]
        CLOSE[LOG-CLOSE-PROCEDURE<br/>CLOSE Handler]
        ERROR[LOG-FLAG-ERROR-PROCEDURE<br/>Error Handler]
    end
    
    PROG --> PID
    PROG --> FC
    PROG --> FS
    PROG --> WS
    PROG --> LS
    PROG --> MAIN
    
    MAIN -->|WHEN "OPEN"| INIT
    MAIN -->|WHEN "ADD"| WRITE
    MAIN -->|WHEN "CLOSE"| CLOSE
    MAIN -->|WHEN OTHER| ERROR
    
    style PROG fill:#e1f5ff
    style MAIN fill:#fff4e6
    style INIT fill:#e8f5e9
    style WRITE fill:#e8f5e9
    style CLOSE fill:#e8f5e9
    style ERROR fill:#ffebee
```

---

## Call Graph (Flowchart)

```mermaid
flowchart TD
    START([External Program<br/>Calls LOGGER])
    ENTRY[PROCEDURE DIVISION USING<br/>LS-LOG-OPERATION-FLAG<br/>LS-LOG-RECORD]
    MAIN{MAIN-PROCEDURE<br/>EVALUATE FLAG}
    
    INIT[LOG-INIT-PROCEDURE]
    INIT1[MOVE path to WS-LOG-FILE-NAME]
    INIT2[OPEN OUTPUT LOG-FILE]
    INIT3[MOVE 1 to LOG-RECORD-ID]
    INIT4[Set function = LOG-INIT-PROCEDURE]
    INIT5[Set message = Starting Program!]
    INIT6[WRITE LOG-RECORD]
    
    WRITE_OP[LOG-WRITE-TO-PROCEDURE]
    WRITE1[ADD 1 to LOG-RECORD-ID]
    WRITE2[Copy LS-LOG-RECORD-FUNCTION-NAME]
    WRITE3[Copy LS-LOG-RECORD-MESSAGE]
    WRITE4[WRITE LOG-RECORD]
    
    CLOSE_OP[LOG-CLOSE-PROCEDURE]
    CLOSE1[ADD 1 to LOG-RECORD-ID]
    CLOSE2[Set function = LOGGER:LOG-CLOSE-PROCEDURE]
    CLOSE3[Set message = Closed logging file]
    CLOSE4[WRITE LOG-RECORD]
    CLOSE5[CLOSE LOG-FILE]
    
    ERROR_OP[LOG-FLAG-ERROR-PROCEDURE]
    ERROR1[DISPLAY READ FLAG ERROR]
    
    RETURN([GOBACK<br/>Return to Caller])
    
    START --> ENTRY
    ENTRY --> MAIN
    
    MAIN -->|OPEN| INIT
    INIT --> INIT1 --> INIT2 --> INIT3 --> INIT4 --> INIT5 --> INIT6 --> RETURN
    
    MAIN -->|ADD| WRITE_OP
    WRITE_OP --> WRITE1 --> WRITE2 --> WRITE3 --> WRITE4 --> RETURN
    
    MAIN -->|CLOSE| CLOSE_OP
    CLOSE_OP --> CLOSE1 --> CLOSE2 --> CLOSE3 --> CLOSE4 --> CLOSE5 --> RETURN
    
    MAIN -->|OTHER| ERROR_OP
    ERROR_OP --> ERROR1 --> RETURN
    
    style START fill:#e1f5ff
    style MAIN fill:#fff4e6
    style INIT fill:#e8f5e9
    style WRITE_OP fill:#e8f5e9
    style CLOSE_OP fill:#e8f5e9
    style ERROR_OP fill:#ffebee
    style RETURN fill:#e1f5ff
```

---

## Normal Logging Session Sequence

```mermaid
sequenceDiagram
    participant Caller as Calling Program
    participant MAIN as MAIN-PROCEDURE
    participant INIT as LOG-INIT-PROCEDURE
    participant WRITE as LOG-WRITE-TO-PROCEDURE
    participant CLOSE as LOG-CLOSE-PROCEDURE
    participant File as LOG-FILE

    Note over Caller: Initialize Logging
    Caller->>MAIN: CALL LOGGER("OPEN", ...)
    MAIN->>INIT: PERFORM LOG-INIT-PROCEDURE
    INIT->>File: OPEN OUTPUT LOG-FILE
    Note over INIT,File: Creates/truncates file
    INIT->>INIT: MOVE 1 to LOG-RECORD-ID
    INIT->>File: WRITE record (ID=1, "LOG-INIT-PROCEDURE", "Starting Program!")
    INIT-->>MAIN: Return
    MAIN-->>Caller: GOBACK

    Note over Caller: Log First Entry
    Caller->>MAIN: CALL LOGGER("ADD", function1, message1)
    MAIN->>WRITE: PERFORM LOG-WRITE-TO-PROCEDURE
    WRITE->>WRITE: ADD 1 to LOG-RECORD-ID (now 2)
    WRITE->>WRITE: Copy function1, message1
    WRITE->>File: WRITE record (ID=2, function1, message1)
    WRITE-->>MAIN: Return
    MAIN-->>Caller: GOBACK

    Note over Caller: Log Second Entry
    Caller->>MAIN: CALL LOGGER("ADD", function2, message2)
    MAIN->>WRITE: PERFORM LOG-WRITE-TO-PROCEDURE
    WRITE->>WRITE: ADD 1 to LOG-RECORD-ID (now 3)
    WRITE->>WRITE: Copy function2, message2
    WRITE->>File: WRITE record (ID=3, function2, message2)
    WRITE-->>MAIN: Return
    MAIN-->>Caller: GOBACK

    Note over Caller: Close Logging
    Caller->>MAIN: CALL LOGGER("CLOSE", ...)
    MAIN->>CLOSE: PERFORM LOG-CLOSE-PROCEDURE
    CLOSE->>CLOSE: ADD 1 to LOG-RECORD-ID (now 4)
    CLOSE->>File: WRITE record (ID=4, "LOGGER:LOG-CLOSE-PROCEDURE", "Closed logging file")
    CLOSE->>File: CLOSE LOG-FILE
    Note over CLOSE,File: Flushes buffers, releases handle
    CLOSE-->>MAIN: Return
    MAIN-->>Caller: GOBACK
```

---

## State Machine Diagram

```mermaid
stateDiagram-v2
    [*] --> Closed: Program Start

    Closed --> Open: OPEN Operation<br/>(LOG-INIT-PROCEDURE)
    note right of Open
        File created/truncated
        LOG-RECORD-ID = 1
        Init record written
    end note

    Open --> Open: ADD Operation<br/>(LOG-WRITE-TO-PROCEDURE)
    note right of Open
        LOG-RECORD-ID incremented
        User record written
    end note

    Open --> Closed: CLOSE Operation<br/>(LOG-CLOSE-PROCEDURE)
    note right of Closed
        Close record written
        File handle released
        Buffers flushed
    end note

    Closed --> Closed: Invalid Operation<br/>(LOG-FLAG-ERROR)
    note left of Closed
        Error: ADD/CLOSE<br/>without OPEN
    end note

    Open --> Open: Invalid Operation<br/>(LOG-FLAG-ERROR)
    note left of Open
        Error: Invalid flag<br/>during session
    end note

    Closed --> [*]: Program End
```

---

## Data Flow Diagram

```mermaid
flowchart LR
    subgraph "Calling Program"
        FLAG[LS-LOG-OPERATION-FLAG<br/>OPEN/ADD/CLOSE]
        FUNC[LS-LOG-RECORD-FUNCTION-NAME<br/>40 bytes]
        MSG[LS-LOG-RECORD-MESSAGE<br/>100 bytes]
    end

    subgraph "LOGGER - MAIN-PROCEDURE"
        EVAL{EVALUATE<br/>Operation Flag}
    end

    subgraph "Handler Procedures"
        INIT_H[LOG-INIT]
        WRITE_H[LOG-WRITE-TO]
        CLOSE_H[LOG-CLOSE]
    end

    subgraph "Internal Data"
        WS_FILE[WS-LOG-FILE-NAME<br/>File path]
        REC_ID[LOG-RECORD-ID<br/>Counter]
        REC_FUNC[LOG-RECORD-FUNCTION-NAME<br/>40 bytes]
        REC_MSG[LOG-RECORD-MESSAGE<br/>100 bytes]
    end

    subgraph "File System"
        LOG_FILE[LOG-FILE<br/>../logs/log.data]
    end

    FLAG --> EVAL
    FUNC -.->|ADD only| WRITE_H
    MSG -.->|ADD only| WRITE_H

    EVAL -->|OPEN| INIT_H
    EVAL -->|ADD| WRITE_H
    EVAL -->|CLOSE| CLOSE_H

    INIT_H --> WS_FILE
    INIT_H --> REC_ID
    INIT_H --> REC_FUNC
    INIT_H --> REC_MSG

    WRITE_H --> REC_ID
    WRITE_H --> REC_FUNC
    WRITE_H --> REC_MSG

    CLOSE_H --> REC_ID
    CLOSE_H --> REC_FUNC
    CLOSE_H --> REC_MSG

    WS_FILE -.->|Dynamic assignment| LOG_FILE
    REC_ID --> LOG_FILE
    REC_FUNC --> LOG_FILE
    REC_MSG --> LOG_FILE

    style FLAG fill:#e1f5ff
    style FUNC fill:#e1f5ff
    style MSG fill:#e1f5ff
    style LOG_FILE fill:#fff4e6
```

---

## Error Handling Flow

```mermaid
flowchart TD
    START([LOGGER Called])
    CHECK_FLAG{Valid Operation<br/>Flag?}
    
    CHECK_OPEN{Is OPEN<br/>Operation?}
    FILE_OPEN{File Already<br/>Open?}
    
    CHECK_ADD{Is ADD<br/>Operation?}
    FILE_FOR_ADD{File Open<br/>for ADD?}
    
    CHECK_CLOSE{Is CLOSE<br/>Operation?}
    FILE_FOR_CLOSE{File Open<br/>for CLOSE?}
    
    ERROR_FLAG[LOG-FLAG-ERROR-PROCEDURE<br/>Display Error]
    ERROR_STATE[State Error:<br/>File not open]
    ERROR_DOUBLE[Error:<br/>File already open]
    
    SUCCESS_INIT[LOG-INIT-PROCEDURE<br/>Success]
    SUCCESS_WRITE[LOG-WRITE-TO-PROCEDURE<br/>Success]
    SUCCESS_CLOSE[LOG-CLOSE-PROCEDURE<br/>Success]
    
    RETURN([GOBACK])
    
    START --> CHECK_FLAG
    
    CHECK_FLAG -->|Yes| CHECK_OPEN
    CHECK_FLAG -->|No| ERROR_FLAG
    
    CHECK_OPEN -->|Yes| FILE_OPEN
    CHECK_OPEN -->|No| CHECK_ADD
    
    FILE_OPEN -->|Yes| ERROR_DOUBLE
    FILE_OPEN -->|No| SUCCESS_INIT
    
    CHECK_ADD -->|Yes| FILE_FOR_ADD
    CHECK_ADD -->|No| CHECK_CLOSE
    
    FILE_FOR_ADD -->|Yes| SUCCESS_WRITE
    FILE_FOR_ADD -->|No| ERROR_STATE
    
    CHECK_CLOSE -->|Yes| FILE_FOR_CLOSE
    FILE_FOR_CLOSE -->|Yes| SUCCESS_CLOSE
    FILE_FOR_CLOSE -->|No| ERROR_STATE
    
    ERROR_FLAG --> RETURN
    ERROR_STATE --> RETURN
    ERROR_DOUBLE --> RETURN
    SUCCESS_INIT --> RETURN
    SUCCESS_WRITE --> RETURN
    SUCCESS_CLOSE --> RETURN
    
    style ERROR_FLAG fill:#ffebee
    style ERROR_STATE fill:#ffebee
    style ERROR_DOUBLE fill:#ffebee
    style SUCCESS_INIT fill:#e8f5e9
    style SUCCESS_WRITE fill:#e8f5e9
    style SUCCESS_CLOSE fill:#e8f5e9
    
    Note1[Note: Current implementation<br/>does NOT check file state<br/>State errors shown are<br/>recommended validations]
    
    style Note1 fill:#fff9c4
```

---

## Record Structure

```mermaid
graph LR
    REC[LOG-RECORD<br/>150 bytes]
    
    subgraph "Field 1: LOG-RECORD-ID"
        ID[PIC 9 10<br/>10 bytes<br/>Sequential number<br/>Example: 0000000042]
    end
    
    subgraph "Field 2: LOG-RECORD-FUNCTION-NAME"
        FUNC[PIC X 40<br/>40 bytes<br/>Function/program name<br/>Example: VALIDATE-INPUT + 26 spaces]
    end
    
    subgraph "Field 3: LOG-RECORD-MESSAGE"
        MSG[PIC X 100<br/>100 bytes<br/>Log message text<br/>Example: Validation passed + 81 spaces]
    end
    
    REC --> ID
    REC --> FUNC
    REC --> MSG
    
    style REC fill:#e1f5ff
    style ID fill:#e8f5e9
    style FUNC fill:#fff4e6
    style MSG fill:#fffde7
```

### Record Layout Example

```
Byte Position:  1         11                                51                                                    151
                |---------|----------------------------------|---------------------------------------------------|
Field:          ID (10)   FUNCTION-NAME (40)                 MESSAGE (100)
                |---------|----------------------------------|---------------------------------------------------|
Example:        0000000001LOG-INIT-PROCEDURE                 Starting Program!                                        
                0000000002PROCESS-RECORDS                    Processed 1000 records successfully                      
                0000000003LOGGER:LOG-CLOSE-PROCEDURE        Closed logging file                                      
```

---

## LOG-RECORD-ID Mutation Timeline

```mermaid
graph TD
    START([Program Start])
    UNDEF[LOG-RECORD-ID: Undefined]
    
    OPEN_OP[OPEN Operation<br/>LOG-INIT-PROCEDURE]
    ID1[LOG-RECORD-ID = 1<br/>MOVE 1 TO LOG-RECORD-ID]
    WRITE1[WRITE: ID=1<br/>LOG-INIT-PROCEDURE<br/>Starting Program!]
    
    ADD1_OP[ADD Operation #1<br/>LOG-WRITE-TO-PROCEDURE]
    ID2[LOG-RECORD-ID = 2<br/>ADD 1 TO LOG-RECORD-ID]
    WRITE2[WRITE: ID=2<br/>User function<br/>User message]
    
    ADD2_OP[ADD Operation #2<br/>LOG-WRITE-TO-PROCEDURE]
    ID3[LOG-RECORD-ID = 3<br/>ADD 1 TO LOG-RECORD-ID]
    WRITE3[WRITE: ID=3<br/>User function<br/>User message]
    
    CLOSE_OP[CLOSE Operation<br/>LOG-CLOSE-PROCEDURE]
    ID4[LOG-RECORD-ID = 4<br/>ADD 1 TO LOG-RECORD-ID]
    WRITE4[WRITE: ID=4<br/>LOGGER:LOG-CLOSE-PROCEDURE<br/>Closed logging file]
    
    END([Program End<br/>Final ID = 4])
    
    START --> UNDEF
    UNDEF --> OPEN_OP
    OPEN_OP --> ID1
    ID1 --> WRITE1
    
    WRITE1 --> ADD1_OP
    ADD1_OP --> ID2
    ID2 --> WRITE2
    
    WRITE2 --> ADD2_OP
    ADD2_OP --> ID3
    ID3 --> WRITE3
    
    WRITE3 --> CLOSE_OP
    CLOSE_OP --> ID4
    ID4 --> WRITE4
    
    WRITE4 --> END
    
    style START fill:#e1f5ff
    style UNDEF fill:#ffebee
    style ID1 fill:#e8f5e9
    style ID2 fill:#e8f5e9
    style ID3 fill:#e8f5e9
    style ID4 fill:#e8f5e9
    style END fill:#e1f5ff
```

---

## File Operations Timeline

```mermaid
gantt
    title LOGGER File Operations Timeline
    dateFormat X
    axisFormat %s

    section File State
    Closed           :closed1, 0, 1s
    Open (Session)   :active, open1, 1s, 10s
    Closed           :closed2, 11s, 12s

    section Operations
    OPEN             :milestone, m1, 1s, 0s
    WRITE (Init)     :crit, w1, 1s, 1s
    WRITE (ADD #1)   :w2, 3s, 1s
    WRITE (ADD #2)   :w3, 5s, 1s
    WRITE (ADD #3)   :w4, 7s, 1s
    WRITE (Close)    :crit, w5, 10s, 1s
    CLOSE            :milestone, m2, 11s, 0s
    Buffer Flush     :crit, flush, 11s, 1s

    section Record ID
    ID = 1           :id1, 1s, 3s
    ID = 2           :id2, 3s, 2s
    ID = 3           :id3, 5s, 2s
    ID = 4           :id4, 7s, 3s
    ID = 5           :id5, 10s, 2s
```

---

## Usage Notes

### Rendering These Diagrams

These Mermaid diagrams can be rendered in:

1. **GitHub**: Native Mermaid support in markdown files
2. **VS Code**: Install "Markdown Preview Mermaid Support" extension
3. **Online**: [mermaid.live](https://mermaid.live/) - paste code to visualize
4. **Documentation Tools**: Sphinx, MkDocs, etc. with Mermaid plugins

### Customization

To modify these diagrams:
1. Copy the mermaid code block
2. Paste into [mermaid.live](https://mermaid.live/)
3. Edit and visualize changes
4. Copy back updated code

---

## Related Documentation

- **[logger_COMPREHENSIVE_DOC.md](logger_COMPREHENSIVE_DOC.md)** - Complete program documentation
- **[logger_CALL_GRAPH.md](logger_CALL_GRAPH.md)** - Text-based call graph with detailed analysis
- **[logger_DATA_DICTIONARY.md](logger_DATA_DICTIONARY.md)** - Data structure details
- **[logger_ERROR_HANDLING.md](logger_ERROR_HANDLING.md)** - Error scenarios and handling

---

## Expert Review Status

**⚠️ AI-GENERATED DOCUMENTATION**: These diagrams were generated by AI and should be reviewed by COBOL experts for accuracy.

**Review Checklist**:
- [ ] Verify diagrams accurately represent program flow
- [ ] Confirm state transitions are correct
- [ ] Validate sequence diagrams match actual execution
- [ ] Review data flow for completeness
- [ ] Verify error handling paths

**Expert Notes**: _[To be filled during review]_
