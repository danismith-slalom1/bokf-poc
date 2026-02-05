# Procedure Documentation Checklist

## ⚠️ CRITICAL REQUIREMENT

**EVERY PROCEDURE/ROUTINE MUST HAVE ITS OWN SEPARATE MARKDOWN FILE**

This is **NON-NEGOTIABLE** and applies to **ALL programs** regardless of size or complexity.

## What Counts as a Procedure?

Document each of the following as a separate file:

- ✅ `ROUTINE 'PROCEDURE-NAME'` declarations (OmniScript/COBOL)
- ✅ `PERFORM PROCEDURE-NAME` blocks
- ✅ Gosub-style procedures
- ✅ Named subroutines
- ✅ Main program logic (if not in a named routine, create `MAIN_PROGRAM_DOC.md`)

## File Naming Convention

**Format**: `[PROCEDURE_NAME]_DOC.md`

**Examples**:
- `CALCULATE_TAX_DOC.md`
- `LOAD_CAVANAL_SECURITY_TABLE_DOC.md`
- `CHECK_SSSA_DOC.md`
- `MAIN_PROGRAM_DOC.md`

**Rules**:
- Use UPPERCASE for procedure name
- Replace dots, hyphens, and spaces with underscores
- Always append `_DOC.md` suffix
- One file per procedure - never combine

## Directory Structure

```
{PROGRAM-NAME}/
├── {PROGRAM}_OVERVIEW.md
├── {PROGRAM}_DATA_DICTIONARY.md
├── {PROGRAM}_CALL_GRAPH.md
├── {PROGRAM}_DIAGRAMS.md
├── {PROGRAM}_ERROR_HANDLING.md
├── {PROGRAM}_INTEGRATION_GUIDE.md
├── {PROGRAM}_BUSINESS_RULES.md
├── {PROGRAM}_CROSS_REFERENCE.md
├── {PROGRAM}_VALIDATION_REPORT.md
└── procedures/                    ← MANDATORY SUBDIRECTORY
    ├── PROCEDURE_1_DOC.md
    ├── PROCEDURE_2_DOC.md
    ├── PROCEDURE_3_DOC.md
    └── MAIN_PROGRAM_DOC.md
```

## Pre-Commit Validation

Before committing documentation, **ALWAYS** verify:

### 1. Count Procedures in Source Code
```bash
# For OmniScript/COBOL with ROUTINE keyword
grep -c "^ROUTINE " source.cbl

# For PERFORM blocks
grep -c "PERFORM " source.cbl | grep -v "^[ ]*\*"
```

### 2. Count Procedure Documentation Files
```bash
find procedures/ -name "*_DOC.md" | wc -l
```

### 3. Verify Counts Match
```bash
PROCEDURE_COUNT=4  # From step 1
DOC_COUNT=4        # From step 2

if [ "$PROCEDURE_COUNT" -eq "$DOC_COUNT" ]; then
  echo "✓ Complete: ${DOC_COUNT}/${PROCEDURE_COUNT} procedures documented"
else
  echo "✗ INCOMPLETE: Only ${DOC_COUNT}/${PROCEDURE_COUNT} procedures documented"
  echo "  Missing: $((PROCEDURE_COUNT - DOC_COUNT)) procedure files"
  exit 1
fi
```

## Common Mistakes to Avoid

### ❌ WRONG: Documenting procedures only in comprehensive docs
```
# DON'T DO THIS
{PROGRAM}_COMPREHENSIVE_DOC.md
  - Contains inline procedure documentation
  - No separate procedure files
```

### ❌ WRONG: Combining multiple procedures in one file
```
# DON'T DO THIS
procedures/
  └── ALL_PROCEDURES.md  ← Contains multiple procedures
```

### ❌ WRONG: Missing procedures/ directory
```
# DON'T DO THIS
{PROGRAM-NAME}/
├── {PROGRAM}_OVERVIEW.md
├── {PROGRAM}_DATA_DICTIONARY.md
└── (no procedures directory)
```

### ❌ WRONG: Documenting only "key" procedures
```
# DON'T DO THIS
procedures/
├── MAIN_PROCEDURE_DOC.md     ← Only 1 of 5 procedures
└── (missing 4 other procedures)
```

### ✅ CORRECT: Every procedure has its own file
```
# DO THIS
procedures/
├── OPEN_TRADE_CSV_FILE_DOC.md
├── LOAD_CAVANAL_SECURITY_TABLE_DOC.md
├── REVIEW_TRADE_ORDERS_DOC.md
├── CLOSE_CLEANUP_DOC.md
└── MAIN_PROGRAM_DOC.md
```

## Required Content in Each Procedure File

Each procedure documentation file **MUST** include:

### 1. Header Section
- Procedure name
- Parent program name
- Purpose (one-sentence summary)

### 2. Overview Section
- Business context
- What problem it solves
- When it's called

### 3. Execution Flow
- Mermaid flowchart diagram
- Step-by-step processing logic

### 4. Source Code Section
- Complete procedure source code
- Properly formatted in code blocks

### 5. Input/Output Section
- Input parameters (if any)
- Output/side effects
- Variables modified
- Files accessed

### 6. Processing Logic
- Detailed breakdown of each step
- Business rules applied
- Data transformations
- Calculations performed

### 7. Dependencies Section
- Prerequisites (what must run first)
- Used by (what calls this procedure)
- External dependencies

### 8. Error Handling
- Error conditions
- Error handling mechanisms
- Recovery procedures
- Known issues

### 9. Testing Recommendations
- Unit test scenarios
- Test data examples
- Expected results
- Edge cases

## Validation Checklist

Before pushing to GitLab, verify:

- [ ] `procedures/` subdirectory exists
- [ ] Number of procedures in source code counted
- [ ] Number of `*_DOC.md` files matches procedure count
- [ ] Each procedure file follows naming convention
- [ ] Each procedure file has all required sections
- [ ] No procedures documented only inline
- [ ] No combined procedure files
- [ ] All procedure files referenced in call graph
- [ ] All procedure files linked from overview

## Quick Reference Commands

### Count procedures in OmniScript/COBOL source:
```bash
grep -c "^ROUTINE " program.cbl
```

### Count procedure documentation files:
```bash
ls -1 procedures/*_DOC.md | wc -l
```

### List missing procedures:
```bash
# Get procedure names from source
grep "^ROUTINE " program.cbl | sed "s/ROUTINE '\(.*\)';/\1/" > /tmp/source_procs.txt

# Get documented procedure names
ls procedures/*_DOC.md | sed 's/procedures\/\(.*\)_DOC.md/\1/' > /tmp/doc_procs.txt

# Show differences
diff /tmp/source_procs.txt /tmp/doc_procs.txt
```

## Examples from Real Documentation

### Example 1: apples.cbl (4 procedures)
```
apples/
└── procedures/
    ├── OPEN_TRADE_CSV_FILE_DOC.md          ← Procedure 1
    ├── LOAD_CAVANAL_SECURITY_TABLE_DOC.md  ← Procedure 2
    ├── REVIEW_TRADE_ORDERS_DOC.md          ← Procedure 3
    └── CLOSE_CLEANUP_DOC.md                ← Procedure 4
```

**Source code**: 4 ROUTINE declarations  
**Documentation files**: 4 procedure files ✅

### Example 2: grapes.cbl (1 procedure)
```
grapes/
└── procedures/
    └── CHECK_SSSA_DOC.md                   ← Procedure 1
```

**Source code**: 1 ROUTINE declaration  
**Documentation files**: 1 procedure file ✅

## Need Help?

If you're unsure whether something needs separate procedure documentation:

**Rule of Thumb**: If it has a name in the source code (ROUTINE, PERFORM, procedure label), it needs a separate file.

**When in doubt**: Create the file. It's better to over-document than under-document.

---

**Last Updated**: February 5, 2026  
**Version**: 1.0
