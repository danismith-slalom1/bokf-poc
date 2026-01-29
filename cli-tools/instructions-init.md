Please document a COBOL program using the COBOL Documentation Module. The COBOL program to document is: <COBOL_PROGRAM_PATH>

The repository name for this COBOL program is: <REPO_NAME>

**CRITICAL**: You will be restricted to working within the folder and its subfolders passed to copilot, so ensure your tool requests use absolute paths or else they will fail (example of something that will fail: if you search all paths for specific file extensions).

**CRITICAL**: All documentation MUST be placed in `cobol-documentation/<REPO_NAME>/[PROGRAM-NAME]/` directory structure. Extract the program name from the COBOL file name (e.g., CBL0009.cobol → CBL0009).

Step 1. Locate the COBOL program at the path specified above. Verify you can access the source file.

Step 2. Check if static analysis has been performed. Look for:
   - Cross-reference reports (variable usage, modifications)
   - Call graph data (PERFORM hierarchy)
   - If these don't exist, note this and ask if they should be generated or if manual analysis is needed.

Step 3. Use the `cobol-documentation` module (in the cobol-documenter directory) to document this COBOL program. Follow the module's prompt (cobol-documenter/INITIALIZER_PROMPT.md) and WORKFLOW.md thoroughly, ensuring all output goes to `cobol-documentation/<REPO_NAME>/[PROGRAM-NAME]/`.

**CRITICAL** Based on prior experience, these key observations are being repeated to you to reinforce their importance:
- "Follow the COBOL documentation WORKFLOW.md completely through all 5 phases"
- "Generate data dictionary FIRST, then paragraphs, then call graphs, then mutations, then synthesis"
- "ALWAYS have COBOL experts review AI-generated documentation"
- "Use static analysis reports to inform documentation - don't guess"
- "Chunk large programs appropriately to avoid context window issues"
