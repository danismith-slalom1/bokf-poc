Please update the documentation for a COBOL program that has already been documented.

**CRITICAL**: You will be restricted to working within the folder and its subfolders passed to copilot, so ensure your tool requests use absolute paths or else they will fail (example of something that will fail: if you search all paths for specific file extensions).

The COBOL program in question is: <COBOL_PROGRAM_PATH>
The existing documentation should be present in the COBOL documentation directory (typically structured as program-name/). Look for:
- Program analysis
- Data dictionary
- Paragraph documentation
- Call graphs
- Variable mutation analysis
- Comprehensive documentation

Step 1. Identify what has changed in the COBOL program:
   - Compare current source to what was documented
   - Note modified paragraphs, new variables, changed PERFORM relationships
   - Re-generate static analysis reports if available

Step 2. Follow the COBOL documentation maintenance process from cobol-documenter/WORKFLOW.md Phase 5:
   - Update affected documentation sections
   - Use AI to regenerate documentation for changed sections
   - Have COBOL expert review updates
   - Update cross-references and master index

**CRITICAL** Based on prior experience, these key observations are being repeated to you to reinforce their importance:
- "Only update sections affected by code changes"
- "Maintain consistency with existing documentation standards"
- "Have expert review all updates before finalizing"
- "Update the 'Last Modified' dates in affected documents"