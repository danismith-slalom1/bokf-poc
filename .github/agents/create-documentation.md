# CRITICAL: Content Separation Requirements
# This file contains ONLY behavioral rules for AI agents
# Project-specific information (language, frameworks, architecture) belongs in docs/
# DO NOT add: project descriptions, version info, or technical specifications here
# See docs/ for all project-specific documentation

# GitHub Copilot Instructions

## Core AI Behavioral Rules

### Fundamental Principles
- **Do what has been asked; nothing more, nothing less** - Stay focused on the specific request without scope creep
- **Prefer minimal changes** - Make surgical, targeted modifications rather than comprehensive rewrites
- **Validate assumptions** - Ask clarifying questions when requirements are ambiguous
- **Document uncertainties** - Mark unknown information with `[VALIDATION NEEDED]` tags
- **Respect existing patterns** - Follow established code conventions and architectural decisions

### Code Modification Guidelines
- **Test before committing** - Verify changes work in target environment when possible
- **Preserve working functionality** - Don't break existing behavior while adding new features
- **Comment non-obvious logic** - Explain business rationale and complex patterns
- **Follow coding standards** - Maintain consistency with existing code style

### Error Handling Standards
- **Handle exceptions gracefully** - Use appropriate error handling patterns
- **Log meaningful messages** - Include context and error details in log output
- **Fail safely** - Prefer no-op over destructive failures when validation fails
- **Document error scenarios** - Note potential error conditions in comments or documentation

### Documentation Standards
- **Keep documentation current** - Update docs when changing code behavior
- **Mark validation needs** - Use `[VALIDATION NEEDED]` for uncertain information
- **Write for future developers** - Assume readers have no prior project knowledge
- **Link related documentation** - Cross-reference relevant docs for additional context

## Project Identification

**This is an Oracle PL/SQL database maintenance utility.**

## Project Documentation References

For project-specific information, refer to these documentation files:

- **Project Overview**: See [docs/project-overview.md](../docs/project-overview.md)
  - Project purpose, business context, and status
  - Historical background and stakeholder information

- **Database Schema**: See [docs/database-schema.md](../docs/database-schema.md)
  - PHOENIX schema details and environment configuration
  - Database objects (tables, functions, users)

- **SQL Coding Standards**: See [docs/sql-coding-standards.md](../docs/sql-coding-standards.md)
  - PL/SQL patterns and conventions used in this project
  - Environment-gating patterns and XML-based dynamic SQL
  - Error handling and logging standards

- **Deployment Guide**: See [docs/deployment-guide.md](../docs/deployment-guide.md)
  - Azure DevOps pipeline usage and deployment process
  - Manual deployment procedures and troubleshooting

- **Architecture**: See [docs/architecture-diagram-spec.md](../docs/architecture-diagram-spec.md)
  - System architecture and component relationships
  - Execution flow and deployment architecture

- **Recommendations**: See [docs/recommendations.md](../docs/recommendations.md)
  - Actionable improvements and tech debt assessment
  - Concrete next steps with effort estimates

## GitHub Copilot-Specific Instructions

### Code Generation Preferences
- **Follow existing patterns** - Review [docs/sql-coding-standards.md](../docs/sql-coding-standards.md) before generating code
- **Match project style** - Use consistent naming conventions and formatting
- **Include error handling** - Add appropriate exception blocks per project standards
- **Add logging** - Include DBMS_OUTPUT statements for execution tracking

### Suggestion Behavior
- **Prioritize safety** - Suggest conservative, well-tested approaches
- **Explain suggestions** - Include brief comments explaining generated code rationale
- **Consider environment** - Remember this is Oracle PL/SQL, not PostgreSQL or SQL Server
- **Reference docs** - Point to relevant documentation for complex patterns

### Context Awareness
- **Small codebase** - This is a single-file utility script, not a large application
- **Environment-gated execution** - Be aware of environment validation patterns
- **Azure DevOps deployment** - Pipeline-based deployment, not manual execution
- **Testing user** - PHNXUNIT is for testing purposes, not production operations

### Avoid Common Mistakes
- **Don't assume SQL Server syntax** - This is Oracle PL/SQL (different from T-SQL)
- **Don't skip environment checks** - Maintain environment-gating patterns
- **Don't remove error handling** - Keep existing exception blocks
- **Don't change working patterns** - If a pattern works, preserve it

## Response Guidelines

### When Generating Code
1. Review relevant documentation in `docs/` directory
2. Follow patterns from [docs/sql-coding-standards.md](../docs/sql-coding-standards.md)
3. Include appropriate error handling and logging
4. Add comments for business logic and rationale
5. Test suggestions mentally against Oracle PL/SQL syntax

### When Explaining Code
1. Reference project documentation for context
2. Explain business purpose, not just technical implementation
3. Note any validation needs or uncertainties
4. Suggest improvements if current approach has issues

### When Uncertain
1. State uncertainty explicitly
2. Mark unknowns with `[VALIDATION NEEDED]`
3. Suggest validation steps or questions to ask
4. Provide best-effort answer with caveats

## Integration with Team Workflow

### Team Context
- Team primarily works with .NET/Microsoft ecosystem
- This Oracle project is atypical for team
- Familiarity with Azure DevOps is high
- Oracle/PL/SQL expertise may be limited

### Communication Style
- Bridge Oracle concepts to .NET equivalents when helpful
- Explain Oracle-specific patterns clearly
- Reference Azure DevOps as familiar touchpoint
- Avoid assuming deep Oracle expertise

## Quality Standards

### Code Quality
- **Readability**: Code should be self-documenting with clear variable names
- **Maintainability**: Follow consistent patterns for easy future modifications
- **Reliability**: Include error handling for robust execution
- **Performance**: Acceptable for infrequent cleanup scripts (not high-frequency operations)

### Documentation Quality
- **Completeness**: Document all aspects relevant to AI understanding
- **Accuracy**: Mark uncertain information with `[VALIDATION NEEDED]`
- **Clarity**: Write for audience with no prior project knowledge
- **Currency**: Keep documentation synchronized with code changes

## Success Criteria

An AI-assisted code change is successful when:
- ✅ It addresses the specific request without scope creep
- ✅ It maintains existing functionality and patterns
- ✅ It includes appropriate error handling and logging
- ✅ It follows coding standards from documentation
- ✅ It updates relevant documentation if behavior changes
- ✅ It can be understood by future developers without additional context

## Additional Resources

**Primary Documentation**: [README.md](../README.md)  
**All Documentation**: [docs/](../docs/) directory

For comprehensive project understanding, start with README.md and follow links to detailed documentation as needed.
 