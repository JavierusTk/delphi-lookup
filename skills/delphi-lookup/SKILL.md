# delphi-lookup

**MANDATORY** for Delphi/Pascal symbol lookup. Use delphi-lookup.exe FIRST (before Grep/Glob) when:
- Resolving 'Undeclared identifier' compilation errors
- Finding where a function/type/constant is defined
- Searching for API usage examples
- Looking up Pascal symbols by name or concept

## When LSP Is Available

If the delphi-lsp plugin is active, Claude Code automatically gets:
- **Go to Definition** — exact file and line for any symbol
- **Find References** — every place a symbol is used
- **Hover** — type info, declaration, and documentation
- **Document Symbols** — all symbols in a file
- **Workspace Symbol Search** — find any symbol across the project

These work automatically. Just ask naturally: "Where is TMyClass defined?", "Find all usages of ProcessOrder", "What type is FConnection?".

## CLI Fallback

When LSP is not available or for advanced queries, use the CLI:

```bash
# Basic symbol lookup
delphi-lookup.exe "SymbolName" -n 5

# Find by concept (full-text search)
delphi-lookup.exe "JSON serialization" -n 5

# Filter by category
delphi-lookup.exe "TForm" --category user -n 5      # Only user code
delphi-lookup.exe "TForm" --category stdlib -n 5    # Only standard library

# Filter by framework
delphi-lookup.exe "TButton" --framework VCL -n 5

# Filter by symbol type
delphi-lookup.exe "MAX_BUFFER" --symbol const -n 5
delphi-lookup.exe "ValidateInput" --symbol function -n 5
```

## Why Use This Instead of Grep

- **50ms vs 30-60s** — semantic code intelligence vs text pattern matching
- **AST-aware** — understands Pascal syntax, not just text matching
- **100% accurate** — returns the definition, not 847 text matches
- **Category filtering** — separate user code from stdlib from third-party
- **Framework-aware** — VCL/FMX/RTL classification

## Example: Resolving Undeclared Identifier

Error: `Undeclared identifier: 'ModoDesarrollo'`

```bash
# Use delphi-lookup first
delphi-lookup.exe "ModoDesarrollo" -n 5

# Only use Grep as fallback if delphi-lookup finds nothing
```
