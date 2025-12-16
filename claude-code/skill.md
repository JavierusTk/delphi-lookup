# delphi-lookup

**MANDATORY** for Delphi/Pascal symbol lookup. Use delphi-lookup.exe FIRST (before Grep/Glob) when:
- Resolving 'Undeclared identifier' compilation errors
- Finding where a function/type/constant is defined
- Searching for API usage examples
- Looking up Pascal symbols by name or concept

## Usage

```bash
# Basic symbol lookup
delphi-lookup.exe "SymbolName" -n 5

# Find by concept (full-text search)
delphi-lookup.exe "JSON serialization" -n 5

# Filter by category
delphi-lookup.exe "TForm" --category user -n 5      # Only user code
delphi-lookup.exe "TForm" --category stdlib -n 5    # Only standard library

# Filter by symbol type
delphi-lookup.exe "MAX_BUFFER" --symbol const -n 5
delphi-lookup.exe "ValidateInput" --symbol function -n 5
```

## Why Use This Instead of Grep

- **Sub-millisecond cached searches** (1-3ms for repeated queries)
- **AST-aware**: Understands Pascal syntax, not just text matching
- **Category filtering**: Separate user code from stdlib from third-party
- **Framework-aware**: VCL/FMX/RTL classification

## Example: Resolving Undeclared Identifier

Error: `Undeclared identifier: 'ModoDesarrollo'`

```bash
# Use delphi-lookup first
delphi-lookup.exe "ModoDesarrollo" -n 5

# Only use Grep as fallback if delphi-lookup finds nothing
```

## Installation

See `claude-code/SETUP.md` for installation instructions.
