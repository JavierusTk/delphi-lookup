# delphi-lookup - Fast Pascal Code Search for AI Agents

[![License: MIT + Commons Clause](https://img.shields.io/badge/License-MIT%20%2B%20Commons%20Clause-yellow.svg)](LICENSE)
[![Platform: Windows x64](https://img.shields.io/badge/Platform-Windows%20x64-blue.svg)]()
[![Delphi: 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)]()

High-performance source code search system for Pascal codebases, optimized for AI coding agents like Claude Code.

## Features

⚡ **Ultra-Fast** - 1-3ms cached searches (100-2000x faster than uncached)
🔍 **FTS5 Search** - Exact + Fuzzy + Full-Text search
📊 **Smart Caching** - Automatic query logging and result caching
🎯 **Category Filtering** - Separate user code, stdlib, third-party
📈 **Scalable** - Handles 300K+ symbols efficiently

## Quick Start

### Installation

1. Download the latest release or build from source
2. Ensure `sqlite3.dll` is in the `bin/` folder (included)

### Basic Usage

```bash
# 1. Build index from your Pascal source
delphi-indexer.exe "C:\Projects\MyDelphiApp\src" --category user

# 2. Search code
delphi-lookup.exe "TStringList" -n 5

# 3. List indexed folders
delphi-indexer.exe --list-folders
```

## Documentation

**For using the tools:**
- **[USER-GUIDE.md](USER-GUIDE.md)** - Complete usage guide, all options, examples

**For maintaining/extending:**
- **[TECHNICAL-GUIDE.md](TECHNICAL-GUIDE.md)** - Architecture, troubleshooting, extending
- **[DATABASE-SCHEMA.md](DATABASE-SCHEMA.md)** - Database schema reference
- **[QUERY-ANALYTICS.md](QUERY-ANALYTICS.md)** - SQL queries for usage analysis
- **[TESTS.md](TESTS.md)** - Testing guide and test suite

## Architecture

```
Pascal Source Files → delphi-indexer → SQLite Database (FTS5)
                                                 ↓
                                          delphi-lookup → Results
```

**Components:**
- **delphi-indexer** - Indexes Pascal code with AST parsing
- **delphi-lookup** - Fast FTS5 search with automatic caching
- **Database** - SQLite with FTS5 full-text search

## System Requirements

**Platform**: Windows x64

**Runtime Dependencies:**
- `sqlite3.dll` (FTS5-enabled, included)

> **Note:** The codebase includes experimental vector embedding support (via Ollama), but this is not recommended for AI agent workflows. FTS5-only search is the default and recommended mode. See CLAUDE.md for details.

**Compilation Dependencies** (Delphi):
- mORMot 2
- FireDAC (included with RAD Studio)
- DelphiAST (included)

## Performance

| Operation | Speed | Details |
|-----------|-------|---------|
| **Cached search** | 1-3ms | Repeated queries |
| **Simple search** | 200-400ms | First time |
| **Full-text search** | 1,500-2,000ms | Complex queries |
| **Indexing** | ~43 chunks/sec | ~1,000 symbols in 25-30s |

**Scalability**: Tested with 344K+ symbols, full Delphi RTL/VCL indexed

## Examples

```bash
# Basic usage
delphi-lookup.exe "TStringList" -n 5

# Search user code only
delphi-lookup.exe "TCustomForm" --category user

# Boost user code in results
delphi-lookup.exe "TForm" --prefer user -n 10

# Full-text search
delphi-lookup.exe "JSON serialization" -n 5

# Find constants
delphi-lookup.exe "MAX_BUFFER" --symbol const

# List indexed folders
delphi-indexer.exe --list-folders
```

## Output Format

Results formatted for AI agent consumption:

```
// Context for query: "TStringList"
// Found 3 result(s)

// Result 1 (Exact Match - STDLIB): TStringList (type)
// File: System.Classes.pas
// Type: code | Category: stdlib
// Relevance: 1.00

TStringList = class(TStrings)
  private
    FList: TStringItemList;
    ...
end;
```

## Configuration

delphi-lookup can be configured via JSON file, environment variables, or command-line parameters.

### Configuration File (recommended)

Copy `delphi-lookup.example.json` to `delphi-lookup.json` and customize:

```json
{
  "database": "delphi_symbols.db",
  "category": "user",
  "num_results": 5
}
```

**Key settings:**
- `database`: SQLite database file path
- `category`: Default source category for indexing
- `num_results`: Default number of search results
- Command-line parameters override config file settings

See CLAUDE.md for full configuration reference.

## Claude Code Integration

delphi-lookup includes a skill for [Claude Code](https://claude.ai/code) that teaches it to use delphi-lookup automatically for Pascal symbol searches.

**Quick setup** - paste this to Claude Code:

```
Install the delphi-lookup skill: copy claude-code/skill.md to ~/.claude/skills/delphi-lookup/
and add the memory instructions from claude-code/SETUP.md to my CLAUDE.md
```

See **[claude-code/SETUP.md](claude-code/SETUP.md)** for detailed instructions.

## Contributing

Contributions are welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## Security

For security considerations and vulnerability reporting, see [SECURITY.md](SECURITY.md).

## Support

**For AI Coding Agents:**
- See [USER-GUIDE.md](USER-GUIDE.md) for complete usage instructions
- See [TECHNICAL-GUIDE.md](TECHNICAL-GUIDE.md) for troubleshooting

**For developers:**
- Check [TESTS.md](TESTS.md) for test suite
- See [DATABASE-SCHEMA.md](DATABASE-SCHEMA.md) for schema details

## License

This project is licensed under the MIT License - see [LICENSE](LICENSE) for details.

Third-party licenses are documented in [THIRD-PARTY-LICENSES.md](THIRD-PARTY-LICENSES.md).

---

**Version**: 1.0
**Target**: AI Coding Agents (Claude Code, Cursor, etc.)
**Platform**: Windows x64
