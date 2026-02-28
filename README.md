# delphi-lookup - Fast Pascal Code Search for AI Agents

[![License: MIT + Commons Clause](https://img.shields.io/badge/License-MIT%20%2B%20Commons%20Clause-yellow.svg)](LICENSE)
[![Platform: Windows x64](https://img.shields.io/badge/Platform-Windows%20x64-blue.svg)]()
[![Delphi: 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)]()

High-performance source code search system for Pascal codebases, optimized for AI coding agents like Claude Code.

## Features

- **Fast** - ~75ms end-to-end for identifier lookups (short-circuit on exact match), ~75ms cached
- **Smart Search** - 3-phase cascade: exact NOCASE → prefix → substring, then FTS5 + fuzzy
- **Declaration Priority** - Declarations rank above implementations with `[Decl]` badges
- **Smart Caching** - Content-hash based cache that survives reindexing
- **Incremental Indexing** - Merkle-style change detection; no-change verification in <10ms
- **Parallel Processing** - Multi-threaded AST parsing and folder scanning
- **Category Filtering** - Separate user code, stdlib, third-party
- **Framework Detection** - Multi-tier VCL/FMX/RTL classification (packages, uses clause, path)
- **Scalable** - Tested with 672K+ symbols (3.2GB database)

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
- **[CLAUDE.md](CLAUDE.md)** - AI agent reference (configuration, architecture, code patterns)

**For maintaining/extending:**
- **[TECHNICAL-GUIDE.md](TECHNICAL-GUIDE.md)** - Architecture, troubleshooting, extending
- **[DATABASE-SCHEMA.md](DATABASE-SCHEMA.md)** - Database schema reference
- **[QUERY-ANALYTICS.md](QUERY-ANALYTICS.md)** - SQL queries for usage analysis
- **[TESTS.md](TESTS.md)** - Testing guide and test suite
- **[CHANGELOG.md](CHANGELOG.md)** - Version history

## Architecture

```
Pascal Source Files → delphi-indexer → SQLite Database (FTS5)
                                                 ↓
                                          delphi-lookup → Results
```

**Components:**
- **delphi-indexer** - Indexes Pascal code with AST parsing (parallel, incremental)
- **delphi-lookup** - Fast identifier lookup with short-circuit + hybrid search (fuzzy + FTS5) with caching
- **Database** - SQLite with WAL mode, FTS5 full-text search, optional vector embeddings

## System Requirements

**Platform**: Windows x64

**Runtime Dependencies:**
- `sqlite3.dll` (FTS5-enabled, included)
- `vec0.dll` (sqlite-vec extension, included)

> **Note:** Vector embedding support (via Ollama) is included but not recommended for AI agent workflows. FTS5-only is the default and recommended mode — agents iterate fast and achieve similar quality with multiple searches while being 17x faster. See CLAUDE.md for details.

**Compilation Dependencies** (Delphi):
- mORMot 2
- FireDAC (included with RAD Studio)
- DelphiAST (included)

## Performance

End-to-end wall-clock time (exe startup + DB connection + search + formatting):

| Operation | End-to-end | Internal search | Details |
|-----------|-----------|----------------|---------|
| **Identifier lookup (cold)** | ~75ms | ~12ms | Exact name match with short-circuit |
| **Identifier lookup (cached)** | ~75ms | ~10ms | From query_cache |
| **Full search (cold)** | ~1.0-1.7s | ~950-1700ms | FTS5 + fuzzy (no short-circuit) |
| **No-change reindex** | ~10ms | — | Merkle-style detection |
| **Indexing** | ~43 chunks/sec | — | Parallel AST parsing |

> Exe startup overhead is ~65ms (process creation + DB connection + FTS5 detection). 83% of production queries are single-word Pascal identifiers that hit the short-circuit path.

**Scalability**: Tested with 672K+ symbols, 3.2GB database

## Examples

```bash
# Basic usage
delphi-lookup.exe "TStringList" -n 5

# Search user code only
delphi-lookup.exe "TCustomForm" --category user

# Boost user code in results
delphi-lookup.exe "TForm" --prefer user -n 10

# Filter by framework
delphi-lookup.exe "TButton" --framework VCL -n 5

# Full-text search
delphi-lookup.exe "JSON serialization" -n 5

# Find constants
delphi-lookup.exe "MAX_BUFFER" --symbol const

# Indexing
delphi-indexer.exe "C:\Projects\MyLib" --category user
delphi-indexer.exe "C:\Projects\MyLib" --force        # Full reindex
delphi-indexer.exe --list-folders                      # Show indexed folders
```

## Output Format

Compact format optimized for AI agent context windows (2-3 lines per result):

```
// Context for query: "CrearQuery"

Found 5 result(s) for "CrearQuery":

1. [Decl] function CrearQuery(const pSQL:string='';const pPrepare:boolean=false):TQueryMAX; overload;
   → TableMax.Query.pas [unit: TableMax.Query] (user, RTL)

2. [Decl] function CrearQuery(const pQuery:string): TDataset; virtual; abstract;
   → gmcSincronizarSQL.Tipos.Origen.pas [unit: gmcSincronizarSQL.Tipos.Origen] (user, RTL)

// Search completed in 12 ms
```

Declarations are tagged with `[Decl]`. Use `--full` for verbose output with code snippets.

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

## AI Tool Integration

### Claude Code (LSP Plugin)

delphi-lookup includes an **LSP plugin** for [Claude Code](https://claude.ai/code) that provides real-time code intelligence — go-to-definition, find references, hover info, and symbol search. ~50ms per query vs 30-60s with grep.

**Quick setup (2 minutes):**

1. Add `"ENABLE_LSP_TOOL": "1"` to `~/.claude/settings.json`
2. Compile `delphi-lsp-server.dproj` and add the exe to PATH
3. Load plugin: `claude --plugin-dir /path/to/delphi-lookup`
4. Restart Claude Code

Then just ask naturally: *"Where is TMyClass defined?"* — Claude uses LSP automatically.

See **[claude-code/SETUP.md](claude-code/SETUP.md)** for detailed instructions and **[LSP-GUIDE.md](LSP-GUIDE.md)** for technical documentation.

### Gemini CLI

Setup guide for [Gemini CLI](https://github.com/google-gemini/gemini-cli) users with a one-shot configuration prompt.

See **[gemini/GEMINI_SETUP.md](gemini/GEMINI_SETUP.md)** for instructions.

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

**Version**: 1.4.0 (2026-02-26)
**Target**: AI Coding Agents (Claude Code, Cursor, etc.)
**Platform**: Windows x64
