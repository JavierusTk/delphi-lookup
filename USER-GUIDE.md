# delphi-lookup - User Guide for AI Coding Agents

## Overview

delphi-lookup is a **high-performance Pascal source code search system** for AI coding agents. It provides instant access to code definitions, examples, and documentation across large Pascal codebases.

**Key Features:**
- ⚡ **Sub-millisecond cached searches** (1-3ms for repeated queries)
- 🔍 **FTS5 search**: Exact match + fuzzy + full-text search
- 📊 **Smart caching**: Automatic query logging and result caching
- 🎯 **Category filtering**: Search user code, stdlib, or third-party separately
- 📈 **Scalable**: Handles 300K+ symbols efficiently

## Quick Start

### 1. Build the Index

```bash
# Index your codebase
delphi-indexer.exe "W:\YourProject"

# Index Delphi standard library
delphi-indexer.exe "C:\Program Files\Embarcadero\Studio\23.0\source\rtl" --category stdlib
```

### 2. Search the Code

```bash
# Basic search
delphi-lookup.exe "TStringList" -n 5

# Search only your code
delphi-lookup.exe "TCustomForm" --category user

# Boost user code in results
delphi-lookup.exe "TForm" --prefer user -n 10
```

## Tools

### delphi-indexer.exe

**Purpose**: Scans Pascal source folders and builds searchable knowledge base.

**Location**: `delphi-indexer.exe` (root folder)

**Basic Usage:**
```bash
delphi-indexer.exe <folder_path> [options]
```

**Options:**
- `--category <name>` - Classify code: `user`, `stdlib`, `third_party`, `official_help`, `project_docs`
- `--list-folders` - Show all indexed folders with statistics
- `--help` - Show complete help

**Examples:**
```bash
# Index user code (default category)
delphi-indexer.exe "C:\Projects\MyFramework"
delphi-indexer.exe "C:\Projects\MyApplication"

# Index standard library
delphi-indexer.exe "C:\Program Files\Embarcadero\Studio\23.0\source\rtl" --category stdlib

# Index third-party library
delphi-indexer.exe "W:\mORMot2\src" --category third_party

# List all indexed folders
delphi-indexer.exe --list-folders
```

**Output:**
- Progress updates during scanning and indexing
- Performance statistics (files/second, chunks/second)
- Summary of symbols indexed

**Folder Exclusions:**

The indexer automatically skips:
- Folders with `.noindex` file (create empty file to exclude)
- Version control: `.git`, `.svn`, `.hg`
- Delphi history: `__history`, `__recovery`, `backup`
- Build output: `Win32`, `Win64`, `Debug`, `Release`, `__DCU_CACHE`

**Performance:**
- ~43 chunks/second
- ~1,000 symbols in 25-30 seconds
- Incremental: Skips unchanged files

### delphi-lookup.exe

**Purpose**: Fast search across indexed Pascal code.

**Location**: `delphi-lookup.exe` (root folder)

**Basic Usage:**
```bash
delphi-lookup.exe "<query>" [options]
```

**Search Options:**
- `-n, --num-results <N>` - Number of results (default: 5, max: 100)
- `-d, --database <file>` - Database file (default: delphi_symbols.db in exe folder)

**Filter Options:**
- `--type <value>` - Content type: `code`, `help`, `markdown`, `comment`
- `--symbol <value>` - Symbol type: `class`, `interface`, `record`, `function`, `procedure`, `constructor`, `destructor`, `property`, `type`, `const`, `var`
- `--category <value>` - Source category: `user`, `stdlib`, `third_party`, `official_help`, `project_docs`
- `--prefer <value>` - Boost category in results (same values as --category)
- `--domain <tag>` - Filter by domain tag (from glossary enrichment)

**Advanced Options (Experimental - Not Recommended):**
- `--enable-semantic` - Enable vector search (experimental, not recommended for AI agents)
- `--use-reranker` - Two-stage reranking (experimental)

**Examples:**
```bash
# Basic searches
delphi-lookup.exe "TStringList"
delphi-lookup.exe "database connection" -n 10
delphi-lookup.exe "JSON serialization" -n 3

# Category filtering
delphi-lookup.exe "TForm" --category user        # Only user code
delphi-lookup.exe "TForm" --category stdlib      # Only standard library
delphi-lookup.exe "TForm" --prefer user -n 10    # Boost user code ranking

# Symbol type filtering
delphi-lookup.exe "MAX_CONNECTIONS" --symbol const
delphi-lookup.exe "GlobalLogger" --symbol var

# Domain filtering (requires glossary enrichment)
delphi-lookup.exe "time tracking" --domain TimeTracking
delphi-lookup.exe "worker operations" --domain ShopFloorControl

# Advanced searches
delphi-lookup.exe "How to send email?" --use-reranker -n 10
delphi-lookup.exe "Find validation logic" --rerank --candidates 100
```

## Search Strategy

delphi-lookup uses **5-tier hybrid search** for optimal results:

1. **Exact Match** (Relevance: 1.00)
   - Case-insensitive exact symbol name match
   - Fastest, most precise

2. **Partial Match** (Relevance: 0.80)
   - Symbol names containing the query
   - Example: "String" finds "TStringList", "TStringBuilder"

3. **Fuzzy Match** (Relevance: 0.50)
   - Parent class matching
   - Similar names with typo tolerance

4. **Full-Text Search (FTS5)** (Relevance: 1.00)
   - Searches code content and comments
   - Finds symbols by what they do, not just their name
   - Example: "JSON serialization" finds JSON-related classes

**Result Merging:**
- All search methods run in parallel
- Results deduplicated by symbol ID
- Sorted by relevance score (highest first)
- Cached for instant retrieval on repeated queries

## Performance

### Search Speed

**Cache Miss** (first query):
- Simple queries: 200-400ms
- Full-text queries: 1,500-2,000ms
- With reranking: +100-150ms

**Cache Hit** (repeated query):
- **1-3ms** (500-2000x faster)
- Automatic caching of all queries

### Indexing Speed

- **~43 chunks/second**
- **~1,000 symbols in 25-30 seconds**
- **Incremental**: Only processes changed files

### Scalability

Successfully tested with:
- **344,394 symbols** indexed
- **16 folders** including full Delphi RTL/VCL
- **621,245 code chunks** total
- Database size: ~500MB

## Output Format

Results are formatted for AI agent consumption:

```
// Context for query: "TStringList"

// Found 3 result(s) for query: "TStringList"

// Result 1 (Exact Match - STDLIB): TStringList (type)
// File: System.Classes.pas
// Type: code | Category: stdlib
// Relevance: 1.00

TStringList = class(TStrings)
  private type
    TOverriddenItem = (sloCompareStrings, sloGetObject);
  private
    FList: TStringItemList;
    FCount: Integer;
    ...
end;
--------------------
// Result 2 (Content Match - STDLIB): GetStringListFromArray (function)
// File: System.IOUtils.pas
// Type: code | Category: stdlib
// Relevance: 1.00

class function GetStringListFromArray(const AnArray: TStringDynArray): TStringList; static;
--------------------
```

Each result includes:
- **Match type**: Exact/Partial/Fuzzy/Content/Vector
- **Category**: USER CODE/STDLIB/THIRD_PARTY
- **Symbol type**: class/interface/record/function/procedure/etc.
- **File location**: Relative path to source file
- **Relevance score**: 0.00-1.00 (higher = better match)
- **Code content**: Full symbol definition

## Query Logging & Analytics

All queries are automatically logged for analysis and caching.

**View query statistics:**
```bash
# See QUERY-ANALYTICS.md for SQL queries to analyze:
# - Most frequent queries
# - Cache hit rates
# - Slowest queries
# - Filter usage patterns
# - Query trends over time
```

**Cache behavior:**
- First query: Full search + store results (`cache_valid=1`)
- Repeated queries: Load from cache (`cache_valid=0`, analytics only)
- Cache cleared when indexing changes data

## Dependencies

### Required Files

**Both executables:**
- `sqlite3.dll` (FTS5-enabled, included in `bin/`)
- `vec0.dll` (sqlite-vec extension, included in `bin/`)

### Delphi Libraries (for compilation)

- **mORMot 2** - JSON handling
- **FireDAC** - Database access (included with RAD Studio)
- **DelphiAST** - Pascal source code parsing (included)

## Configuration

delphi-lookup can be configured via JSON file, environment variables, or command-line parameters.

### Configuration File

Copy `delphi-lookup.example.json` to `delphi-lookup.json` and customize:

```json
{
  "database": "delphi_symbols.db",
  "buffer_size": 500,
  "category": "user",
  "num_results": 5
}
```

**Key settings:**
- `database`: SQLite database file path
- `num_results`: Default number of results to return
- Command-line parameters override config file settings

### FTS5-Only Mode (Recommended)

delphi-lookup uses FTS5 full-text search by default. This mode works fully offline and provides excellent results for AI coding agents that can iterate on searches.

## Database

**File**: `delphi_symbols.db` (created in executable folder)

**Key characteristics:**
- **Self-describing**: Stores model name and dimensions used for indexing
- **Incremental**: Tracks indexed folders, skips unchanged files
- **WAL mode**: Allows concurrent reads/writes
- **FTS5 enabled**: Fast full-text search

See [DATABASE-SCHEMA.md](DATABASE-SCHEMA.md) for complete schema reference.

## Troubleshooting

### "Database file not found"
- Check that `delphi_symbols.db` exists in the executable's folder
- Use `-d` option to specify custom database location
- Run delphi-indexer first to create database

### "Cannot load client library: sqlite3.dll"
- Ensure `sqlite3.dll` is in the same folder as the executable
- For delphi-indexer, also ensure `vec0.dll` is present

### "Failed to connect to Ollama"
- Verify Ollama server is running at the specified URL
- Check network connectivity
- Use `--ollama-url` to specify custom URL

### "FTS5 not available"
- Ensure you're using the included FTS5-enabled `sqlite3.dll`
- Check that the DLL is in the executable folder
- See TECHNICAL-GUIDE.md for FTS5 troubleshooting

### Slow searches
- First query is always slower (builds cache)
- Repeated queries should be 1-3ms
- Check database size (VACUUM if needed)
- See QUERY-ANALYTICS.md for performance analysis

## Related Documentation

- **[TECHNICAL-GUIDE.md](TECHNICAL-GUIDE.md)** - Implementation details, troubleshooting, extending the system
- **[DATABASE-SCHEMA.md](DATABASE-SCHEMA.md)** - Complete database schema reference
- **[QUERY-ANALYTICS.md](QUERY-ANALYTICS.md)** - SQL queries for usage analysis
- **[TESTS.md](TESTS.md)** - Testing guide and test suite

## Tips for AI Coding Agents

**Best Practices:**
1. **Use category filtering** to focus searches on relevant code
2. **Start specific, go broad**: Try exact names first, then partial matches
3. **Use --prefer user** to prioritize project code over stdlib
4. **Enable caching** by repeating important queries
5. **Filter by symbol type** when looking for specific constructs
6. **Use full-text search** for finding code by behavior (automatic with FTS5)

**Common Patterns:**
```bash
# Find a class definition
delphi-lookup.exe "TMyClass" --category user

# Find all validation functions
delphi-lookup.exe "validate" --symbol function

# Find database-related code
delphi-lookup.exe "database connection" -n 20

# Compare stdlib vs user implementations
delphi-lookup.exe "TForm" --category stdlib -n 3
delphi-lookup.exe "TForm" --category user -n 3
```

---

**Version**: 1.0
**Platform**: Windows x64
**Target**: AI Coding Agents (Claude Code, etc.)
