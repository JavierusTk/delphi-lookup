# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**delphi-lookup** is a high-performance Pascal source code search system designed for AI coding agents. It provides sub-millisecond cached searches across large Pascal codebases using FTS5 full-text search.

**Features:**
- Fast search: exact name + fuzzy matching + FTS5 full-text
- Framework-aware: VCL/FMX/RTL classification and filtering
- Documentation indexing: Official Delphi CHM help files
- Query caching: 1-3ms cache hits (100-2000x faster)
- Incremental indexing: Only processes new/modified files

> **Note on Vector/Semantic Search:** The codebase includes experimental support for vector embeddings (via Ollama), but this is **not recommended for production use**. Evaluation showed that for AI coding agents that can iterate (like Claude Code), FTS5-only search combined with agentic exploration provides better results with 10x lower latency. See "Vector Search Status" section below for details.

## Tools

### delphi-indexer.exe
Builds searchable database from Pascal source folders and CHM documentation.

```bash
# Index source folders
delphi-indexer.exe "W:\YourProject" --category user
delphi-indexer.exe "C:\...\Studio\23.0\source\vcl" --category stdlib --framework VCL

# Index CHM documentation
delphi-indexer.exe --index-chm "C:\...\Help\Doc\vcl.chm" --delphi-version 12.0

# List indexed folders
delphi-indexer.exe --list-folders
```

**Key parameters:**
- `--category` - Source classification: `user`, `stdlib`, `third_party`, `official_help`
- `--framework` - Force framework tag: `VCL`, `FMX`, `RTL` (skips auto-detection)
- `--type` - Content type: `code`, `help`, `markdown`
- `--force` - Full reindex (ignore timestamps/hashes)
- `--index-chm` - Index CHM documentation file
- `--delphi-version` - Version tag for CHM (default: 12.0)

### delphi-lookup.exe
Fast hybrid search with caching and framework filtering.

```bash
# Basic searches
delphi-lookup.exe "TStringList" -n 5
delphi-lookup.exe "JSON serialization" -n 3

# Framework filtering
delphi-lookup.exe "TButton" --framework VCL -n 5
delphi-lookup.exe "TForm" --framework FMX -n 3

# Category filtering
delphi-lookup.exe "ShowMessage" --category stdlib -n 5
delphi-lookup.exe "TMyClass" --prefer user -n 10

# Symbol type filtering
delphi-lookup.exe "MAX_BUFFER" --symbol const
delphi-lookup.exe "ValidateInput" --symbol function

# Advanced
delphi-lookup.exe "validation logic" --use-reranker --candidates 100
```

**Key parameters:**
- `-n` - Number of results (default: 5)
- `--framework` - Filter by framework: `VCL`, `FMX`, `RTL`
- `--category` - Filter by category: `user`, `stdlib`, `third_party`, `official_help`
- `--prefer` - Boost category in results (doesn't exclude others)
- `--symbol` - Filter by type: `class`, `function`, `const`, `var`, etc.
- `--use-reranker` - Enable two-stage reranking (improves precision ~75% → ~95%, +100ms)
- `--candidates` - Candidate count for reranking (default: 50)

### CheckFTS5.exe
Diagnostic tool to verify FTS5 support in sqlite3.dll.

```bash
./CheckFTS5.exe
```

## Configuration

delphi-lookup supports configuration via JSON file, environment variables, or command-line parameters (in order of precedence: CLI > env > config file).

### Configuration File

Copy `delphi-lookup.example.json` to `delphi-lookup.json`:

```json
{
  "database": "delphi_symbols.db",
  "buffer_size": 500,
  "category": "user",
  "num_results": 5
}
```

> **Note:** FTS5-only mode is the default and recommended configuration. Embedding parameters exist but are not recommended (see "Vector Search Status" section).

### Configuration Parameters

| Parameter | Default | Used By | Description |
|-----------|---------|---------|-------------|
| `database` | `delphi_symbols.db` | Both | SQLite database file |
| `buffer_size` | `500` | Indexer | Batch size for processing |
| `category` | `user` | Indexer | Default source category |
| `num_results` | `5` | Search | Number of results to return |

### Experimental Parameters (Not Recommended)

These parameters enable vector embeddings and reranking. **Not recommended for AI agent workflows** - see "Vector Search Status" section for rationale.

| Parameter | Default | Description |
|-----------|---------|-------------|
| `embedding_url` | `""` | Embedding service URL (empty = disabled) |
| `semantic_search` | `false` | Enable vector similarity search |
| `use_reranker` | `false` | Enable two-stage reranking |

### Special Behaviors

- **No config file**: Use `--no-config` to skip loading `delphi-lookup.json`

## Framework Detection System

delphi-lookup includes intelligent, multi-tier framework detection to accurately categorize mixed RTL/VCL/FMX code.

### Package Intelligence

```bash
# Scan .dpk packages to populate package database
delphi-indexer.exe --scan-packages "C:\Projects\Packages" --category user

# Scan Delphi standard library packages
delphi-indexer.exe --scan-packages "C:\...\Studio\23.0\lib" --category stdlib

# List all scanned packages
delphi-indexer.exe --list-packages
```

**Framework detection from packages:**
- Analyzes `requires` clause in `.dpk` files
- Detects VCL (requires `vcl`), FMX (requires `fmx`), or RTL (only RTL packages)
- Tracks file membership per package

### Mapping File Generation

```bash
# Preview framework mappings (dry run)
delphi-indexer.exe --generate-mapping "C:\Projects\MyLib" --output framework-overrides.json --dry-run

# Generate mapping file
delphi-indexer.exe --generate-mapping "C:\Projects\MyLib" --output framework-overrides.json

# Force regeneration
delphi-indexer.exe --generate-mapping "C:\Projects\MyLib" --output framework-overrides.json --force
```

**Mapping file format (JSON):**
```json
{
  "version": "1.0",
  "overrides": [
    {
      "file": "C:\\Projects\\MyLib\\Utils\\Logger.pas",
      "framework": "RTL",
      "reason": "Package-level framework: RTL"
    },
    {
      "pattern": "C:\\ThirdParty\\**\\*.pas",
      "framework": "RTL",
      "reason": "Third-party RTL-only library"
    }
  ]
}
```

### Multi-Tier Detection

Framework detection uses 5-tier priority system:

1. **Comment tag** - Explicit declaration in first 10 lines:
   ```pascal
   // FRAMEWORK: RTL
   unit MyUnit;
   ```

2. **Mapping file** - Exact path or glob pattern match in `framework-overrides.json`

3. **Package database** - Framework from `.dpk` requires clause

4. **Uses clause analysis** - Detects from interface + implementation uses:
   - `Vcl.*` or (`Forms`, `Controls`, `Dialogs`) → VCL
   - `FMX.*` → FMX
   - Only `System.*`, `Data.*`, `WinApi.*` → RTL

5. **Folder path** - Fallback detection from path patterns

### Indexing with Framework Detection

```bash
# Use mapping file for framework detection
delphi-indexer.exe "C:\Projects\MyLib" --mapping-file framework-overrides.json --category user

# Explicit framework override (skips detection)
delphi-indexer.exe "C:\Projects\MyLib\Utils" --framework RTL --category user
```

### Validation Warnings

Detection validates consistency and warns about:
- Comment tag conflicts with uses clause
- Mapping file conflicts with package framework
- File framework more restrictive than package (possible .dpk error)
- Orphaned files (not in any package, require manual categorization)

### Workflow

**Recommended setup:**
1. Scan packages: `--scan-packages "C:\Projects\Packages"`
2. Generate mapping: `--generate-mapping "C:\Projects\MyLib" --output framework-overrides.json`
3. Review orphaned files and add to packages or comment tags
4. Index with mapping: `delphi-indexer.exe "C:\Projects\MyLib" --mapping-file framework-overrides.json`

## Database Schema

**File:** `delphi_symbols.db` (SQLite with WAL mode)

**Core tables:**
- `symbols` - Symbol definitions (name, type, content, file_path, framework, etc.)
- `symbols_fts` - FTS5 full-text search index
- `vec_embeddings` - Vector embeddings (1536-dimensional)
- `query_log` - Query caching and analytics
- `indexed_folders` - Incremental indexing tracking
- `packages` - Package registry with framework detection
- `package_files` - File membership tracking
- `metadata` - Self-describing schema info

**Documentation fields:**
- `framework` - VCL, FMX, RTL (auto-detected or manual)
- `platforms` - Windows, Android, iOS, macOS, Linux
- `delphi_version` - Version tag (e.g., '12.0')
- `introduced_version`, `deprecated_version` - Version tracking
- `is_inherited`, `inherited_from` - Inheritance tracking

## Indexing Strategy

**Recommended workflow (with framework detection):**

```bash
# Step 1: Scan all packages first (one-time setup)
delphi-indexer.exe --scan-packages "C:\Projects\Packages" --category user

# Step 2: Generate framework mapping file
delphi-indexer.exe --generate-mapping "C:\Projects\MyLib" --output framework-overrides.json
# Review orphaned files and add to packages or comment tags

# Step 3: Index user code with mapping file
delphi-indexer.exe "C:\Projects\MyFramework" --mapping-file framework-overrides.json --category user
delphi-indexer.exe "C:\Projects\MyApplication" --mapping-file framework-overrides.json --category user

# Step 4: Index Delphi standard library
delphi-indexer.exe "C:\...\source\rtl" --category stdlib
delphi-indexer.exe "C:\...\source\vcl" --category stdlib
delphi-indexer.exe "C:\...\source\data" --category stdlib

# Step 5: CHM documentation
delphi-indexer.exe --index-chm "C:\...\Help\Doc\vcl.chm" --delphi-version 12.0
delphi-indexer.exe --index-chm "C:\...\Help\Doc\system.chm" --delphi-version 12.0

# Step 6: Third-party libraries (with explicit framework)
delphi-indexer.exe "C:\ThirdParty\mORMot2\src" --category third_party --framework RTL
```

**Simple workflow (without framework detection):**

```bash
# User code (explicit framework)
delphi-indexer.exe "C:\Projects\MyVCLLib" --category user --framework VCL
delphi-indexer.exe "C:\Projects\MyApplication" --category user

# Standard library
delphi-indexer.exe "C:\...\source\vcl" --category stdlib --framework VCL
```

**Use `reposcan.bat`** for automated batch indexing.

## Performance

**Indexing:**
- ~43 chunks/second
- ~1,000 symbols in 25-30 seconds
- Incremental: Only processes new/modified files

**Searching:**
- **Cache hit:** 1-3ms (repeated query)
- **Cache miss:** 200-2000ms (first query)
- **With reranking:** +100-150ms

**Scalability:**
- Tested with 482,545+ symbols
- Database: ~500MB

## Search Quality

**Recommended mode: FTS5-only** (~400ms latency)
- Exact name matching
- Fuzzy name matching (Levenshtein distance)
- FTS5 full-text content search

For AI coding agents that iterate (like Claude Code with Explore Agent), FTS5-only combined with multiple search passes provides better results than single-shot semantic search.

## Vector Search Status

**Status: Experimental / Not Recommended**

The codebase includes full support for vector embeddings, but after evaluation we recommend **not using it** for AI coding agent workflows.

**Benchmark results (December 2025):**

| Mode | Precision@10 | Hit Rate@10 | Latency |
|------|--------------|-------------|---------|
| FTS5 only | 35% | 30% | ~400ms |
| FTS5 + Vector | 64% | 95% | ~4500ms |

**Why we don't recommend vector search for AI agents:**

1. **Claude Code abandoned RAG** - Anthropic's own Claude Code tool tried semantic search with Voyage embeddings and found "agentic search" (grep/glob with iteration) outperformed it. Quote from Anthropic: "Semantic search is usually faster than agentic search, but less accurate, more difficult to maintain, and less transparent."

2. **Agents iterate** - The 64% single-shot precision looks good, but AI agents can do 2-3 fast FTS5 searches and synthesize results, matching or exceeding that precision while being 10x faster.

3. **Context-specific factors** - Spanish-language code and Delphi/Pascal are underrepresented in embedding model training data, reducing effectiveness.

4. **Operational overhead** - Requires Ollama server running, adds complexity with marginal benefit.

**The code remains available** if future improvements (faster models, better multilingual embeddings) change the calculus. To enable: configure `embedding_url` in `delphi-lookup.json`.

**Potential use case: External documentation** — Vector search may be valuable for indexing external documentation (ElevateDB manual, Report Builder manual, etc.) where conceptual queries in natural language benefit from semantic search. Unlike Spanish code with Delphi identifiers, technical documentation is typically in English prose—exactly where embeddings excel.

## Architecture

### Indexing Pipeline
1. **uFolderScanner.pas** - Recursive .pas file discovery
2. **uASTProcessor.pas** - Pascal AST parsing + comment extraction (via DelphiAST)
3. **uIndexerEmbeddings.Ollama.pas** - Vector embedding generation
4. **uDatabaseBuilder.pas** - SQLite storage with FTS5

### Search Pipeline
1. **uQueryProcessor.pas** - 5-tier hybrid search
2. **uVectorSearch.pas** - Vector similarity (optional)
3. **uReranker.pas** - Two-stage reranking (optional)
4. **uResultFormatter.pas** - AI-friendly output

### CHM Indexing Pipeline
1. **uCHMExtractor.pas** - CHM extraction (Python wrapper)
2. **uCHMParser.pas** - HTML parsing
3. **uDocChunker.pas** - Documentation chunking
4. **uDocTypes.pas** - Type definitions

## Dependencies

**External services (optional):**
- **Ollama** (for embeddings): Configure `embedding_url` in `delphi-lookup.json`
- **Model:** `jina-code-embed-1-5b` (1536 dimensions)
- Without Ollama: Works offline with FTS5-only search (~75% precision)

**Required DLLs:**
- **sqlite3.dll** - FTS5-enabled (included in `bin/`)
- **vec0.dll** - sqlite-vec extension (included in `bin/`)

**Delphi libraries:**
- **mORMot 2** - JSON processing
- **FireDAC** - Database access (included with RAD Studio)
- **DelphiAST** - Pascal AST parsing (included)

## Database Maintenance

```bash
# Check FTS5 status
./CheckFTS5.exe

# List indexed folders
delphi-indexer.exe --list-folders

# Monitor database size
ls -lh delphi_symbols.db*

# Clean old query logs (via SQL)
sqlite3 delphi_symbols.db "DELETE FROM query_log WHERE cache_valid = 0 AND executed_at < datetime('now', '-30 days')"

# Reclaim space
sqlite3 delphi_symbols.db "VACUUM"
```

## Important Notes

**FTS5 Requirement:**
- External `sqlite3.dll` is REQUIRED (FireDAC's embedded SQLite may lack FTS5)
- Must configure FireDAC to use the external DLL (see Code Patterns below)
- Use `CheckFTS5.exe` to verify FTS5 support

**Cache Behavior:**
- Cache source: `cache_valid = 1` (first query with hash)
- Cache hit: `cache_valid = 0` (repeated query, analytics only)
- One cache entry per unique query hash
- Cache invalidated when delphi-indexer modifies data

**Automatic Exclusions:**
delphi-indexer skips these folders automatically:
- `.git`, `.svn`, `.hg` (version control)
- `__history`, `__recovery`, `backup` (Delphi history)
- `Win32`, `Win64`, `Debug`, `Release`, `__DCU_CACHE` (build output)
- Folders containing `.noindex` file

## Code Patterns

**Connection Setup (CRITICAL):**
```pascal
// FireDAC SQLite connection with external DLL for FTS5
FDConnection.DriverName := 'SQLite';
FDConnection.Params.Database := 'delphi_symbols.db';

// Use external sqlite3.dll (required for FTS5)
var DriverLink := TFDPhysSQLiteDriverLink.Create(nil);
DriverLink.VendorLib := ExtractFilePath(ParamStr(0)) + 'bin\sqlite3.dll';

// Enable WAL mode after connection
FDConnection.ExecSQL('PRAGMA journal_mode=WAL');
```

**Framework Detection:**
```pascal
// Automatic (in TDatabaseBuilder.InsertSymbol)
Framework := DetectFramework(FilePath, UnitName);

// Manual override (via parameter)
DatabaseBuilder.BuildDatabase(..., ExplicitFramework := 'VCL');
```

**Query Caching:**
```pascal
QueryHash := GenerateQueryHash(Query, [Filters...]);
// Check cache first, execute search if cache miss
```

## Related Documentation

- **[README.md](README.md)** - Quick start guide
- **[USER-GUIDE.md](USER-GUIDE.md)** - Complete usage guide
- **[TECHNICAL-GUIDE.md](TECHNICAL-GUIDE.md)** - Implementation details
- **[DATABASE-SCHEMA.md](DATABASE-SCHEMA.md)** - Schema reference
- **[TESTS.md](TESTS.md)** - Testing guide

## Build Environment

- **Platform:** Windows x64
- **Delphi:** RAD Studio 12
- **Project files:** `delphi-indexer.dproj`, `delphi-lookup.dproj`, `CheckFTS5.dproj`

---

**Version:** 1.0
**Target:** AI Coding Agents (Claude Code, etc.)
**Status:** Production-ready
