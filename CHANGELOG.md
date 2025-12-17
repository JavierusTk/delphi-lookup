# Changelog

All notable changes to delphi-lookup will be documented in this file.

## [Unreleased]

### Added
- **Gemini CLI Setup Guide**: One-shot configuration prompt for Gemini CLI users (`gemini/GEMINI_SETUP.md`)
- **Smart Cache Revalidation System**: Content-hash based cache validation that survives index updates
  - `content_hash` field in `symbols` table (MD5 of symbol content)
  - `query_cache` table replacing query_log for caching (one row per unique query)
  - Cache hits validate that referenced symbols still exist and content hasn't changed
  - Cache entries invalidated only when their specific symbols change (not on every reindex)
  - Support for caching "0 results" queries
- `--revalidate-cache [N]` command in delphi-indexer:
  - Revalidates invalidated queries with N+ hits (default: 3)
  - Purges obsolete cache entries (0 results >30 days, low hits >30 days)
  - Can be interrupted with Ctrl+C safely
  - Shows detailed progress and summary statistics
- `--stats` flag in delphi-lookup to show usage statistics (total queries, failure rate, avg duration)
- `--clear-cache` flag in delphi-lookup to delete all cached queries
- Cache preservation when no changes: `FinalizeFolder` no longer invalidates cache if no files were modified
- Realistic search examples documentation (`docs/HELP-SEARCH-EXAMPLES.md`)

### Changed
- **Vector Search Recommendations**: Clarified recommendations by user type:
  - AI agents: FTS5-only (iterate fast, 10x lower latency)
  - Human developers on English codebases: Vector search recommended (higher single-shot precision)
  - Updated CLAUDE.md and TECHNICAL-GUIDE.md with detailed rationale
- Cache format now uses `id:hash` pairs in `result_ids` for robust validation
- `query_cache` table tracks hit counts, first/last seen timestamps, and average duration
- `InvalidateQueryCache` now invalidates both `query_cache` and `query_log` tables
- `ShowStats` displays both query_log and query_cache statistics
- `ClearCache` clears both tables
- `FinalizeFolder` now requires `AFilesModified` parameter to decide whether to invalidate cache

### Technical
- Automatic schema migration for existing databases (adds `content_hash`, creates `query_cache`)
- One-time migration from `query_log` to `query_cache` on first use
- Backwards-compatible: still reads from `query_log` as fallback

## [1.0.0] - 2025-12-15

### Added
- **delphi-indexer.exe**: Pascal source code indexer with support for:
  - Incremental indexing (only modified files)
  - Automatic framework detection (VCL/FMX/RTL)
  - Source categories (user, stdlib, third_party)
  - FTS5 full-text search
  - Optional vector embeddings (Ollama)

- **delphi-lookup.exe**: Hybrid search tool with:
  - Exact name matching
  - Fuzzy matching (Levenshtein distance)
  - FTS5 full-text search
  - Optional semantic search (vectors)
  - Query caching
  - Filters by framework, category, symbol type

- **CheckFTS5.exe**: Diagnostic tool to verify FTS5 support

- CHM documentation indexing support (partially implemented)
- Multi-tier framework detection system (comment tags, mapping files, packages, uses clause)
- Query logging for analytics (`query_log` table)

### Technical
- SQLite database with WAL mode
- FTS5 for full-text search
- sqlite-vec for vector search (optional)
- DelphiAST for Pascal code parsing
