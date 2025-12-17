# Changelog

All notable changes to delphi-lookup will be documented in this file.

## [Unreleased]

### Added
- `--stats` flag in delphi-lookup to show usage statistics (total queries, failure rate, avg duration)
- `--clear-cache` flag in delphi-lookup to delete all cached queries
- Cache preservation when no changes: `FinalizeFolder` no longer invalidates cache if no files were modified
- Realistic search examples documentation (`docs/HELP-SEARCH-EXAMPLES.md`)

### Changed
- `FinalizeFolder` now requires `AFilesModified` parameter to decide whether to invalidate cache

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
