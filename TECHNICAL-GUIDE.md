# delphi-lookup - Technical Guide for AI Coding Agents

## Overview

This guide provides technical implementation details for AI agents maintaining, troubleshooting, or extending the delphi-lookup system.

## Architecture

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                      delphi-indexer.exe                        │
│  ┌────────────┐  ┌────────────┐  ┌──────────────────────┐  │
│  │  Folder    │→ │    AST     │→ │  Ollama Embeddings   │  │
│  │  Scanner   │  │ Processor  │  │  (jina-code-embed)   │  │
│  └────────────┘  └────────────┘  └──────────────────────┘  │
│         ↓               ↓                     ↓             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │         Database Builder (SQLite + FTS5 + Vectors)   │  │
│  └──────────────────────────────────────────────────────┘  │
└───────────────────────────┬─────────────────────────────────┘
                            ↓
                   delphi_symbols.db
                   (SQLite + WAL mode)
                            ↓
┌───────────────────────────┴─────────────────────────────────┐
│                      delphi-lookup.exe                         │
│  ┌────────────┐  ┌────────────┐  ┌──────────────────────┐  │
│  │  Cache     │→ │   Query    │→ │    Result            │  │
│  │  Lookup    │  │ Processor  │  │   Formatter          │  │
│  └────────────┘  └────────────┘  └──────────────────────┘  │
│         ↓               ↓                     ↓             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Hybrid Search: Exact + Fuzzy + FTS5 + Vector        │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Data Flow

**Indexing:**
1. `uFolderScanner` - Recursively discovers `.pas` files
2. `uASTProcessor` - Parses Pascal code into AST chunks
3. `uEmbeddings.Ollama` - Generates 1536-dim vectors via Ollama API
4. `uDatabaseBuilder` - Stores symbols, vectors, and metadata in SQLite

**Searching:**
1. Cache lookup (MD5 hash of query + filters)
2. If cache miss:
   - Exact match (symbol name = query)
   - Partial match (symbol name contains query)
   - Fuzzy match (parent class, similar names)
   - FTS5 full-text (content and comments)
   - Vector similarity (if enabled)
3. Merge results, deduplicate, rank by relevance
4. Store result IDs in cache
5. Format and output

## Database Implementation

### SQLite Configuration

**WAL Mode:**
```sql
PRAGMA journal_mode=WAL;
```

Benefits:
- Concurrent reads while writing
- Allows query logging without blocking searches
- Automatic checkpointing

**FTS5 Configuration:**

All connections use external FTS5-enabled `sqlite3.dll` via FireDAC:
```pascal
// Configure FireDAC to use external DLL
var DriverLink := TFDPhysSQLiteDriverLink.Create(nil);
DriverLink.VendorLib := ExtractFilePath(ParamStr(0)) + 'bin\sqlite3.dll';

FDConnection.DriverName := 'SQLite';
FDConnection.Params.Database := 'delphi_symbols.db';
```

**Important**: FireDAC's embedded SQLite may lack FTS5 support. Always use external `sqlite3.dll`.

### Table: symbols

Main table storing all indexed code symbols.

```sql
CREATE TABLE symbols (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  full_name TEXT,
  content TEXT NOT NULL,
  file TEXT NOT NULL,
  type TEXT NOT NULL,
  comments TEXT,
  parent_class TEXT,
  implemented_interfaces TEXT,
  visibility TEXT,
  content_type TEXT DEFAULT 'code',
  source_category TEXT DEFAULT 'user',
  domain_tags TEXT,
  unit_name TEXT,
  line_number INTEGER
)
```

**Indices:**
```sql
CREATE INDEX idx_symbols_name ON symbols(name COLLATE NOCASE);
CREATE INDEX idx_symbols_type ON symbols(type);
CREATE INDEX idx_symbols_category ON symbols(source_category);
CREATE INDEX idx_symbols_file ON symbols(file);
```

### Table: vec_embeddings

Vector embeddings for semantic search (sqlite-vec virtual table).

```sql
CREATE VIRTUAL TABLE vec_embeddings USING vec0(
  symbol_id INTEGER PRIMARY KEY,
  embedding FLOAT[1536]
)
```

**Note**: Created by loading `vec0.dll` extension.

**Query syntax:**
```sql
SELECT symbol_id, vec_distance_L2(embedding, :query_vector) as distance
FROM vec_embeddings
WHERE embedding MATCH :query_vector
  AND k = :limit
ORDER BY distance
```

### Table: symbols_fts

FTS5 virtual table for full-text search.

```sql
CREATE VIRTUAL TABLE symbols_fts USING fts5(
  name,
  full_name,
  content,
  comments,
  content=symbols,
  content_rowid=id
)
```

**Synchronized with symbols table** via triggers (not shown, created automatically by FTS5).

**Query syntax:**
```sql
SELECT rowid, rank
FROM symbols_fts
WHERE symbols_fts MATCH :query
ORDER BY rank
LIMIT :limit
```

### Table: query_log

Unified query logging and caching table.

```sql
CREATE TABLE query_log (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  query_text TEXT NOT NULL,
  query_hash TEXT NOT NULL,

  -- Cache fields
  result_ids TEXT,
  cache_valid INTEGER DEFAULT 1,

  -- Filters (part of cache key)
  content_type_filter TEXT,
  source_category_filter TEXT,
  prefer_category TEXT,
  domain_tags_filter TEXT,
  symbol_type_filter TEXT,

  -- Search configuration
  num_results_requested INTEGER,
  max_distance REAL,
  use_semantic_search INTEGER DEFAULT 0,
  use_reranker INTEGER DEFAULT 0,
  candidate_count INTEGER,

  -- Performance metrics (milliseconds)
  duration_ms INTEGER NOT NULL,
  result_count INTEGER NOT NULL,
  exact_match_found INTEGER DEFAULT 0,
  cache_hit INTEGER DEFAULT 0
)
```

**Indices:**
```sql
CREATE INDEX idx_query_log_hash ON query_log(query_hash);
CREATE INDEX idx_query_log_cache_valid ON query_log(cache_valid, query_hash);
```

**Cache Strategy:**

- `cache_valid = 1`: Cache source (first query, contains result_ids)
- `cache_valid = 0`: Analytics log (repeated query, not used for caching)

**Benefits:**
- One cache entry per unique query (no duplicates competing)
- Full analytics preserved (all executions logged)
- Aggressive cleanup of analytics logs (keep cache sources)

**Cache Lookup:**
```sql
SELECT result_ids
FROM query_log
WHERE query_hash = :hash
  AND cache_valid = 1
ORDER BY executed_at DESC
LIMIT 1
```

**Cache Invalidation:**

Triggered when `delphi-indexer` modifies data:
```pascal
procedure InvalidateQueryCache;
begin
  FQuery.SQL.Text := 'UPDATE query_log SET cache_valid = 0';
  FQuery.ExecSQL;
end;
```

### Table: indexed_folders

Tracks which folders have been indexed and when.

```sql
CREATE TABLE indexed_folders (
  folder_path TEXT PRIMARY KEY,
  indexed_at TIMESTAMP NOT NULL,
  duration_seconds INTEGER NOT NULL,
  files_count INTEGER NOT NULL,
  chunks_count INTEGER NOT NULL,
  content_type TEXT DEFAULT 'code',
  source_category TEXT DEFAULT 'user'
)
```

Used for:
- Incremental indexing (skip unchanged files)
- Progress reporting
- Usage statistics

### Table: metadata

Self-describing database metadata.

```sql
CREATE TABLE metadata (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
)
```

**Standard keys:**
- `embedding_model` - Ollama model name (e.g., "jina-code-embed-1-5b")
- `embedding_dimensions` - Vector dimensions (e.g., "1536")
- `ollama_url` - Ollama server URL
- `created_at` - Database creation timestamp
- `schema_version` - Database schema version

**Ensures query-time model compatibility**: delphi-lookup reads these values to generate compatible embeddings.

## Code Organization

### delphi-indexer

**Main Program**: `delphi-indexer.dpr`
- Command-line parsing
- Database initialization
- Orchestrates indexing pipeline

**Units:**
- `uConfig.pas` - Configuration constants (URLs, defaults, limits) - unified for both tools
- `uFolderScanner.pas` - Recursive folder scanning, file filtering
- `uASTProcessor.pas` - Pascal AST parsing, code chunking
- `uIndexerEmbeddings.Ollama.pas` - Ollama API client, batched embedding generation
- `uDatabaseBuilder.pas` - SQLite operations, FTS5 setup, vector storage
- `uGlossaryEnricher.pas` - Domain tag enrichment (optional)

**Key Classes:**

`TFolderScanner`:
- `ScanFolder()` - Recursive .pas file discovery
- Respects `.noindex` files
- Filters version control, build folders

`TASTProcessor`:
- `ProcessFile()` - Parse Pascal source to AST chunks
- State machine for block detection
- Handles forward declarations

`TOllamaEmbeddingGenerator`:
- `GenerateEmbedding()` - Single embedding generation
- `GenerateEmbeddings()` - Batched generation (100 chunks)
- HTTP/JSON via Indy components

`TDatabaseBuilder`:
- `CreateSchema()` - Create tables, indices, FTS5
- `InsertSymbols()` - Insert symbols + vectors + FTS5 sync
- `InvalidateQueryCache()` - Cache invalidation
- `CleanupQueryLogs()` - Remove old logs

### delphi-lookup

**Main Program**: `delphi-lookup.dpr`
- Command-line parsing
- Cache lookup logic
- Query logging

**Units:**
- `uConfig.pas` - Configuration constants (unified for both tools)
- `uSearchTypes.pas` - Type definitions (TSearchResult, etc.)
- `uQueryProcessor.pas` - Hybrid search implementation
- `uVectorSearch.pas` - Vector similarity search
- `uResultFormatter.pas` - Output formatting
- `uReranker.pas` - Two-stage reranking (optional)
- `uLookupEmbeddings.Ollama.pas` - Ollama API client for query embeddings

**Key Classes:**

`TQueryProcessor`:
- `PerformHybridSearch()` - Main search orchestration
- `ExactSearch()` - Name matching (case-insensitive)
- `FuzzySearch()` - Parent class, partial match
- `FTSSearch()` - Full-text search (FTS5)

`TVectorSearch`:
- `SearchSimilar()` - Vector similarity search
- Loads sqlite-vec extension (`vec0.dll`)
- Reads model metadata from database

`TResultFormatter`:
- `FormatResults()` - Format output for AI agents
- Groups by relevance tier
- Includes file paths, match types

## Pascal Code Chunking

### State Machine Approach

`TASTProcessor` uses a state machine to chunk Pascal source into logical blocks:

**States:**
1. **Outside Block** - Scanning for block starts
2. **In Class Declaration** - Inside `class...end`
3. **In Interface Declaration** - Inside `interface...end`
4. **In Record Declaration** - Inside `record...end`
5. **In Routine** - Inside `procedure/function...end`

**Transitions:**
- `class`, `interface`, `record` → Enter block
- `procedure`, `function`, `constructor`, `destructor` → Enter routine
- `end;` → Exit block (tracks nesting level)

**Forward Declaration Handling:**
```pascal
TMyClass = class;  // Forward declaration (skip)
...
TMyClass = class   // Actual declaration (process)
  ...
end;
```

**Output:**
```json
{
  "file": "Unit1.pas",
  "type": "class",
  "name": "TMyClass",
  "content": "TMyClass = class...",
  "line_number": 42
}
```

## Extension Loading

### sqlite-vec Extension

**File**: `vec0.dll` (included)

**Loading:**
```pascal
// Remove .dll extension for SQLite convention
var ExtPath := ChangeFileExt('vec0.dll', '');
FQuery.SQL.Text := Format('SELECT load_extension(%s)', [QuotedStr(ExtPath)]);
FQuery.Open;
```

**Fallback Locations:**
1. Same folder as executable
2. Two levels up (delphi-lookup root)

**Error Handling:**
- If not found: Raise exception
- If load fails: Raise exception with details

## Performance Optimization

### Indexing Performance

**Batched Embedding Generation:**
```pascal
// Process 100 chunks per API call
const BATCH_SIZE = 100;

for I := 0 to (ChunkCount div BATCH_SIZE) do
begin
  BatchChunks := ExtractBatch(Chunks, I * BATCH_SIZE, BATCH_SIZE);
  BatchEmbeddings := Generator.GenerateEmbeddings(BatchChunks);
  InsertBatch(BatchEmbeddings);
end;
```

**Benefits:**
- Reduces HTTP overhead (100x fewer requests)
- Ollama batches embedding generation internally
- ~10x faster than individual calls

**Incremental Indexing:**
```pascal
// Check file modification time
if FileAge(FilePath) <= FolderLastIndexed then
  Skip(FilePath);  // File unchanged since last index
```

**Benefits:**
- Only processes new/modified files
- Dramatically faster re-indexing
- Tracked via `indexed_folders` table

### Search Performance

**Query Caching:**

MD5 hash of query + all filters:
```pascal
function GenerateQueryHash(const AQuery: string; const AFilters: array of string): string;
var
  Combined: string;
begin
  Combined := AQuery;
  for Filter in AFilters do
    Combined := Combined + '|' + Filter;
  Result := THashMD5.GetHashString(Combined);
end;
```

**Cache Flow:**
1. Generate hash
2. Look up in `query_log` WHERE `cache_valid = 1`
3. If found: Load symbols by result_ids (1-3ms)
4. If not found: Execute full search, store result_ids

**Performance:**
- Cache hit: **1-3ms** (100-2000x faster)
- Cache miss: 200-2000ms (depends on query complexity)

**Parallel Search:**

All search methods run concurrently:
```pascal
// Pseudo-code
TThread.CreateAnonymousThread(ExactSearch).Start;
TThread.CreateAnonymousThread(FuzzySearch).Start;
TThread.CreateAnonymousThread(FTSSearch).Start;
TThread.CreateAnonymousThread(VectorSearch).Start;

WaitForAllThreads;
MergeAndRankResults;
```

**Deduplication:**

Results merged by symbol ID:
```pascal
var SeenIDs := TDictionary<Integer, Boolean>.Create;
for Result in AllResults do
  if not SeenIDs.ContainsKey(Result.SymbolID) then
  begin
    FinalResults.Add(Result);
    SeenIDs.Add(Result.SymbolID, True);
  end;
```

### Vector Search Benchmark Results

Benchmark conducted December 2025 with 24 queries across 5 categories:
- Conceptual (natural language)
- Bilingual (Spanish↔English)
- Functionality search
- Business domain terms
- Abstract patterns

**Results:**

| Mode | Precision@10 | Hit Rate@10 | Latency |
|------|--------------|-------------|---------|
| FTS5 only | 35% | 30% | ~400ms |
| FTS5 + Vector | **64%** | **95%** | ~4500ms |
| FTS5 + Vector + Reranker | 65% | 95% | ~4600ms |

**Key Findings:**

1. **Vector search dramatically improves semantic understanding:**
   - Precision: +83% relative improvement (35% → 64%)
   - Hit Rate: +217% relative improvement (30% → 95%)

2. **Reranker provides minimal benefit:** Only +1% precision for ~100ms extra latency

3. **Best use cases for vector search:**
   - Conceptual queries in natural language ("send email with attachment")
   - Finding semantically related code without exact name matches
   - Human developers who need high single-shot precision

4. **When to use FTS5-only:** AI coding agents that iterate

**Recommendations by User Type:**

| User Type | Codebase | Mode | Rationale |
|-----------|----------|------|-----------|
| AI Agent | Any | FTS5-only | Iterate fast, synthesize results |
| Human | English | Vector | Single-shot precision matters, 4.5s acceptable |
| Human | Non-English | FTS5-only | Embedding models trained on English |

**For AI Agents:** FTS5-only is recommended. Agents can do 2-3 fast searches (~400ms each) and synthesize results, achieving comparable precision to vector search while being 10x faster overall. Anthropic's Claude Code team reached the same conclusion after testing semantic search with Voyage embeddings.

**For Human Developers (English codebases):** Vector search is recommended. When enabled, full method implementations (up to 8192 chars) are indexed, providing rich semantic content. Expected precision: 75-85%. The 4.5s latency is acceptable for humans waiting for search results.

See CLAUDE.md "Vector Search Status" for configuration details.

## Troubleshooting

### FTS5 Not Available

**Symptom:** "no such module: fts5" or slow full-text searches

**Diagnosis:**
1. Check connection settings:
   ```pascal
   Connection.SpecificOptions.Values['Direct'];  // Must be 'False'
   Connection.SpecificOptions.Values['ClientLibrary'];  // Must be 'sqlite3.dll'
   ```

2. Verify DLL exists and has FTS5:
   ```bash
   # Check file exists
   ls -lh sqlite3.dll

   # Check for FTS5 symbols (Linux/WSL)
   strings sqlite3.dll | grep ENABLE_FTS5
   ```

3. Test FTS5 in database:
   ```sql
   CREATE VIRTUAL TABLE test_fts USING fts5(content);
   DROP TABLE test_fts;
   ```

**Solution:**
- Ensure `sqlite3.dll` is in executable folder
- Use included FTS5-enabled DLL (3.1MB file)
- Don't use `Direct := 'True'` (embedded SQLite, no FTS5)

**Tool:** Use `CheckFTS5.exe` diagnostic tool:
```bash
CheckFTS5.exe
```

### Database Locks

**Symptom:** "database is locked" errors

**Causes:**
1. Writing while read transaction open
2. Multiple processes accessing same database
3. WAL mode not enabled

**Solutions:**

**Enable WAL mode** (done automatically now):
```pascal
FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
FQuery.ExecSQL;
```

**Close connections before logging:**
```pascal
// Free search connections first
QueryProcessor.Free;
VectorSearch.Free;

// Then log (uses separate connection)
LogQuery(...);
```

**Avoid transaction deadlocks:**
```pascal
// Don't nest transactions
Connection.BeginTrans;
try
  // Do work
  Connection.Commit;
except
  Connection.Rollback;
  raise;
end;
```

### Slow Vector Searches

**Symptom:** Vector searches take >1 second

**Diagnosis:**
1. Check embedding dimensions match:
   ```sql
   SELECT value FROM metadata WHERE key = 'embedding_dimensions';
   ```

2. Check vector count:
   ```sql
   SELECT COUNT(*) FROM vec_embeddings;
   ```

3. Profile query:
   ```sql
   EXPLAIN QUERY PLAN
   SELECT symbol_id, vec_distance_L2(embedding, :vec) as dist
   FROM vec_embeddings
   WHERE embedding MATCH :vec AND k = 10
   ```

**Solutions:**

**Reduce candidate count:**
```bash
delphi-lookup.exe "query" --enable-semantic --candidates 20  # Instead of 50
```

**Check Ollama server:**
```bash
# Test embedding generation (use embedding_url from delphi-lookup.json or OLLAMA_URL env var)
curl -X POST http://127.0.0.1:11434/api/embeddings \
  -d '{"model": "jina-code-embed-1-5b", "prompt": "test"}'
```

**Verify sqlite-vec loaded:**
```sql
SELECT load_extension('vec0');  -- Should return without error
```

### Memory Usage

**Symptom:** High memory usage during indexing or search

**Causes:**
1. Loading entire codebase into memory
2. Keeping all results in memory
3. Large embedding batches

**Solutions:**

**Reduce batch size** (indexing):
```pascal
// In uIndexerEmbeddings.Ollama.pas
const BATCH_SIZE = 50;  // Instead of 100
```

**Limit result count** (searching):
```bash
delphi-lookup.exe "query" -n 10  # Instead of 100
```

**Process files in chunks** (indexing):
Already implemented - files processed one at a time.

## Extending the System

### Adding New Search Methods

1. **Create search method in `TQueryProcessor`:**
```pascal
function TQueryProcessor.MyCustomSearch(const AQuery: string): TSearchResultList;
begin
  Result := TSearchResultList.Create;
  // Implement search logic
end;
```

2. **Call from `PerformHybridSearch`:**
```pascal
CustomResults := MyCustomSearch(QueryText);
AllResults.AddRange(CustomResults);
```

3. **Merge and rank:**
Already handled by existing merge logic.

### Adding New Filters

1. **Add filter field to symbols table:**
```sql
ALTER TABLE symbols ADD COLUMN my_filter TEXT;
CREATE INDEX idx_symbols_my_filter ON symbols(my_filter);
```

2. **Populate during indexing:**
```pascal
// In TASTProcessor
Chunk.MyFilter := ExtractMyFilter(Code);
```

3. **Add command-line option:**
```pascal
// In delphi-lookup.dpr
if FindCmdLineSwitch('my-filter', MyFilterValue, True, [clstValueNextParam]) then
  MyFilter := MyFilterValue;
```

4. **Apply in search:**
```pascal
if MyFilter <> '' then
  Query.SQL.Add('AND my_filter = :filter');
```

### Adding New Output Formats

1. **Create formatter class:**
```pascal
type
  TMyFormatter = class(TResultFormatter)
    procedure FormatResults(...); override;
  end;
```

2. **Implement format logic:**
```pascal
procedure TMyFormatter.FormatResults(...);
begin
  // Output in custom format (JSON, XML, etc.)
end;
```

3. **Use in main program:**
```pascal
if OutputFormat = 'my-format' then
  Formatter := TMyFormatter.Create
else
  Formatter := TResultFormatter.Create;
```

## Best Practices

### For Indexing

1. **Index in order of importance:**
   - User code first (most queries target this)
   - Standard library second
   - Third-party last

2. **Use appropriate categories:**
   - `user` - Your project code
   - `stdlib` - Delphi RTL/VCL/FMX
   - `third_party` - External libraries
   - `official_help` - Documentation
   - `project_docs` - Project-specific docs

3. **Exclude build artifacts:**
   - Create `.noindex` in build output folders
   - Exclude `__history`, `backup` folders (automatic)

4. **Re-index when:**
   - Significant code changes
   - Changing embedding model
   - Database corruption

### For Searching

1. **Use category filters:**
   ```bash
   --category user      # Only search your code
   --prefer user        # Boost your code in results
   ```

2. **Enable caching:**
   - Repeated queries are 100-2000x faster
   - No configuration needed (automatic)

3. **Disable semantic search:**
   - Default is disabled (low precision)
   - Only enable for conceptual queries

4. **Use reranker for quality:**
   ```bash
   --use-reranker  # Improves precision from ~75% to ~95%
   ```

### For Maintenance

1. **Monitor query logs:**
   ```sql
   -- See QUERY-ANALYTICS.md for analysis queries
   SELECT query_text, COUNT(*) as freq
   FROM query_log
   GROUP BY query_text
   ORDER BY freq DESC
   LIMIT 10;
   ```

2. **Clean old logs periodically:**
   ```sql
   -- Remove analytics logs older than 30 days
   DELETE FROM query_log
   WHERE cache_valid = 0
     AND executed_at < datetime('now', '-30 days');
   ```

3. **VACUUM database occasionally:**
   ```sql
   -- Reclaim space after deleting logs
   VACUUM;
   ```

4. **Monitor database size:**
   ```bash
   ls -lh delphi_symbols.db*
   # Check .db, .db-wal, .db-shm files
   ```

## Related Documentation

- **[USER-GUIDE.md](USER-GUIDE.md)** - How to use the tools
- **[DATABASE-SCHEMA.md](DATABASE-SCHEMA.md)** - Complete database schema
- **[QUERY-ANALYTICS.md](QUERY-ANALYTICS.md)** - Analytics SQL queries
- **[TESTS.md](TESTS.md)** - Testing guide

---

**Version**: 1.0
**Target**: AI Coding Agents (maintenance, extension, troubleshooting)
