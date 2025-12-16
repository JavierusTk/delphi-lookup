# delphi-lookup Database Schema - delphi_symbols.db

**Database**: SQLite 3 with `sqlite-vec` extension
**File**: `delphi_symbols.db`
**Location**: Project root

---

## Table Structure

### 1. `metadata` - Self-Describing Database Configuration

**Purpose**: Stores database configuration and model information

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS metadata (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
)
```

**Sample Data**:
```
key                    | value
-----------------------|--------------------------------
embedding_model        | jina-code-embed-1-5b
embedding_dimensions   | 1536
ollama_url             | http://127.0.0.1:11434
indexed_date           | 2025-11-05 19:57:14
schema_version         | 1
```

**Usage**:
- Validates compatibility when adding new folders
- Prevents mixing incompatible embedding models
- Documents database creation parameters

---

### 2. `indexed_folders` - Per-Folder Tracking

**Purpose**: Tracks which folders have been indexed and when

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS indexed_folders (
  folder_path TEXT PRIMARY KEY,
  indexed_at TIMESTAMP NOT NULL,
  duration_seconds INTEGER NOT NULL,
  files_count INTEGER NOT NULL,
  chunks_count INTEGER NOT NULL
)
```

**Sample Data**:
```
folder_path              | indexed_at          | duration_seconds | files_count | chunks_count
-------------------------|---------------------|------------------|-------------|-------------
C:\Projects\MyFramework  | 2025-11-05 19:57:14 | 1811             | 405         | 20446
C:\Projects\MyApp        | 2025-11-05 21:15:30 | 2450             | 620         | 28500
C:\Projects\Utils        | 2025-11-05 22:45:00 | 1950             | 380         | 15200
```

**Queries**:
```sql
-- List all indexed folders
SELECT * FROM indexed_folders ORDER BY indexed_at DESC;

-- Total chunks across all folders
SELECT SUM(chunks_count) as total_chunks, SUM(files_count) as total_files FROM indexed_folders;

-- Performance statistics
SELECT folder_path,
       files_count,
       chunks_count,
       duration_seconds,
       ROUND(CAST(chunks_count AS FLOAT) / duration_seconds, 2) as chunks_per_second
FROM indexed_folders
ORDER BY chunks_per_second DESC;
```

---

### 3. `symbols` - Main Code Chunks Table

**Purpose**: Stores extracted Pascal code chunks (classes, functions, procedures, etc.)

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS symbols (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  full_name TEXT,
  type TEXT NOT NULL,
  file_path TEXT NOT NULL,
  content TEXT NOT NULL,
  comments TEXT,
  parent_class TEXT,
  implemented_interfaces TEXT,
  visibility TEXT,
  start_line INTEGER,
  end_line INTEGER,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  UNIQUE(name, file_path, type)
)
```

**Field Descriptions**:
- `id`: Auto-increment primary key
- `name`: Symbol name (e.g., `TTableMAX`, `ProcessFiles`, `DoBeforePost`)
- `full_name`: Fully qualified name (e.g., `TTableMAX.DoBeforePost`)
- `type`: Symbol type (`class`, `interface`, `record`, `procedure`, `function`, `constructor`, `destructor`, `property`, `type`, `const`, `var`)
- `file_path`: Full Windows path (e.g., `C:\Projects\MyLib\Db\TableMAX.pas`)
- `content`: Complete source code for the symbol
- `comments`: Extracted documentation comments
- `parent_class`: For methods/properties, the containing class (e.g., `TTableMAX`)
- `implemented_interfaces`: Comma-separated list of interfaces
- `visibility`: `public`, `private`, `protected`, `published`
- `start_line`, `end_line`: Source location
- `created_at`: Timestamp when symbol was indexed

**UNIQUE Constraint**: `(name, file_path, type)`
- **Critical for incremental updates**: Prevents duplicates
- **INSERT OR REPLACE behavior**: Updates existing symbols during reindexing
- **Example**: `TMyClass` in `C:\Projects\MyLib\Utils\MyUnit.pas` with type `class` can only appear once

**Sample Data**:
```
id    | name         | type      | file_path                          | parent_class | visibility
------|--------------|-----------|------------------------------------|--------------|-----------
1     | TMyClass     | class     | C:\Projects\MyLib\Core.pas         | NULL         | public
2     | DoProcess    | procedure | C:\Projects\MyLib\Core.pas         | TMyClass     | protected
3     | TMyForm      | class     | C:\Projects\MyLib\Forms\Main.pas   | NULL         | public
...   | ...          | ...       | ...                                | ...          | ...
```

**Statistics** (example - 20,446 total symbols):
```
type        | count  | unique_files
------------|--------|-------------
procedure   | 8,542  | 325
function    | 5,218  | 310
class       | 2,891  | 285
type        | 1,652  | 198
property    | 1,423  | 145
interface   | 580    | 92
constructor | 140    | 68
```

**Queries**:
```sql
-- Find all symbols in a specific file
SELECT name, type, parent_class, visibility
FROM symbols
WHERE file_path = 'C:\Projects\MyLib\Core.pas'
ORDER BY start_line;

-- Find all classes
SELECT name, file_path, comments
FROM symbols
WHERE type = 'class'
ORDER BY name;

-- Find all methods of a class
SELECT name, type, visibility, start_line
FROM symbols
WHERE parent_class = 'TTableMAX'
ORDER BY start_line;

-- Search by name (exact match)
SELECT name, type, file_path, LEFT(content, 200) as preview
FROM symbols
WHERE name = 'TTableMAX';

-- Search by name pattern (fuzzy)
SELECT name, type, file_path
FROM symbols
WHERE name LIKE '%TableMAX%'
ORDER BY name;

-- Count symbols per folder
SELECT
  CASE
    WHEN file_path LIKE 'C:\Projects\MyFramework%' THEN 'MyFramework'
    WHEN file_path LIKE 'C:\Projects\MyApp%' THEN 'MyApp'
    ELSE 'Other'
  END as folder,
  COUNT(*) as symbol_count
FROM symbols
GROUP BY folder;
```

---

### 4. `vec_embeddings` - Vector Embeddings (sqlite-vec Virtual Table)

**Purpose**: Stores 1536-dimensional vector embeddings for semantic search

**Schema**:
```sql
CREATE VIRTUAL TABLE IF NOT EXISTS vec_embeddings USING vec0(
  symbol_id INTEGER,
  chunk_id TEXT,
  embedding_text TEXT,
  embedding FLOAT[1536]
)
```

**Field Descriptions**:
- `symbol_id`: Foreign key to `symbols.id` (links embedding to symbol)
- `chunk_id`: Unique identifier for this chunk (UUID-like)
- `embedding_text`: Original text that was embedded (for reference)
- `embedding`: 1536-dimensional float vector (generated by `jina-code-embed-1-5b`)

**Virtual Table Properties**:
- **Managed by sqlite-vec extension** (vec0.dll)
- **No traditional indexes** - uses specialized vector similarity structures
- **Distance metric**: L2 (Euclidean) distance
- **Similarity formula**: `Similarity = 1.0 - Distance`

**Sample Data** (conceptual - binary data):
```
symbol_id | chunk_id                              | embedding_text          | embedding (1536 floats)
----------|---------------------------------------|-------------------------|-------------------------
1         | 7f3a4b2e-91c5-4d6a-8a2f-1e5c9d7b8a3c | class TTableMax(...     | [0.023, -0.145, 0.892, ...]
2         | 8e4b5c3f-a2d6-5e7b-9b3g-2f6d0e8c9b4d | procedure DoBeforePost  | [-0.012, 0.234, -0.567, ...]
```

**Vector Search Query**:
```sql
-- Find semantically similar code chunks
-- (Note: actual syntax varies, shown for illustration)
SELECT
  v.symbol_id,
  s.name,
  s.type,
  s.file_path,
  vec_distance_l2(v.embedding, :query_embedding) as distance,
  1.0 - vec_distance_l2(v.embedding, :query_embedding) as similarity
FROM vec_embeddings v
JOIN symbols s ON v.symbol_id = s.id
WHERE vec_distance_l2(v.embedding, :query_embedding) < 1.5
ORDER BY distance
LIMIT 10;
```

**Storage**:
- Each embedding: 1536 floats × 4 bytes = 6,144 bytes
- 20,446 embeddings × 6,144 bytes ≈ 125 MB (just vectors)
- Actual storage larger due to indexing structures

---

### 5. `symbols_fts` - Full-Text Search Index (FTS5 Virtual Table)

**Purpose**: Provides fast full-text search on symbol names, content, and comments

**Schema**:
```sql
CREATE VIRTUAL TABLE IF NOT EXISTS symbols_fts USING fts5(
  name,
  full_name,
  content,
  comments,
  content='symbols',    -- Links to symbols table
  content_rowid='id'    -- Uses symbols.id as rowid
)
```

**Usage**:
- **Content search**: Find symbols by text in their code content
- **Comment search**: Find symbols by documentation comments
- **Name search**: Full-text matching on symbol names

**Queries**:
```sql
-- Full-text search for "database connection"
SELECT s.name, s.type, s.file_path
FROM symbols_fts fts
JOIN symbols s ON fts.rowid = s.id
WHERE symbols_fts MATCH 'database connection'
ORDER BY rank;

-- Search in comments only
SELECT s.name, s.type, s.comments
FROM symbols_fts fts
JOIN symbols s ON fts.rowid = s.id
WHERE symbols_fts MATCH 'comments:transaction'
ORDER BY rank;
```

**Note**: FTS5 might not be available in all SQLite builds. The system gracefully degrades to LIKE searches if FTS5 is unavailable.

---

### 6. `search_cache` - Query Cache (Optimization)

**Purpose**: Caches common search queries for performance

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS search_cache (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  query_text TEXT NOT NULL,
  query_hash TEXT NOT NULL,
  result_ids TEXT NOT NULL,
  hit_count INTEGER DEFAULT 1,
  last_used TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  UNIQUE(query_hash)
)
```

**Field Descriptions**:
- `query_text`: Original search query
- `query_hash`: Hash of query for fast lookup
- `result_ids`: Comma-separated list of matching symbol IDs
- `hit_count`: Number of times this query was executed
- `last_used`: Last cache hit timestamp

**Usage**: (Note: Currently not actively used in delphi-lookup)
- Cache warm-up for common queries
- Performance monitoring
- Query statistics

---

## Indexes

### Primary Indexes on `symbols` table:
```sql
CREATE INDEX IF NOT EXISTS idx_symbols_name ON symbols(name);
CREATE INDEX IF NOT EXISTS idx_symbols_type ON symbols(type);
CREATE INDEX IF NOT EXISTS idx_symbols_file ON symbols(file_path);
CREATE INDEX IF NOT EXISTS idx_symbols_parent ON symbols(parent_class);
```

**Purpose**:
- `idx_symbols_name`: Fast exact name lookups
- `idx_symbols_type`: Filter by symbol type (class, function, etc.)
- `idx_symbols_file`: Find all symbols in a file
- `idx_symbols_parent`: Find all methods/properties of a class

### Indexes on `search_cache`:
```sql
CREATE INDEX IF NOT EXISTS idx_cache_hash ON search_cache(query_hash);
CREATE INDEX IF NOT EXISTS idx_cache_used ON search_cache(last_used);
```

---

## Database Relationships

```
metadata
  ↓ (configuration)
indexed_folders ←→ symbols (via file_path pattern)
  ↓                   ↓
  └─────────────→ (folder tracking)
                    ↓
                symbols.id ←→ vec_embeddings.symbol_id (1:1 relationship)
                    ↓
                symbols.id ←→ symbols_fts.rowid (1:1 relationship)
```

**Key Relationships**:
1. Each **folder** in `indexed_folders` contains many **symbols**
2. Each **symbol** has at most one **vector embedding**
3. Each **symbol** has at most one **FTS entry**
4. **Metadata** describes the entire database (no foreign keys)

---

## Data Lifecycle

### Initial Indexing (Fresh Database)
1. Database file created (empty)
2. Tables created (`metadata`, `indexed_folders`, `symbols`, `vec_embeddings`, `symbols_fts`, `search_cache`)
3. Metadata stored (model, dimensions, URL, schema version)
4. For each Pascal file:
   - Extract code chunks
   - Generate embeddings via Ollama
   - INSERT symbols (with UNIQUE constraint)
   - INSERT vec_embeddings (linked by symbol_id)
5. Create indexes
6. Populate FTS table (bulk insert from symbols)
7. ANALYZE for query optimization
8. Record folder stats in `indexed_folders`

### Incremental Update (Folder Reindex)
1. Database file exists
2. Validate metadata compatibility (model, dimensions)
3. **Remove old folder data**:
   - DELETE FROM vec_embeddings WHERE symbol_id IN (SELECT id FROM symbols WHERE file_path LIKE 'C:\folder%')
   - DELETE FROM symbols WHERE file_path LIKE 'C:\folder%'
   - DELETE FROM symbols_fts WHERE rowid IN (old symbol ids)
   - DELETE FROM indexed_folders WHERE folder_path = 'C:\folder'
4. Index folder (same as initial indexing)
5. Update FTS incrementally (INSERT new symbols)
6. ANALYZE for query optimization
7. Update folder stats in `indexed_folders`

---

## Example Database Statistics

**Tables** (example with ~20,000 symbols):
- `metadata`: 5 rows
- `indexed_folders`: N rows (one per indexed folder)
- `symbols`: ~20,000 rows
- `vec_embeddings`: ~20,000 rows (1 per symbol, if embeddings enabled)
- `symbols_fts`: ~20,000 rows (mirrors symbols)
- `search_cache`: N rows

**Breakdown by Type** (typical distribution):
```
Type          Count    Percentage
-----------   ------   ----------
procedure     ~40%     (most common)
function      ~25%
class         ~14%
type          ~8%
property      ~7%
interface     ~3%
constructor   ~1%
```

**Storage Breakdown** (example ~300MB total):
- Symbols (text): ~55%
- Vector embeddings: ~40%
- FTS index: ~4%
- Indexes + overhead: ~1%

---

## Query Performance

### Exact Name Lookup (Indexed)
```sql
SELECT * FROM symbols WHERE name = 'TTableMAX';
```
**Performance**: <10ms (index scan)

### Fuzzy Name Search
```sql
SELECT * FROM symbols WHERE name LIKE '%TableMAX%';
```
**Performance**: 50-200ms (index range scan)

### Vector Similarity Search
```sql
-- Via delphi-lookup with max_distance=1.5
SELECT ... FROM vec_embeddings WHERE vec_distance_l2(...) < 1.5 LIMIT 10;
```
**Performance**: 200-800ms (depends on dataset size, ~40ms per 1000 chunks)

### Full-Text Search
```sql
SELECT * FROM symbols_fts WHERE symbols_fts MATCH 'database connection';
```
**Performance**: 100-300ms (FTS5 optimized)

### Hybrid Search (delphi-lookup)
**Combines**: Exact + Fuzzy + Vector + FTS
**Performance**: 600-2100ms (dominated by vector search)

---

## Space Estimation for Multiple Folders

**Rule of thumb**: ~16KB per symbol (including embeddings)

### Example Estimation
```
Folder              Files    Symbols    Estimated Size
------------------  -----    -------    --------------
MyFramework         400      20,000     320 MB
MyApplication       600      28,000     450 MB
Utils               180      8,000      130 MB
ThirdParty          400      15,000     240 MB
Delphi RTL/VCL      600      26,000     420 MB
------------------  -----    -------    --------------
TOTAL               2,180    97,000     ~1.5 GB
```

**Note**: Actual size varies based on code complexity and comment density.

---

## Schema Version History

### Version 1 (Current)
- Initial schema with all tables
- sqlite-vec integration (1536 dimensions)
- FTS5 optional support
- Folder tracking infrastructure

### Future Versions (Planned)
- Version 2: Add file modification timestamps for incremental file-level updates
- Version 3: Add cross-reference tables for symbol dependencies
- Version 4: Add language-agnostic support (Python, TypeScript, etc.)

---

## Maintenance Queries

### Database Health Check
```sql
-- Check for orphaned embeddings (should be 0)
SELECT COUNT(*) FROM vec_embeddings v
WHERE NOT EXISTS (SELECT 1 FROM symbols WHERE id = v.symbol_id);

-- Check for missing embeddings (should be 0 for full indexing)
SELECT COUNT(*) FROM symbols s
WHERE NOT EXISTS (SELECT 1 FROM vec_embeddings WHERE symbol_id = s.id);

-- Check folder consistency
SELECT
  f.folder_path,
  f.chunks_count as tracked_count,
  (SELECT COUNT(*) FROM symbols WHERE file_path LIKE f.folder_path || '%') as actual_count,
  f.chunks_count - (SELECT COUNT(*) FROM symbols WHERE file_path LIKE f.folder_path || '%') as difference
FROM indexed_folders f;
```

### Database Optimization
```sql
-- Update query planner statistics
ANALYZE;

-- Reclaim unused space (after large deletions)
VACUUM;

-- Check database integrity
PRAGMA integrity_check;

-- Show database size
PRAGMA page_count;
PRAGMA page_size;
```

### Folder Management
```sql
-- List all indexed folders with stats
SELECT * FROM indexed_folders ORDER BY indexed_at DESC;

-- Remove specific folder data
DELETE FROM vec_embeddings
WHERE symbol_id IN (SELECT id FROM symbols WHERE file_path LIKE 'C:\Projects\MyLib%');

DELETE FROM symbols WHERE file_path LIKE 'C:\Projects\MyLib%';

DELETE FROM indexed_folders WHERE folder_path = 'C:\Projects\MyLib';

-- Verify removal
SELECT folder_path, COUNT(*) as remaining_symbols
FROM indexed_folders f
LEFT JOIN symbols s ON s.file_path LIKE f.folder_path || '%'
GROUP BY f.folder_path;
```

---

## Backup and Recovery

### Backup Database
```bash
# Simple file copy (SQLite is file-based)
cp delphi_symbols.db delphi_symbols.db.backup

# SQLite backup command
sqlite3 delphi_symbols.db ".backup delphi_symbols.db.backup"
```

### Restore Database
```bash
cp delphi_symbols.db.backup delphi_symbols.db
```

### Export Schema Only
```bash
sqlite3 delphi_symbols.db ".schema" > schema.sql
```

### Export Data (CSV)
```bash
sqlite3 delphi_symbols.db <<EOF
.headers on
.mode csv
.output symbols_export.csv
SELECT * FROM symbols;
.quit
EOF
```

---

**Database Version**: 1
