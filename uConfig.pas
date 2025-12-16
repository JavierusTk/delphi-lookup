unit uConfig;

interface

const
  // Database file (default, can be overridden by config/command line)
  DEFAULT_DB_FILE = 'delphi_symbols.db';

  // Embedding configuration
  // embedding_url empty = no embeddings (FTS5 only)
  DEFAULT_EMBEDDING_PROVIDER = 'ollama';
  DEFAULT_EMBEDDING_MODEL = 'jina-code-embed-1-5b';
  DEFAULT_OLLAMA_PORT = 11434;  // Used when URL has no port and provider=ollama

  // Note: Embedding dimensions are auto-detected at runtime by generating
  // a test embedding. This allows switching between different models without
  // code changes. The detected dimensions are stored in the database metadata
  // and used to create the appropriate vec0 virtual table schema.

  // Valid content type values
  CONTENT_TYPE_CODE = 'code';
  CONTENT_TYPE_HELP = 'help';
  CONTENT_TYPE_MARKDOWN = 'markdown';
  CONTENT_TYPE_COMMENT = 'comment';

  // Valid source category values
  SOURCE_CATEGORY_USER = 'user';
  SOURCE_CATEGORY_STDLIB = 'stdlib';
  SOURCE_CATEGORY_THIRD_PARTY = 'third_party';
  SOURCE_CATEGORY_OFFICIAL_HELP = 'official_help';
  SOURCE_CATEGORY_PROJECT_DOCS = 'project_docs';

  /////// INDEXING ONLY
  DEFAULT_BATCH_SIZE = 100;
  DEFAULT_BUFFER_SIZE = 500;

  // Source classification defaults
  DEFAULT_CONTENT_TYPE = 'code';
  DEFAULT_SOURCE_CATEGORY = 'user';

  /////// LOOKUP ONLY
  // Vector search configuration
  // Note: The threshold is based on maximum distance. Lower distance = more similar.
  // Distance is converted to similarity: Similarity = 1.0 - Distance
  // Examples: distance 0.0 = similarity 1.0 (identical)
  //           distance 0.3 = similarity 0.7 (quite similar)
  //           distance 1.5 = similarity -0.5 (semantically related)
  // Optimal values by dataset size:
  //   - Small (1k-10k chunks): 1.5-2.0
  //   - Medium (10k-100k chunks): 0.5-1.0
  //   - Large (100k+ chunks): 0.3-0.5
  DEFAULT_MAX_DISTANCE = 1.5;  // Default maximum vector distance (range: 0.1-10.0)
  MIN_MAX_DISTANCE = 0.1;      // Minimum allowed max distance
  MAX_MAX_DISTANCE = 10.0;     // Maximum allowed max distance

  DEFAULT_NUM_RESULTS = 5;
  MAX_NUM_RESULTS = 100;

  // Reranker configuration
  // reranker_url empty = reranker not available
  DEFAULT_RERANKER_PROVIDER = 'custom';
  DEFAULT_RERANKER_CANDIDATE_COUNT = 50;  // Top N candidates to send for reranking
  DEFAULT_RERANKER_TIMEOUT = 5000;        // 5 seconds

/// <summary>Get embedding URL from environment variable (EMBEDDING_URL or OLLAMA_URL)</summary>
/// <returns>URL from environment or empty string if not set</returns>
function GetEmbeddingURLFromEnv: string;

/// <summary>Get Reranker URL from environment variable (RERANKER_URL)</summary>
/// <returns>URL from environment or empty string if not set</returns>
function GetRerankerURLFromEnv: string;

/// <summary>Normalize embedding URL: add default port for Ollama if missing</summary>
function NormalizeEmbeddingURL(const AURL, AProvider: string): string;

implementation

uses
  System.SysUtils, System.RegularExpressions;

function GetEmbeddingURLFromEnv: string;
begin
  // Check EMBEDDING_URL first, then fallback to OLLAMA_URL for compatibility
  Result := GetEnvironmentVariable('EMBEDDING_URL');
  if Result = '' then
    Result := GetEnvironmentVariable('OLLAMA_URL');
end;

function GetRerankerURLFromEnv: string;
begin
  Result := GetEnvironmentVariable('RERANKER_URL');
end;

function NormalizeEmbeddingURL(const AURL, AProvider: string): string;
var
  HasPort: Boolean;
begin
  Result := AURL;

  if Result = '' then
    Exit;

  // Check if URL already has a port (look for :number after host)
  // Pattern: ://host:port or ://host/ or ://host (end of string)
  HasPort := TRegEx.IsMatch(Result, '://[^/]+:\d+');

  // If provider is Ollama and no port specified, add default port
  if (LowerCase(AProvider) = 'ollama') and not HasPort then
  begin
    // Remove trailing slash if present
    if Result.EndsWith('/') then
      Result := Copy(Result, 1, Length(Result) - 1);

    Result := Result + ':' + IntToStr(DEFAULT_OLLAMA_PORT);
  end;
end;

end.
