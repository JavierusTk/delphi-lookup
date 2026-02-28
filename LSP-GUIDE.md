# LSP Server Technical Guide

Technical documentation for the delphi-lsp-server — a Language Server Protocol server that provides Delphi/Pascal code intelligence to Claude Code and other LSP-compatible editors.

## Overview

The delphi-lsp-server bridges Claude Code's LSP integration with delphi-lookup's FTS5 symbol index. Instead of grep-based text search (~30-60s), Claude Code gets semantic code navigation (~50ms) with 100% accuracy for identifier lookups.

```
Claude Code ──JSON-RPC 2.0 (stdio)──► delphi-lsp-server ──SQL──► delphi_symbols.db
                                              │
                                       TQueryProcessor
                                       (same engine as
                                        delphi-lookup.exe)
```

## LSP Capabilities

### Supported Methods

| Method | Description | How It Works |
|--------|-------------|--------------|
| `initialize` | Server handshake | Advertises capabilities, connects to database |
| `initialized` | Post-init notification | Marks server as ready |
| `shutdown` / `exit` | Graceful termination | Closes DB connection, exits |
| `textDocument/definition` | Go to definition | Extracts identifier at cursor, searches index for definition |
| `textDocument/references` | Find all references | Searches all symbol occurrences (up to 100) |
| `textDocument/hover` | Hover information | Returns declaration + comments + file info as markdown |
| `textDocument/documentSymbol` | Symbols in file | Lists all indexed symbols for a given file path |
| `workspace/symbol` | Global symbol search | Runs hybrid search (exact → fuzzy → FTS5) |

### Document Synchronization

The server acknowledges but does not process:
- `textDocument/didOpen`
- `textDocument/didChange`
- `textDocument/didClose`
- `textDocument/didSave`

The server is **index-based** — it serves results from the pre-built `delphi_symbols.db` rather than parsing files on-the-fly. Run `delphi-indexer.exe` to update the index when source files change.

### Capabilities Advertised

```json
{
  "capabilities": {
    "textDocumentSync": 0,
    "definitionProvider": true,
    "referencesProvider": true,
    "hoverProvider": true,
    "documentSymbolProvider": true,
    "workspaceSymbolProvider": true
  },
  "serverInfo": {
    "name": "delphi-lsp-server",
    "version": "1.1.0"
  }
}
```

`textDocumentSync: 0` means "None" — the server does not need file change notifications since it works from the pre-built index.

## Architecture

### Source Files

| File | Lines | Purpose |
|------|-------|---------|
| `delphi-lsp-server.dpr` | 124 | Entry point — parses args, creates server, runs message loop |
| `LSP/uLSPTypes.pas` | 321 | LSP type definitions (Position, Range, Location, SymbolKind, etc.) |
| `LSP/uLSPProtocol.pas` | 393 | JSON-RPC 2.0 protocol handler (stdin/stdout, Content-Length headers) |
| `LSP/uLSPHandlers.pas` | 546 | Method dispatch and handler implementations |
| `LSP/uPositionResolver.pas` | 229 | Cursor position → Pascal identifier extraction |

### Request Flow

```
1. Claude Code sends JSON-RPC request via stdin
   ┌─────────────────────────────────────────────┐
   │ Content-Length: 123\r\n                      │
   │ \r\n                                         │
   │ {"jsonrpc":"2.0","id":1,                     │
   │  "method":"textDocument/definition",         │
   │  "params":{...}}                             │
   └─────────────────────────────────────────────┘

2. TLSPProtocol.ReadMessage parses headers + JSON body

3. TLSPServer.DispatchMethod routes to handler

4. Handler (e.g., HandleTextDocumentDefinition):
   a. Extract URI + position from params
   b. Read file content from disk
   c. TPositionResolver.GetIdentifierAtPosition → symbol name
   d. TQueryProcessor.FindSymbolDefinition → TSearchResult
   e. Convert to LSP Location JSON (0-indexed)

5. TLSPProtocol.WriteResponse sends result via stdout
   ┌─────────────────────────────────────────────┐
   │ Content-Length: 156\r\n                      │
   │ \r\n                                         │
   │ {"jsonrpc":"2.0","id":1,                     │
   │  "result":{"uri":"file:///...","range":{...}}}│
   └─────────────────────────────────────────────┘
```

### Position Resolution

`TPositionResolver` handles the critical step of converting an LSP cursor position (line:column) to a Pascal identifier string:

1. Extract the line at the given 0-indexed line number
2. Find identifier character boundaries at the column position
3. Handle qualified names (`TMyClass.MyMethod`) by extending across dots
4. Return the identifier text for database lookup

Character rules for Pascal identifiers:
- Start: letter or `_`
- Continue: letter, digit, or `_`

### Symbol Type Mapping

Pascal symbol types from the database are mapped to LSP SymbolKind:

| Database Type | LSP SymbolKind | Value |
|---------------|----------------|-------|
| `class` | Class | 5 |
| `interface` | Interface | 11 |
| `record` | Struct | 23 |
| `function` | Function | 12 |
| `procedure` | Method | 6 |
| `constructor` | Constructor | 9 |
| `destructor` | Method | 6 |
| `property` | Property | 7 |
| `const` | Constant | 14 |
| `var` | Variable | 13 |
| `type` | Class | 5 |
| `enum` | Enum | 10 |
| `field` | Field | 8 |
| `unit` | Module | 2 |

### URI Conversion

The server handles bidirectional conversion between Windows paths and `file://` URIs:

- `C:\Projects\MyApp\Unit1.pas` → `file:///C:/Projects/MyApp/Unit1.pas`
- `file:///C:/Projects/MyApp/Unit1.pas` → `C:\Projects\MyApp\Unit1.pas`

Special characters are URL-encoded/decoded as needed.

## Configuration

### Command-Line Arguments

```
delphi-lsp-server.exe [options]

Options:
  -d, --database <path>  Path to delphi_symbols.db
  -l, --log <path>       Enable logging to file
  -h, --help             Show help
```

### Environment Variables

| Variable | Description |
|----------|-------------|
| `DELPHI_LSP_DATABASE` | Path to `delphi_symbols.db` |
| `DELPHI_LSP_LOG` | Path to log file (enables debugging) |

### Database Discovery Order

1. `--database` command-line argument
2. `DELPHI_LSP_DATABASE` environment variable
3. `delphi_symbols.db` in the same directory as the exe

## Plugin Configuration

### .lsp.json

The `.lsp.json` file at the repository root configures how Claude Code launches the server:

```json
{
  "delphi": {
    "command": "delphi-lsp-server",
    "args": [],
    "extensionToLanguage": {
      ".pas": "pascal",
      ".dpr": "pascal",
      ".dpk": "pascal",
      ".inc": "pascal"
    },
    "transport": "stdio",
    "env": {
      "DELPHI_LSP_DATABASE": "",
      "DELPHI_LSP_LOG": ""
    },
    "startupTimeout": 30000,
    "shutdownTimeout": 5000,
    "restartOnCrash": true,
    "maxRestarts": 3
  }
}
```

### Configuration Fields

| Field | Required | Default | Description |
|-------|----------|---------|-------------|
| `command` | Yes | — | LSP binary name (must be in PATH) |
| `args` | No | `[]` | Command-line arguments |
| `extensionToLanguage` | Yes | — | Maps file extensions to language IDs |
| `transport` | No | `stdio` | Communication: `stdio` or `socket` |
| `env` | No | `{}` | Environment variables for the server process |
| `startupTimeout` | No | — | Max wait for initialization (ms) |
| `shutdownTimeout` | No | — | Max wait for graceful shutdown (ms) |
| `restartOnCrash` | No | `false` | Auto-restart on crash |
| `maxRestarts` | No | — | Max restart attempts |

### Customizing for Your Environment

To point to a specific database, edit `.lsp.json`:

```json
{
  "delphi": {
    "command": "delphi-lsp-server",
    "args": ["--database", "W:\\Projects\\delphi_symbols.db"],
    "extensionToLanguage": {
      ".pas": "pascal",
      ".dpr": "pascal",
      ".dpk": "pascal",
      ".inc": "pascal"
    }
  }
}
```

To enable debug logging:

```json
{
  "delphi": {
    "command": "delphi-lsp-server",
    "args": ["--log", "/tmp/delphi-lsp.log"],
    "extensionToLanguage": {
      ".pas": "pascal",
      ".dpr": "pascal",
      ".dpk": "pascal",
      ".inc": "pascal"
    }
  }
}
```

## Building

### Requirements

- RAD Studio 12 (Delphi)
- Win64 target (enforced by compiler directive)
- Dependencies: FireDAC, mORMot 2, DelphiAST (same as delphi-lookup)

### Compile

```
Open delphi-lsp-server.dproj in RAD Studio
Build > Win64 > Release
```

Output: `Win64\Release\delphi-lsp-server.exe`

### Runtime Dependencies

Place alongside the exe or in PATH:
- `sqlite3.dll` (FTS5-enabled, from `bin/`)
- `vec0.dll` (sqlite-vec, from `bin/`)

## Logging

When logging is enabled (via `--log` or `DELPHI_LSP_LOG`), the server writes:

```
2026-02-28 10:15:32.456 LSP Protocol logging started
2026-02-28 10:15:32.458 Headers read: Content-Length: 1234
2026-02-28 10:15:32.459 Content read (1234 bytes): {"jsonrpc":"2.0",...
2026-02-28 10:15:32.460 Parsed message: method=initialize, id=0
2026-02-28 10:15:32.461 Sending: {"jsonrpc":"2.0","id":0,"result":{...}}
```

## Performance

| Operation | Latency | Notes |
|-----------|---------|-------|
| Server startup | ~65ms | Process creation + DB connection + FTS5 detection |
| `goToDefinition` (exact match) | ~12ms | COLLATE NOCASE index, short-circuit |
| `findReferences` | ~12-50ms | Depends on number of matches |
| `hover` | ~12ms | Same as definition + markdown formatting |
| `workspaceSymbol` | ~12-1700ms | Depends on query type (identifier vs conceptual) |
| Cached query | ~10ms | All query types |

End-to-end from Claude Code's perspective: ~50-75ms (includes JSON-RPC overhead).

## Limitations

- **Index-based**: Results reflect the last `delphi-indexer` run, not live file edits
- **No incremental sync**: File changes are not tracked in real-time
- **Windows only**: The server requires Win64 compilation
- **No diagnostics push**: Unlike full language servers (Pyright, gopls), this server does not push diagnostics after edits — it provides navigation and lookup only
- **No completion**: Code completion is not implemented (delphi-lookup focuses on symbol search)

## Future Enhancements

Potential additions based on Claude Code's LSP capabilities:

- `textDocument/implementation` — find classes implementing an interface (data exists: `is_inherited`, `inherited_from` fields)
- `callHierarchy/incomingCalls` — what calls a function
- `callHierarchy/outgoingCalls` — what a function calls
- `textDocument/publishDiagnostics` — push errors after edits (would require file watching)
- `textDocument/completion` — symbol completion suggestions
