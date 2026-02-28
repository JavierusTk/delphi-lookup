# Claude Code Setup for delphi-lookup

This guide configures Claude Code to use delphi-lookup for Delphi/Pascal code intelligence — both via the **LSP plugin** (recommended) and the **CLI skill** (fallback).

## What You Get

| Feature | Without LSP | With LSP |
|---------|------------|----------|
| "Where is TMyClass defined?" | Grep: 30-60s, fuzzy | LSP goToDefinition: ~50ms, exact |
| "Find all usages of ProcessOrder" | Grep: misses call sites | LSP findReferences: all usages |
| "What type is FConnection?" | Read file manually | LSP hover: type + docs instant |
| Error detection after edits | Manual compile | Automatic diagnostics |

## Prerequisites

- **Claude Code** 2.0.74 or later (`claude --version`)
- **delphi-lsp-server.exe** compiled and in PATH (see [Building the LSP Server](#building-the-lsp-server))
- **delphi_symbols.db** built with delphi-indexer (see [Building Your Index](#building-your-index))

## Quick Setup (2 minutes)

### Step 1: Enable LSP Tool

Add to `~/.claude/settings.json`:

```json
{
  "ENABLE_LSP_TOOL": "1"
}
```

Also add to your shell profile (`~/.bashrc` or `~/.zshrc`) as fallback:

```bash
export ENABLE_LSP_TOOL=1
```

> **Note**: `ENABLE_LSP_TOOL` was discovered via [GitHub Issue #15619](https://github.com/anthropics/claude-code/issues/15619). It may change or become unnecessary in future versions.

### Step 2: Install the Plugin

**Option A: From this repository (recommended)**

```bash
claude --plugin-dir /path/to/delphi-lookup
```

This loads the plugin for the current session. The LSP server starts automatically for any `.pas`, `.dpr`, `.dpk`, or `.inc` files.

**Option B: Install to user scope (persistent)**

```bash
# If published to a marketplace:
claude plugin install delphi-lsp

# Or install from local directory:
claude plugin install --scope user /path/to/delphi-lookup
```

### Step 3: Configure Database Path

The LSP server auto-discovers `delphi_symbols.db` next to the exe. To override, set the environment variable:

```bash
export DELPHI_LSP_DATABASE="/path/to/delphi_symbols.db"
```

Or edit `.lsp.json` in the plugin root:

```json
{
  "delphi": {
    "command": "delphi-lsp-server",
    "args": ["--database", "/path/to/delphi_symbols.db"],
    "extensionToLanguage": {
      ".pas": "pascal",
      ".dpr": "pascal",
      ".dpk": "pascal",
      ".inc": "pascal"
    }
  }
}
```

### Step 4: Restart Claude Code

LSP servers initialize at startup. After installing, restart Claude Code completely.

### Step 5: Verify

Ask Claude Code:

```
Where is TStringList defined?
```

If it uses the LSP `goToDefinition` operation instead of grepping files, you're good.

You can also press **Ctrl+O** to see diagnostics pushed by LSP servers.

## Verify Plugin Status

```bash
# Check plugin is installed and enabled
claude plugin list

# Look for:
#   delphi-lsp  Status: enabled
```

> **The #1 gotcha**: A plugin can be installed but _disabled_. If `claude plugin list` shows `Status: disabled`, run:
> ```bash
> claude plugin enable delphi-lsp
> ```
> Then restart Claude Code.

To be safe, also set in `~/.claude/settings.json`:

```json
{
  "ENABLE_LSP_TOOL": "1",
  "enabledPlugins": {
    "delphi-lsp": true
  }
}
```

## Building the LSP Server

The LSP server is a Delphi console application that communicates via JSON-RPC 2.0 over stdio.

### Compile

Open `delphi-lsp-server.dproj` in RAD Studio 12 and build for Win64:

```
Platform: Windows 64-bit
Configuration: Release
```

### Install

Place `delphi-lsp-server.exe` in your PATH:

```bash
# Option 1: Copy to a PATH directory
copy delphi-lsp-server.exe C:\Tools\

# Option 2: Add the build output to PATH
set PATH=%PATH%;C:\Projects\delphi-lookup\Win64\Release
```

Verify: `delphi-lsp-server --help`

### Required DLLs

The following must be in the same directory as the exe or in PATH:

- `sqlite3.dll` (FTS5-enabled, from `bin/`)
- `vec0.dll` (sqlite-vec extension, from `bin/`)

## Building Your Index

Before the LSP server can provide results, you need an indexed database:

```bash
# Index your project
delphi-indexer.exe "C:\YourProject\src" --category user

# Index Delphi standard library (recommended)
delphi-indexer.exe "C:\Program Files (x86)\Embarcadero\Studio\23.0\source\rtl" --category stdlib
delphi-indexer.exe "C:\Program Files (x86)\Embarcadero\Studio\23.0\source\vcl" --category stdlib --framework VCL

# Index third-party libraries
delphi-indexer.exe "C:\ThirdParty\mORMot2\src" --category third_party --framework RTL
```

## Using LSP in Practice

You don't need special commands. Just talk to Claude Code naturally:

| You say... | Claude uses... |
|-----------|---------------|
| "Where is ProcessOrder defined?" | `goToDefinition` |
| "Find all usages of TUserService" | `findReferences` |
| "What type is FConnection?" | `hover` |
| "What functions are in uConfig.pas?" | `documentSymbol` |
| "Find the TPaymentProcessor class" | `workspaceSymbol` |

## CLI Skill (Additional/Fallback)

The plugin also includes a CLI skill for advanced queries that go beyond LSP:

```bash
# Category filtering (not available via LSP)
delphi-lookup.exe "TForm" --category user -n 5

# Framework filtering
delphi-lookup.exe "TButton" --framework VCL -n 5

# Symbol type filtering
delphi-lookup.exe "MAX_BUFFER" --symbol const -n 5

# Conceptual search
delphi-lookup.exe "JSON serialization" -n 5
```

### Optional: Allow delphi-lookup Without Permission Prompts

Add to `~/.claude/settings.json`:

```json
{
  "permissions": {
    "allow": [
      "Bash(delphi-lookup.exe:*)",
      "Bash(delphi-indexer.exe:*)"
    ]
  }
}
```

## Debugging

### Enable LSP Logging

```bash
export DELPHI_LSP_LOG=/tmp/delphi-lsp.log
```

Or in `.lsp.json`:

```json
{
  "delphi": {
    "env": {
      "DELPHI_LSP_LOG": "/tmp/delphi-lsp.log"
    }
  }
}
```

### Common Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| LSP tool not available | `ENABLE_LSP_TOOL` not set | Add to `settings.json`, restart |
| "Executable not found in $PATH" | `delphi-lsp-server` not in PATH | Install binary, verify with `which delphi-lsp-server` |
| Plugin installed but disabled | Not enabled after install | `claude plugin enable delphi-lsp` + restart |
| "Total LSP servers loaded: 0" | All plugins disabled | Enable plugins, restart |
| No results from LSP | Database not found | Set `DELPHI_LSP_DATABASE` env var |
| Slow startup | Large database | Normal — server indexes on start, subsequent queries are ~50ms |

### Debug Checklist

1. Check binary: `which delphi-lsp-server` or `where delphi-lsp-server`
2. Check plugin: `claude plugin list` — look for `Status: enabled`
3. Check settings: `ENABLE_LSP_TOOL` is `"1"` in `~/.claude/settings.json`
4. Check logs: `~/.claude/debug/latest` — search for "Total LSP servers loaded: N"
5. Check database: `DELPHI_LSP_DATABASE` points to an existing `delphi_symbols.db`

## Plugin Directory Structure

```
delphi-lookup/
├── .claude-plugin/
│   └── plugin.json              # Plugin manifest
├── .lsp.json                    # LSP server configuration
├── skills/
│   └── delphi-lookup/
│       └── SKILL.md             # Claude Code skill
├── LSP/
│   ├── uLSPTypes.pas            # LSP type definitions
│   ├── uLSPProtocol.pas         # JSON-RPC 2.0 protocol
│   ├── uLSPHandlers.pas         # LSP method handlers
│   └── uPositionResolver.pas    # Cursor → identifier resolution
├── delphi-lsp-server.dpr        # LSP server entry point
└── delphi-lsp-server.dproj      # Build configuration
```
