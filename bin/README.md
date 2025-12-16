# Binary Dependencies

This folder contains third-party binary dependencies required for delphi-lookup.

## Required Files

| File | Description |
|------|-------------|
| `sqlite3.dll` | SQLite library with FTS5 support (Windows x64) |
| `vec0.dll` | sqlite-vec extension for vector similarity search |

## Optional Tools

| File | Description |
|------|-------------|
| `sqlite3.exe` | SQLite command-line shell (for debugging) |
| `sqldiff.exe` | SQLite database diff tool |
| `sqlite3_analyzer.exe` | SQLite database analyzer |

## Installation

Copy these files to either:
- The same directory as delphi-indexer.exe / delphi-lookup.exe
- Or keep them in this `bin/` folder (the application searches both locations)

## Downloading Fresh Binaries

### SQLite (sqlite3.dll, sqlite3.exe, etc.)

1. Go to https://sqlite.org/download.html
2. Download "Precompiled Binaries for Windows" → 64-bit DLL
3. Extract and copy `sqlite3.dll` to this folder

**Important:** Ensure the build includes FTS5 support. The official precompiled
binaries from sqlite.org include FTS5 by default.

### sqlite-vec (vec0.dll)

1. Go to https://github.com/asg017/sqlite-vec/releases
2. Download the Windows x64 release
3. Extract and copy `vec0.dll` to this folder

## License Information

See [LICENSES.md](LICENSES.md) for detailed license information for all binaries.
