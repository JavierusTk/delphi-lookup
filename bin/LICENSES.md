# Third-Party Binary Licenses

This folder contains pre-built binaries required for delphi-lookup operation.

## SQLite (sqlite3.dll, sqlite3.exe, sqldiff.exe, sqlite3_analyzer.exe)

**Version:** 3.47.0 (2024-10-21)
**Source:** https://sqlite.org/download.html
**License:** Public Domain

SQLite is in the public domain and does not require a license. From the SQLite website:

> SQLite is in the Public Domain
>
> All of the code and documentation in SQLite has been dedicated to the public
> domain by the authors. All code authors, and representatives of the companies
> they work for, have signed affidavits dedicating their contributions to the
> public domain and originals of those signed affidavits are stored in a firesafe
> at the main offices of Hwaci. Anyone is free to copy, modify, publish, use,
> compile, sell, or distribute the original SQLite code, either in source code
> form or as a compiled binary, for any purpose, commercial or non-commercial,
> and by any means.

**Build Notes:**
- This build includes FTS5 (Full-Text Search) extension
- Compiled for Windows x64

## sqlite-vec (vec0.dll)

**Version:** 0.1.3
**Source:** https://github.com/asg017/sqlite-vec
**License:** MIT License

```
MIT License

Copyright (c) 2024 Alex Garcia

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

**Build Notes:**
- sqlite-vec provides vector similarity search capabilities
- Compiled for Windows x64
- Load with: `SELECT load_extension('vec0');`

## Usage

These binaries should be placed in the same directory as the compiled delphi-lookup
executables (delphi-indexer.exe, delphi-lookup.exe) or in the `bin/` subfolder.
