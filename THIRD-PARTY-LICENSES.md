# Third-Party Licenses

This project uses the following third-party libraries and tools:

## Runtime Dependencies

### SQLite
- **License:** Public Domain
- **Website:** https://sqlite.org/
- **Files:** `bin/sqlite3.dll`, `bin/sqlite3.exe`, `bin/sqldiff.exe`, `bin/sqlite3_analyzer.exe`
- **Note:** SQLite is in the public domain. No license restrictions apply.

### sqlite-vec
- **License:** MIT License
- **Author:** Alex Garcia
- **Website:** https://github.com/asg017/sqlite-vec
- **Files:** `bin/vec0.dll`

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

## Compilation Dependencies

### DelphiAST (Modified)
- **License:** Mozilla Public License 2.0
- **Original Author:** Roman Yankovsky
- **Website:** https://github.com/RomanYankovsky/DelphiAST
- **Files:** `DelphiAST/` directory
- **Status:** **MODIFIED** - This is a modified version of DelphiAST
- **Modifications:**
  - `Source/SimpleParser/SimpleParser.Lexer.pas`: Added compatibility for older Delphi versions without intrinsic string helpers (uses `TCharacter.IsLetterOrDigit()` instead of `AChar.IsLetterOrDigit`)
- **Note:** Per MPL 2.0 Section 3.1, modifications to covered files remain under MPL 2.0. The original license file is preserved at `DelphiAST/LICENSE`

### mORMot 2
- **License:** Mozilla Public License 2.0 / LGPL
- **Website:** https://github.com/synopse/mORMot2
- **Note:** Used for JSON handling and HTTP client functionality

### FireDAC
- **License:** Commercial (included with RAD Studio)
- **Note:** Part of Embarcadero RAD Studio. Not redistributed.

## External Services (Optional)

### Ollama
- **License:** MIT License
- **Website:** https://ollama.ai/
- **Note:** Used for embedding generation during indexing. Not bundled.

## Acknowledgments

This project would not be possible without the excellent work of the open-source
community. Special thanks to:

- The SQLite team for their public domain database engine
- Alex Garcia for sqlite-vec vector search extension
- The mORMot team for their comprehensive Delphi framework
- Roman Yankovsky for DelphiAST
