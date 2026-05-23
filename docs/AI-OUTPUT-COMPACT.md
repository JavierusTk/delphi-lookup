# delphi-lookup: AI-optimized compact output

**Status:** implemented
**Audience:** AI coding agents (sole consumer of this tool)
**Reindex required:** no — this is a query-time formatting change

## Motivation

The previous default output emitted ~12-15 lines per query — most of it chrome:

```
// Context for query: "TStringList"

// [CACHE HIT] Loaded 3 results from cache in 4 ms

Found 3 result(s) for "TStringList":

1. [Decl] procedure ProcesarH303linea(Linea:TStringList);
   → dmodLRT.pas [unit: dmodLRT] (user, VCL)

2. ...

// Search completed in 4 ms
```

For a session with 30-50 lookups this is **3K-8K tokens of pure overhead**. The unit-only location (`→ dmodLRT.pas`) also forces a follow-up Grep to locate the symbol — the agent has no `start_line` to navigate directly.

## Design

**Default output mode** changes from `compact` (two lines per result + chrome) to a denser format:

- One line per result.
- No `// Context for query`, no `// [CACHE HIT]`, no `// Search completed` chrome.
- No `Found N result(s) for "X":` preamble.
- Each line: `N. badge signature  <full-file-path>:<start_line>  [category,framework]`
- The full path + line lets an agent jump straight to the symbol with `Read offset=line` — no second Grep.

Example (3 results, 3 lines, ~250 chars total):

```
1. [Decl] procedure ProcesarH303linea(Linea:TStringList);  W:\Clientes\LRT\dmodLRT.pas:1234  [user,VCL]
2. [Decl] procedure ProcesarH302linea(Linea:TStringList);  W:\Clientes\LRT\dmodLRT.pas:1240  [user,VCL]
3. [Decl] procedure ProcesarH301linea(Linea:TStringList);  W:\Clientes\LRT\dmodLRT.pas:1245  [user,VCL]
```

When there are **0 results**, emit a single line: `No results for "<query>".` No multi-line "Try…" prose (the agent knows what to try).

## Flag matrix

| Flag | Behavior |
|---|---|
| (default) | Ultra-compact, no chrome |
| `--full` | Old verbose multi-line format with separators (unchanged) |
| `--json` | JSON output (unchanged) |
| `--with-timing` | Append a final `// Search completed in N ms` line — opt-in |

The default-on chrome is moved behind `--with-timing` because timing is debug info, not part of search results an agent will parse.

## Files touched

- `uResultFormatter.pas` — new `FormatUltraCompactSingleResult` + `FormatUltraCompactResults`
- `delphi-lookup.dpr` — flag parsing, default branch suppresses chrome

## Test plan

- `Tests/test-output-format.bat` — runs delphi-lookup.exe against an existing DB on known queries and asserts:
  - Default output has **zero** `// Context`, `// [CACHE HIT]`, `// Search completed` lines.
  - Default output has **zero** `Found N result(s)` lines.
  - Each result line contains a `:<digits>` line-number reference.
  - `--full` still emits the verbose format.
  - `--json` still emits valid JSON.
  - `--with-timing` re-adds the timing footer.

## Backwards compatibility

- `--full` and `--json` unchanged.
- Old "compact" 2-line format (`FormatCompactResults`) kept in the unit but no longer the default — accessible via the new explicit `--compact-v1` flag (rarely needed).
