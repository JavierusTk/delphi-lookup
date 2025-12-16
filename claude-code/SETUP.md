# Claude Code Setup for delphi-lookup

This guide helps you configure Claude Code to use delphi-lookup for Pascal/Delphi symbol searches.

## Quick Setup (Copy-Paste to Claude Code)

Copy and paste this prompt to Claude Code:

```
Install the delphi-lookup skill for me:

1. Copy the skill file from this repo's `claude-code/skill.md` to `~/.claude/skills/delphi-lookup/skill.md`

2. Add this to my global CLAUDE.md (`~/.claude/CLAUDE.md`) in a "Symbol Lookup" section:

## Delphi Symbol Lookup

**CRITICAL**: To find WHERE a Pascal symbol is defined (function, type, constant, variable):
1. **FIRST**: Use delphi-lookup.exe
2. **FALLBACK**: Use Grep only if delphi-lookup returns no results

**When to use delphi-lookup**:
| Situation | Command |
|-----------|---------|
| "Undeclared identifier: X" error | `delphi-lookup.exe "X" -n 5` |
| Find function/type definition | `delphi-lookup.exe "SymbolName" -n 5` |
| Find API usage examples | `delphi-lookup.exe "APIName" -n 10` |
| Search by concept | `delphi-lookup.exe "JSON serialization" -n 5` |

**Why**: delphi-lookup has a cached FTS5 index with sub-millisecond search. Grep scans files sequentially.

3. Tell me where delphi-lookup.exe is located so I can update the path if needed.
```

## Manual Setup

### 1. Install Skill

Create the skill directory and file:

```bash
mkdir -p ~/.claude/skills/delphi-lookup
cp claude-code/skill.md ~/.claude/skills/delphi-lookup/skill.md
```

### 2. Add to CLAUDE.md

Add to your `~/.claude/CLAUDE.md`:

```markdown
## Delphi Symbol Lookup

**CRITICAL**: To find WHERE a Pascal symbol is defined:
1. **FIRST**: Use delphi-lookup.exe
2. **FALLBACK**: Use Grep only if delphi-lookup returns no results

| Situation | Command |
|-----------|---------|
| "Undeclared identifier: X" error | `delphi-lookup.exe "X" -n 5` |
| Find function/type definition | `delphi-lookup.exe "SymbolName" -n 5` |

**Why**: Sub-millisecond cached FTS5 search vs sequential file scanning.
```

### 3. Add to Allowed Tools (Optional)

If you want Claude Code to run delphi-lookup without asking permission, add to your settings:

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

Or in `~/.claude/settings.json`.

### 4. Build Your Index

Before using delphi-lookup, you need to index your codebase:

```bash
# Index your project
delphi-indexer.exe "C:\YourProject\src" --category user

# Index Delphi standard library (optional but recommended)
delphi-indexer.exe "C:\Program Files (x86)\Embarcadero\Studio\23.0\source\rtl" --category stdlib
delphi-indexer.exe "C:\Program Files (x86)\Embarcadero\Studio\23.0\source\vcl" --category stdlib
```

## Verification

Ask Claude Code:

```
Search for TStringList using delphi-lookup
```

It should use delphi-lookup.exe instead of Grep.
