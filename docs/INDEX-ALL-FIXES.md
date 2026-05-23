# index-all.bat fixes

**Status:** implemented
**Reindex required:** yes — this *is* the reindex (run after applying fixes)

## What was wrong

DB inspection (`SELECT folder_path FROM indexed_folders`) showed 21 of 26 declared folders actually indexed. The five silent failures came from path typos in `index-all.bat`:

| Line | Declared | Reality |
|---|---|---|
| 49 | `"W:\Producción"` | folder is `W:\Produccion` (no accent), **144 .pas missed** |
| 72 | `"W:\mORMot2"` | lives at `W:\Public\mORMot2`, **420 .pas missed** |
| 73 | `"W:\ElevateDB 2 VCL-CS-SRC"` | doesn't exist on this machine |
| 75 | `"W:\RBE"` | doesn't exist |
| 79 | `"W:\SecureBridge"` | doesn't exist |

The `if errorlevel 1 (echo WARNING ...)` clause makes these failures look like warnings buried in 30+ minutes of console output — invisible in practice.

## Changes applied

### Path corrections
- `W:\Producción` → `W:\Produccion`
- `W:\mORMot2` → `W:\Public\mORMot2`
- Removed entries that don't exist locally (ElevateDB, RBE, SecureBridge). Kept commented in case they reappear.

### Coverage additions (new sections)

- **`--scan-packages`** for `W:\Packages290` and the Embarcadero `lib` so framework detection has its primary tier filled. This eliminates the 20.641 misclassified `framework=RTL` rows we saw on user code.
- **`--index-chm`** loop over `C:\Program Files (x86)\Embarcadero\Studio\23.0\Help\Doc\*.chm` so `--category official_help` is populated (was 0 rows). Each CHM is indexed with `--delphi-version 12.0`.

### Pre-flight existence check

Before launching the long indexing, the script now `dir` -checks each folder and prints `[SKIP] not found` for any missing path instead of letting the indexer fail silently 30 minutes in.

## How to run

```
W:\bin\index-all.bat              REM incremental (only modified files)
W:\bin\index-all.bat --force      REM full reindex (~45 min)
```

After the changes above, expect:

- `indexed_folders` count: 26 declared → ~22-24 actually indexed (the non-existent ones are pre-skipped, not silently failed)
- `Producción` symbols searchable
- `mORMot2` symbols searchable
- `source_category='official_help'` populated from CHMs
- `framework='RTL'` no longer the dominant tag on user code

## Verification queries

```sql
-- Confirm Producción is in
SELECT COUNT(*) FROM symbols WHERE file_path LIKE 'W:\Produccion\%';

-- Confirm mORMot2 is in
SELECT COUNT(*) FROM symbols WHERE file_path LIKE 'W:\Public\mORMot2\%';

-- Confirm CHM docs landed
SELECT COUNT(*) FROM symbols WHERE source_category='official_help';

-- Confirm framework tags now correct (most user code should be VCL, not RTL)
SELECT source_category, framework, COUNT(*) FROM symbols GROUP BY 1,2 ORDER BY 3 DESC;
```
