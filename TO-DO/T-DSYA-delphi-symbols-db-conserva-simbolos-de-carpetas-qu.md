---
effort: small
id: T-DSYA
origin: 'T-67W7 (2026-10-16): tras el reindexado completo de PT-JAVI, 596 ficheros
  siguen con el hash vacío y son todos de rutas inexistentes'
priority: baja
proposal: Añadir al indexer un modo de purga (p. ej. --prune-missing) que borre de
  symbols/symbols_fts/vec_embeddings/file_hashes/indexed_files/indexed_folders lo
  que cuelga de rutas inexistentes en disco, y llamarlo al final de index-all.bat
risk: low
type: debt
value: med
verification: Tras la purga, SELECT count(*) FROM symbols WHERE file_path LIKE 'W:\Produccion\%'
  OR file_path LIKE 'W:\Public\mORMot2\%' da 0, y no queda ninguna fila de file_hashes
  cuyo fichero no exista
---
# delphi_symbols.db conserva símbolos de carpetas que ya no existen y ningún pase los purga

## Qué pasa

La detección de borrados del incremental (`DetectAndDeleteEliminatedFolders` y `DetectDeletedFiles` en `uChangeDetector.pas`) solo mira dentro de la raíz que se está indexando. Si se cambia de sitio o de nombre una carpeta raíz entera, o deja de estar en `index-all.bat`, sus símbolos se quedan en la base para siempre y salen en las búsquedas apuntando a ficheros que no existen.

## Evidencia (`W:\bin\delphi_symbols.db` de PT-JAVI, 2026-10-16, tras el reindexado de T-67W7)

- 596 filas de `file_hashes` siguen con el hash vacío que puso la migración de T-67W7, y todas son de rutas inexistentes: `W:\Public\…` (420, el antiguo `W:\Public\mORMot2`, que hoy se indexa como `W:\mORMot2`), `W:\Produccion\…` (144; la carpeta real es `W:\Producción`) y `W:\Clientes\_Varios\JMN\…` (32).
- 46.060 símbolos cuelgan de `W:\Produccion\` y `W:\Public\mORMot2\`. Además de ser duplicados obsoletos, conservan el texto degradado a cp1252 de antes de T-67W7, porque ningún pase los vuelve a procesar.
- `indexed_folders` mantiene `W:\Produccion` y `W:\Public\mORMot2` con `folder_hash=''`.

## Notas

- `W:\VCL\DBdata` también tiene el `folder_hash` vacío aunque sus ficheros ya tienen hash. No provoca reprocesado, solo el cálculo del MD5 de sus ficheros en cada pase; conviene ver por qué no se rellena.
- Cada máquina de la flota tiene su propia `delphi_symbols.db`, así que la purga tiene que ejecutarse en cada una (de ahí la propuesta de meterla en `index-all.bat`).
