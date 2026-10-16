---
effort: small
id: T-67W7
origin: 'T-9AG2 de search-docs (2026-10-16): mismo defecto de FireDAC; comprobado
  en W:/bin/delphi_symbols.db'
priority: baja
proposal: Enlazar todos los parámetros de texto con AsWideString en lugar de AsString
  (los 127 ParamByName(...).AsString := de uDatabaseBuilder, uChangeDetector, uPackageScanner,
  uQueryProcessor y uFrameworkDetector), subir la versión de esquema con un reindexado
  completo pendiente y añadir un lint que impida que vuelva un ParamByName(...).AsString
  :=
risk: med
type: bug
value: low
verification: 'Tras el cambio y el reindexado: SELECT count(*) FROM symbols WHERE
  instr(content, char(8594)) > 0 da >0, y el comentario MT-BRIDGE de W:\VCL\BaseMAX\CreatorMax.pas
  sale con «→» y no con «?»; los tests del repo sin regresión'
---
# El texto llega a SQLite como cp1252 con best-fit: símbolos y letras no latinas se pierden en delphi_symbols.db

## Qué pasa

Los parámetros de texto se enlazan con FireDAC `ParamByName(...).AsString`, y el driver SQLite de FireDAC enlaza `AsString` (`ftString`) como ANSI: Windows lo convierte a cp1252 con «best-fit» antes de que llegue a SQLite. Lo que no existe en cp1252 se sustituye por un parecido o por `?`: `τ` → `t`, `≥` → `=`, `→` y los emojis → `?`. `ñ`, `é` y `€` sí sobreviven, porque existen en cp1252.

## Evidencia (2026-10-16, `W:/bin/delphi_symbols.db`)

- `symbols.content` con `→`: 0 filas; con `≥`: 0; con `τ`: 0. Con `ñ`: 366 y con `é`: 700. Lo mismo en `symbols.comments`.
- El comentario de `W:\VCL\BaseMAX\CreatorMax.pas` «MT-BRIDGE: Permite omitir registro UI en workers → Eliminar…» está guardado como «workers ? Eliminar». La convención MT-BRIDGE usa `→` por definición, así que todas esas marcas están afectadas.
- 127 asignaciones `ParamByName(...).AsString :=`: `uDatabaseBuilder.pas` (74), `uChangeDetector.pas` (19), `uQueryProcessor.pas` (17), `uPackageScanner.pas` (16) y `uFrameworkDetector.pas` (1). Por ejemplo, `uDatabaseBuilder.pas:1249-1271` (INSERT de símbolos).
- `OpenMode=CreateUTF8` (`uDatabaseConnection.pas:204`) no lo evita: solo fija la codificación de la base, que en SQLite ya es UTF-8.

## Lo que ya se sabe (de search-docs `T-9AG2`)

Prueba aislada con el mismo `sqlite3.dll` y cuatro variantes:
- `AsString`, con `StringFormat` por defecto o `Unicode`: se guarda degradado.
- `AsWideString`: se guarda intacto.

`StringFormat=Unicode` no influye en los parámetros. La lectura ya era Unicode: los campos salen como `WideMemo`/`WideString`. En search-docs el arreglo fue cambiar los 41 enlaces a `AsWideString`, subir el esquema a v5 marcando `full_reindex_pending` en las bases con filas, y añadir un lint en `tests/fixtures/unicode/test-unicode.sh`.

## Impacto

Como la consulta se degrada igual que el índice, la búsqueda casa en lo que es cp1252. El daño:
- no se puede buscar un término con letras griegas o símbolos;
- un `?` de la consulta casa con cualquier carácter perdido;
- el contenido devuelto (código y comentarios) sale corrupto.
