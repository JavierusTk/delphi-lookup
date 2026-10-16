# RESOLVED

Tombstones for resolved TO-DO items (machine-readable; read by `agent-todo lint`).

<!-- agent-todo:resolved:start -->
- commit: 796293a
  note: 'Hecho y desplegado el 2026-10-16. 796293a: los 153 ParamByName(...).AsString
    := pasan a AsWideString, y MigrateTextEncoding vacía los hashes de una base sin
    metadata.text_encoding=''utf16'' para que la pasada incremental la reindexe sin
    --force. 713342b: la salida redirigida de delphi-lookup sale en UTF-8; antes llegaba
    en cp1252 aunque la base estuviera bien. 9adc8fa: una sola TruncateAtImplementation
    para --force e incremental; sin ella, la migración habría borrado unos 36.000
    símbolos en 196 ficheros. 62b4a23: Tests/test-unicode.ps1 da 14/14 con los binarios
    nuevos y falla 11 de 14 con los anteriores, y su lint detecta un AsString metido
    a propósito. Desplegado en W:\bin e773505. Base de PT-JAVI reindexada en el momento
    con delphi-lookup-reindex.bat (39 min; exit 1 solo por los .chm de siempre): 263
    símbolos con → en content, 295 en comments, marca utf16, y el MT-BRIDGE de W:\VCL\BaseMAX\CreatorMax.pas
    sale con «→». TTableMAX, CrearQuery y ErrorInformado dan lo mismo con el lookup
    nuevo que con el viejo. Las demás máquinas se migran solas en su tarea de las
    04:30. Quedan dos items aparte: T-5AF2 (condicionales de una línea en el truncado)
    y el de las carpetas huérfanas, que no se reindexan y conservan el texto degradado.'
  resolved_at: '2026-10-16'
  title: 'El texto llega a SQLite como cp1252 con best-fit: símbolos y letras no latinas
    se pierden en delphi_symbols.db'
  uid: delphi-lookup:T-67W7
<!-- agent-todo:resolved:end -->
