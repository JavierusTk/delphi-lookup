---
effort: small
id: T-5AF2
origin: 'T-67W7 (2026-10-16): al unificar el truncado (9adc8fa), la regresión sobre
  una copia de W:\bin\delphi_symbols.db sacó la diferencia'
priority: baja
proposal: 'Contar la profundidad por directiva y no por línea: recorrer cada {$IF…}/{$IFDEF}/{$IFNDEF}/{$IFOPT}
  y cada {$ENDIF}/{$IFEND} de la línea, de modo que ''{$ifdef X}..{$endif}'' en una
  línea quede a cero. Valorar antes el efecto: unos 196 ficheros pasarían de indexarse
  enteros a solo su interfaz'
risk: med
type: bug
value: med
verification: TruncateAtImplementation sobre W:\VCL\BaseMAX\ParserCore.pas corta en
  la línea 286 y TableMax.low.pas recupera sus 9 símbolos; recuento de símbolos antes/después
  en un --force sobre una copia de la base, con la diferencia explicada por fichero
---
# TruncateAtImplementation pierde la cuenta con las condicionales de una línea y no trunca el fichero

## Qué pasa

`TruncateAtImplementation` (`uFolderScanner.pas`, única copia desde `9adc8fa`) corta la unidad en la línea `implementation` de nivel superior para el indexado solo-FTS. Para no cortar dentro de un `{$IFDEF}` lleva una profundidad de condicionales, pero la calcula **por línea**: si una línea contiene `{$IFDEF `/`{$IFNDEF `/`{$IF `/`{$IFOPT ` suma 1 y solo mira el cierre (`{$ENDIF`/`{$IFEND`) en las líneas que no abren. Una condicional completa en una sola línea suma 1 y no resta nunca, así que la profundidad no vuelve a 0, no se trunca y el fichero se indexa **entero**, implementación incluida.

## Evidencia (2026-10-16)

- `W:\VCL\BaseMAX\ParserCore.pas:135`: `procedure FuncIntToStr_Gen(Param: PExpressionRec; Val: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif});`. El `implementation` está en la línea 286, y aun así la base tenía símbolos hasta la línea 2873.
- `W:\VCL\DBdata\TableMax.low.pas:8-12`: cinco `{$IFDEF X} unidades, {$ENDIF}` en el `uses`. Al indexarse entero, DelphiAST no lo parsea (`[WARN] Failed to parse TableMax.low.pas (even after preprocessing)`) y queda con 0 símbolos; con la interfaz sola daba 9.
- Medida sobre una copia de `W:\bin\delphi_symbols.db` (script `impl_loss.py`, que cuenta los símbolos posteriores a la primera línea `implementation`): 36.313 símbolos en 196 ficheros están en esa situación (stdlib 13.583/53, third_party 11.658/113, user 11.072/30).

## Por qué no se arregló en T-67W7

Antes de `9adc8fa` el incremental tenía su propia copia sin condicionales, que sí cortaba, y `--force` e incremental daban contenido distinto. Unificarlos en esta función conserva lo que ya hay en la base (decisión del usuario, 2026-10-16). Corregir la cuenta cambia el contenido de esos 196 ficheros: pierden la implementación y, en casos como `TableMax.low.pas`, recuperan la interfaz. Eso es una decisión aparte.

## Antes de tocarlo

- Decidir qué se quiere en modo solo-FTS: solo la interfaz, que es el diseño, o también la implementación, que hoy entra por accidente en esos ficheros.
- Contar también los ficheros que hoy no se parsean al indexarse enteros (`Failed to parse … even after preprocessing` en el log de `index-all`).
