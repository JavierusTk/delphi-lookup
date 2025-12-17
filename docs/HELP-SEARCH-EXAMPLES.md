# Búsquedas Realistas en Documentación Delphi

Este documento lista búsquedas que un desarrollador **realmente haría** contra la ayuda de Delphi.

## Premisa Crítica

La ayuda de Delphi es una **referencia de API**, no un tutorial. El uso real es:
- Buscar documentación de un símbolo **que ya conozco**
- Ver parámetros, tipos de retorno, ejemplos de sintaxis
- Encontrar en qué unidad está algo

**Lo que NO hago en la ayuda de Delphi:**
- Preguntas conceptuales → Google/Stack Overflow/ChatGPT
- "How to X" → Google/AI
- Comparaciones → Google/AI
- Best practices → Google/AI
- Debugging → Google/Stack Overflow

---

## Búsquedas Realistas

### 1. Clases (quiero ver propiedades/métodos disponibles)

| # | Query | Intención |
|---|-------|-----------|
| 1 | `TStringList` | Ver métodos de manipulación de listas de strings |
| 2 | `TList<T>` | Ver métodos de lista genérica |
| 3 | `TDictionary` | Ver cómo usar el diccionario |
| 4 | `TThread` | Ver propiedades y métodos de threading |
| 5 | `TStream` | Ver jerarquía de streams |
| 6 | `TMemoryStream` | Ver métodos específicos |
| 7 | `TFileStream` | Ver constructores y modos |
| 8 | `TJSONObject` | Ver API de JSON |
| 9 | `TDataSet` | Ver métodos de datasets |
| 10 | `TField` | Ver propiedades de campos |
| 11 | `TButton` | Ver eventos disponibles |
| 12 | `TForm` | Ver ciclo de vida |
| 13 | `TTimer` | Ver propiedades |
| 14 | `TRegEx` | Ver métodos de regex |
| 15 | `TTask` | Ver API de parallel programming |

### 2. Métodos/Propiedades Específicos

| # | Query | Intención |
|---|-------|-----------|
| 16 | `TStringList.Sort` | ¿Tiene parámetro de comparación? |
| 17 | `TStringList.Find` | ¿Cómo funciona la búsqueda binaria? |
| 18 | `TList.Sort` | ¿Qué comparador usa? |
| 19 | `TThread.Synchronize` | ¿Cómo se usa correctamente? |
| 20 | `TThread.Queue` | Diferencia con Synchronize |
| 21 | `TStream.CopyFrom` | ¿Qué parámetros tiene? |
| 22 | `TDataSet.Locate` | ¿Qué opciones acepta? |
| 23 | `TDataSet.Lookup` | ¿Qué retorna? |
| 24 | `TField.AsVariant` | ¿Cómo maneja nulls? |
| 25 | `TForm.ShowModal` | ¿Qué valores retorna? |

### 3. Funciones del RTL

| # | Query | Intención |
|---|-------|-----------|
| 26 | `FormatDateTime` | Ver format specifiers |
| 27 | `Format` | Ver format specifiers |
| 28 | `StrToDateTime` | ¿Qué formatos acepta? |
| 29 | `TryStrToInt` | ¿Cómo maneja errores? |
| 30 | `Copy` | ¿Los índices son 1-based? |
| 31 | `Pos` | ¿Retorna 0 o -1 si no encuentra? |
| 32 | `StringReplace` | ¿Qué flags tiene? |
| 33 | `IncMonth` | ¿Qué pasa con días inválidos? |
| 34 | `FileExists` | ¿Funciona con directorios? |
| 35 | `DirectoryExists` | ¿Maneja UNC paths? |
| 36 | `ExtractFilePath` | ¿Incluye trailing slash? |
| 37 | `ChangeFileExt` | ¿Necesita el punto? |
| 38 | `SameText` | ¿Es case-insensitive? |
| 39 | `CompareStr` | ¿Qué valores retorna? |
| 40 | `Sleep` | ¿Unidades en ms? |

### 4. Tipos y Enumeraciones

| # | Query | Intención |
|---|-------|-----------|
| 41 | `TDateTime` | ¿Cómo se representa internamente? |
| 42 | `TArray<T>` | ¿Diferencia con array of T? |
| 43 | `TProc` | ¿Qué signature tiene? |
| 44 | `TFunc` | Ver variantes |
| 45 | `TAlignment` | Ver valores posibles |
| 46 | `TModalResult` | Ver constantes (mrOk, etc.) |
| 47 | `TShiftState` | Ver flags del teclado |
| 48 | `TSearchOption` | Ver opciones de búsqueda |
| 49 | `TEncoding` | Ver encodings disponibles |
| 50 | `TFormatSettings` | Ver campos configurables |

### 5. Excepciones

| # | Query | Intención |
|---|-------|-----------|
| 51 | `EAccessViolation` | ¿Qué información contiene? |
| 52 | `EInOutError` | ¿Qué códigos de error tiene? |
| 53 | `EConvertError` | ¿Cuándo se lanza? |
| 54 | `EDatabaseError` | Ver jerarquía |
| 55 | `EAbort` | ¿Se propaga o es silenciosa? |

### 6. Unidades (¿dónde está X?)

| # | Query | Intención |
|---|-------|-----------|
| 56 | `System.SysUtils` | ¿Qué contiene? |
| 57 | `System.Classes` | ¿Qué contiene? |
| 58 | `System.IOUtils` | Ver funciones de archivo modernas |
| 59 | `System.JSON` | Ver clases JSON |
| 60 | `System.RegularExpressions` | Ver API de regex |
| 61 | `System.Generics.Collections` | Ver colecciones genéricas |
| 62 | `System.Threading` | Ver parallel programming |
| 63 | `Vcl.Dialogs` | Ver diálogos disponibles |
| 64 | `Vcl.Forms` | Ver clases de formularios |
| 65 | `Data.DB` | Ver clases de base de datos |

### 7. Constantes

| # | Query | Intención |
|---|-------|-----------|
| 66 | `fmOpenRead` | ¿Qué valor tiene? |
| 67 | `fmShareDenyNone` | ¿Se puede combinar? |
| 68 | `mrOk` | ¿Qué valor numérico? |
| 69 | `MaxInt` | ¿Cuál es el valor? |
| 70 | `NaN` | ¿Cómo se detecta? |

### 8. Directivas del Compilador

| # | Query | Intención |
|---|-------|-----------|
| 71 | `$IFDEF` | Sintaxis correcta |
| 72 | `$REGION` | ¿Cómo se usa? |
| 73 | `$R` | Vincular recursos |
| 74 | `$WARN` | Suprimir warnings específicos |
| 75 | `$RTTI` | Controlar generación RTTI |

---

## Estadísticas

| Categoría | Cantidad | % |
|-----------|----------|---|
| Clases | 15 | 20% |
| Métodos/Propiedades | 10 | 13% |
| Funciones RTL | 15 | 20% |
| Tipos/Enumeraciones | 10 | 13% |
| Excepciones | 5 | 7% |
| Unidades | 10 | 13% |
| Constantes | 5 | 7% |
| Directivas | 5 | 7% |
| **Total** | **75** | **100%** |

---

## Conclusión

**100% de las búsquedas realistas son identificadores exactos.**

- No hay preguntas en lenguaje natural
- No hay "how to X"
- No hay comparaciones
- No hay best practices

**FTS5 es perfectamente suficiente** para el uso real de la documentación de Delphi.

Los embeddings **no aportan valor** porque:
1. Siempre busco un nombre exacto
2. FTS5 matchea perfectamente nombres de símbolos
3. Las búsquedas conceptuales las hago en Google/AI, no en la ayuda

---

## ¿Entonces para qué indexar la ayuda?

El valor de indexar la ayuda de Delphi en delphi-lookup es:

1. **Búsqueda unificada**: Un solo lugar para código + documentación
2. **Contexto para AI**: Claude Code puede buscar documentación oficial sin salir al browser
3. **Offline**: Funciona sin conexión a internet
4. **Velocidad**: Más rápido que abrir el CHM y buscar manualmente

Pero **FTS5-only es totalmente suficiente**. No necesitamos embeddings para buscar `FormatDateTime` o `TStringList.Sort`.
