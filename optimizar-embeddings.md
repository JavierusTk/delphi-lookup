# Optimización de Búsqueda por Embeddings en delphi-lookup

## Resumen Ejecutivo

Este documento detalla los problemas identificados en el sistema de búsqueda por embeddings de delphi-lookup y proporciona soluciones concretas para mejorar la calidad de los resultados.

**Estado actual:** Precisión ~35-64% (benchmark en código español)
**Objetivo:** Precisión ~75-85% (alineado con codebases en inglés)

**Problemas críticos identificados:**
1. Asimetría entre texto indexado y queries
2. Métrica de distancia incorrecta (L2 vs Cosine)
3. Conversión distance→score incorrecta (genera scores negativos)

---

## Problema #1: Asimetría entre Indexación y Query

### Descripción

El texto usado para generar embeddings durante la **indexación** tiene un formato estructurado con prefijos, mientras que los **queries** se pasan sin procesar. Esto causa que los vectores estén en espacios semánticos diferentes.

### Ubicación del código

**Indexación** - `uIndexerEmbeddings.Ollama.pas:218-278`:

```pascal
function TOllamaEmbeddingGenerator.CreateEmbeddingText(AChunk: TCodeChunk): string;
var
  TextParts: TStringList;
  CodeText: string;
begin
  TextParts := TStringList.Create;
  try
    // Prioritize code content
    CodeText := AChunk.EnrichedText;
    if CodeText = '' then
      CodeText := AChunk.Content;

    if CodeText <> '' then
    begin
      TextParts.Add('CODE:');                    // ← Prefijo estructurado
      if Length(CodeText) > 8192 then
        TextParts.Add(Copy(CodeText, 1, 8192) + '...')
      else
        TextParts.Add(CodeText);
    end;

    if AChunk.Name <> '' then
      TextParts.Add('NAME: ' + AChunk.Name);     // ← Prefijo estructurado

    // ... más prefijos: TYPE:, INHERITS:, IMPLEMENTS:, FILE:, DESCRIPTION:

    Result := TextParts.Text;
  finally
    TextParts.Free;
  end;
end;
```

**Query** - `uLookupEmbeddings.Ollama.pas:160-182`:

```pascal
function TOllamaEmbeddingGenerator.GenerateEmbedding(const AText: string): TEmbedding;
var
  Vector: TVector;
begin
  Result := nil;

  if Trim(AText) = '' then
    Exit;

  Vector := CallOllamaAPI(AText);  // ← AText pasa DIRECTO sin formato

  if Length(Vector) > 0 then
    Result := TEmbedding.Create('query', AText, Vector);
end;
```

### Ejemplo del problema

**Documento indexado:**
```
CODE:
function TMyClass.Calculate(Value: Integer): Double;
begin
  Result := Value * 2.5;
end;
NAME: Calculate
FULL_NAME: TMyClass.Calculate
TYPE: function
INHERITS: TBaseClass
FILE: MyUnit.pas
DESCRIPTION:
// Calcula el doble del valor
```

**Query del usuario:** `"calculate value"`

Los embeddings generados están en espacios semánticos muy diferentes, lo que reduce drásticamente la similaridad.

### Solución propuesta

Modificar `uLookupEmbeddings.Ollama.pas` para aplicar formato similar al query:

```pascal
function TOllamaEmbeddingGenerator.GenerateEmbedding(const AText: string): TEmbedding;
var
  Vector: TVector;
  FormattedQuery: string;
begin
  Result := nil;

  if Trim(AText) = '' then
    Exit;

  // NUEVO: Formatear query similar al documento indexado
  FormattedQuery := FormatQueryText(AText);

  Vector := CallOllamaAPI(FormattedQuery);

  if Length(Vector) > 0 then
    Result := TEmbedding.Create('query', AText, Vector);
end;

// NUEVO: Función para formatear queries
function TOllamaEmbeddingGenerator.FormatQueryText(const AQuery: string): string;
var
  CleanQuery: string;
begin
  CleanQuery := Trim(AQuery);

  // Detectar si parece un identificador Pascal (TClassName, FunctionName, etc.)
  if LooksLikeIdentifier(CleanQuery) then
  begin
    // Query tipo: "TStringList", "Calculate", "FMyField"
    Result := 'NAME: ' + CleanQuery;
  end
  else
  begin
    // Query tipo: "validate input before save", "convert json to object"
    Result := 'DESCRIPTION: ' + CleanQuery;
  end;
end;

// NUEVO: Detectar si es un identificador Pascal
function TOllamaEmbeddingGenerator.LooksLikeIdentifier(const AText: string): Boolean;
var
  Words: TArray<string>;
begin
  // Un identificador típicamente:
  // - Es una sola palabra (sin espacios)
  // - Empieza con T, F, I, E (convenciones Delphi) o letra mayúscula
  // - Usa PascalCase o tiene underscores

  Words := AText.Split([' '], TStringSplitOptions.ExcludeEmpty);

  if Length(Words) <> 1 then
    Exit(False);

  // Patrón típico de identificadores Delphi
  Result := TRegEx.IsMatch(AText, '^[A-Z][a-zA-Z0-9_]*$') or  // PascalCase
            TRegEx.IsMatch(AText, '^[TFIEtfie][A-Z]');         // Prefijos Delphi
end;
```

### Archivos a modificar

| Archivo | Cambios |
|---------|---------|
| `uLookupEmbeddings.Ollama.pas` | Añadir `FormatQueryText`, `LooksLikeIdentifier`, modificar `GenerateEmbedding` |

---

## Problema #2: Métrica de Distancia Incorrecta

### Descripción

Se usa **distancia L2 (Euclidiana)** pero los embeddings de Jina están **normalizados** (norma = 1.0), lo que hace que **cosine similarity** sea la métrica más apropiada.

### Ubicación del código

`uVectorSearch.pas:261-288`:

```pascal
// Build the vector search query using vec0 MATCH syntax
FQuery.SQL.Text := Format('''
  SELECT
    v.symbol_id,
    s.*,
    vec_distance_L2(v.embedding, %s) as distance   -- ← PROBLEMA: L2
  FROM vec_embeddings v
  JOIN symbols s ON v.symbol_id = s.id
  WHERE v.embedding MATCH %s AND k = %d
  ORDER BY distance
''', [QuotedStr(VectorJSON), QuotedStr(VectorJSON), AMaxResults * 2]);
```

### Por qué L2 no es óptimo

Para vectores normalizados (norma = 1):
- **L2 distance** range: [0, 2.0]
- **Cosine similarity** range: [-1, 1]
- Relación: `L2² = 2(1 - cosine_similarity)`

Jina-code-embed genera vectores normalizados, diseñados para cosine similarity.

### Solución propuesta

**Opción A: Usar vec_distance_cosine (si sqlite-vec lo soporta)**

```pascal
// En uVectorSearch.pas:261-288
FQuery.SQL.Text := Format('''
  SELECT
    v.symbol_id,
    s.*,
    vec_distance_cosine(v.embedding, %s) as distance
  FROM vec_embeddings v
  JOIN symbols s ON v.symbol_id = s.id
  WHERE v.embedding MATCH %s AND k = %d
  ORDER BY distance
''', [QuotedStr(VectorJSON), QuotedStr(VectorJSON), AMaxResults * 2]);
```

**Opción B: Convertir L2 a cosine similarity manualmente**

Si sqlite-vec no soporta `vec_distance_cosine`, calcular la conversión:

```pascal
// En uVectorSearch.pas, después de obtener la distancia L2
var L2Distance := FQuery.FieldByName('distance').AsFloat;

// Convertir L2 a cosine similarity (para vectores normalizados)
// Fórmula: cosine_sim = 1 - (L2² / 2)
var CosineSimilarity := 1.0 - (L2Distance * L2Distance / 2.0);

// Normalizar a [0, 1] para el score
SearchResult.Score := (CosineSimilarity + 1.0) / 2.0;
```

**Opción C: Verificar soporte de funciones de distancia**

Añadir detección en tiempo de ejecución:

```pascal
// En TVectorSearch.Initialize o Create
function TVectorSearch.DetectDistanceFunction: string;
begin
  // Probar vec_distance_cosine primero
  try
    FQuery.SQL.Text := 'SELECT vec_distance_cosine(''[1,0]'', ''[1,0]'')';
    FQuery.Open;
    FQuery.Close;
    Result := 'vec_distance_cosine';
    FDistanceFunction := 'cosine';
  except
    // Fallback a L2
    Result := 'vec_distance_L2';
    FDistanceFunction := 'L2';
  end;
end;
```

### Archivos a modificar

| Archivo | Cambios |
|---------|---------|
| `uVectorSearch.pas` | Cambiar función de distancia o añadir conversión |

---

## Problema #3: Conversión Distance → Score Incorrecta

### Descripción

La conversión actual `Score := 1.0 - Distance` asume que la distancia está en el rango [0, 1], pero L2 puede retornar valores en [0, 2+], generando **scores negativos**.

### Ubicación del código

`uVectorSearch.pas:197-201`:

```pascal
while not FQuery.EOF do
begin
  var Distance := FQuery.FieldByName('distance').AsFloat;

  SearchResult := TSearchResult.Create;
  // ... asignar campos ...
  SearchResult.Score := 1.0 - Distance;  // ← PROBLEMA: puede ser negativo
  SearchResult.MatchType := 'vector_similarity';
```

### Demostración del problema

| Distance (L2) | Score actual | ¿Válido? |
|---------------|--------------|----------|
| 0.0 | 1.0 | ✅ |
| 0.3 | 0.7 | ✅ |
| 0.8 | 0.2 | ⚠️ |
| 1.0 | 0.0 | ❌ |
| 1.5 | -0.5 | ❌ Negativo |
| 2.0 | -1.0 | ❌ Negativo |

### Solución propuesta

Implementar conversión correcta según la métrica de distancia:

```pascal
// En uVectorSearch.pas - Crear función de conversión

/// <summary>
/// Convierte distancia a score de similaridad [0, 1]
/// </summary>
function TVectorSearch.DistanceToScore(ADistance: Double): Double;
begin
  case FDistanceFunction of
    'cosine':
      // vec_distance_cosine retorna 1 - cosine_similarity
      // Rango: [0, 2], donde 0 = idéntico, 2 = opuesto
      Result := 1.0 - (ADistance / 2.0);

    'L2':
      // L2 distance para vectores normalizados
      // Rango: [0, 2], donde 0 = idéntico
      // Opción 1: Exponential decay (recomendado)
      Result := Exp(-ADistance);

      // Opción 2: Inverse (alternativa)
      // Result := 1.0 / (1.0 + ADistance);

      // Opción 3: Conversión a cosine (para vectores normalizados)
      // var CosineSim := 1.0 - (ADistance * ADistance / 2.0);
      // Result := (CosineSim + 1.0) / 2.0;

  else
    // Fallback seguro
    Result := Max(0.0, 1.0 - ADistance);
  end;

  // Asegurar rango [0, 1]
  Result := Max(0.0, Min(1.0, Result));
end;

// Uso en SearchSimilar:
while not FQuery.EOF do
begin
  var Distance := FQuery.FieldByName('distance').AsFloat;

  SearchResult := TSearchResult.Create;
  // ... asignar campos ...
  SearchResult.Score := DistanceToScore(Distance);  // ← CORREGIDO
  SearchResult.MatchType := 'vector_similarity';
```

### Archivos a modificar

| Archivo | Cambios |
|---------|---------|
| `uVectorSearch.pas` | Añadir `DistanceToScore`, añadir campo `FDistanceFunction`, modificar `SearchSimilar` |

---

## Problema #4: Umbral de Filtrado Incorrecto

### Descripción

El umbral de filtrado usa la misma lógica incorrecta del score, aceptando resultados con similaridad muy baja.

### Ubicación del código

`uVectorSearch.pas:298-302`:

```pascal
// Apply minimum similarity threshold (based on max distance parameter)
var MinSimilarity := 1.0 - AMaxDistance;  // Con AMaxDistance=1.5 → MinSimilarity=-0.5

if SearchResult.Score > MinSimilarity then
  Result.Add(SearchResult)
```

### Solución propuesta

Recalcular el umbral basado en la nueva conversión de score:

```pascal
// En uVectorSearch.pas

function TVectorSearch.SearchSimilar(const AQuery: string;
  AMaxResults: Integer; AMaxDistance: Double): TSearchResultList;
var
  MinScore: Double;
begin
  // Convertir max distance a min score usando la misma función
  MinScore := DistanceToScore(AMaxDistance);

  // ... código de búsqueda ...

  while not FQuery.EOF do
  begin
    var Distance := FQuery.FieldByName('distance').AsFloat;
    var Score := DistanceToScore(Distance);

    // Filtrar por score mínimo
    if Score >= MinScore then
    begin
      SearchResult := TSearchResult.Create;
      SearchResult.Score := Score;
      // ...
      Result.Add(SearchResult);
    end;

    FQuery.Next;
  end;
end;
```

### Archivos a modificar

| Archivo | Cambios |
|---------|---------|
| `uVectorSearch.pas` | Modificar lógica de filtrado en `SearchSimilar` |
| `uConfig.pas` | Revisar `DEFAULT_MAX_DISTANCE` si es necesario |

---

## Problema #5: Optimización de Rendimiento - Construcción de JSON

### Descripción

El vector se convierte a JSON usando concatenación de strings en un loop, lo cual es O(n²) para 1536 dimensiones.

### Ubicación del código

`uVectorSearch.pas:250-258` y `uDatabaseBuilder.pas:1127-1141`:

```pascal
var USFormat: TFormatSettings := TFormatSettings.Create('en-US');  // Creado cada vez
var VectorJSON: string := '[';
for I := 0 to Length(QueryEmbedding.Vector) - 1 do
begin
  if I > 0 then
    VectorJSON := VectorJSON + ',';  // ← Concatenación O(n²)
  VectorJSON := VectorJSON + FloatToStr(QueryEmbedding.Vector[I], USFormat);
end;
VectorJSON := VectorJSON + ']';
```

### Solución propuesta

```pascal
// Crear variable de clase o unit-level para TFormatSettings
var
  GUSFormat: TFormatSettings;

initialization
  GUSFormat := TFormatSettings.Create('en-US');

// Usar TStringBuilder para construcción eficiente
function VectorToJSON(const AVector: TVector): string;
var
  Builder: TStringBuilder;
  I: Integer;
begin
  Builder := TStringBuilder.Create(Length(AVector) * 16);  // Pre-allocate
  try
    Builder.Append('[');
    for I := 0 to High(AVector) do
    begin
      if I > 0 then
        Builder.Append(',');
      Builder.Append(FloatToStr(AVector[I], GUSFormat));
    end;
    Builder.Append(']');
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
```

### Archivos a modificar

| Archivo | Cambios |
|---------|---------|
| `uVectorSearch.pas` | Añadir `VectorToJSON`, variable global `GUSFormat` |
| `uDatabaseBuilder.pas` | Usar misma función o duplicar optimización |

---

## Problema #6: RemoveDuplicates O(n²)

### Descripción

El algoritmo de eliminación de duplicados tiene complejidad O(n²) con llamadas adicionales a `IndexOf` que también son O(n).

### Ubicación del código

`uSearchTypes.pas:114-149`:

```pascal
procedure TSearchResultList.RemoveDuplicates;
var
  I, J: Integer;
  ItemsToDelete: TList<Integer>;
begin
  ItemsToDelete := TList<Integer>.Create;
  try
    for I := 0 to Count - 1 do
    begin
      for J := I + 1 to Count - 1 do  // ← O(n²)
      begin
        if (Items[I].SymbolID = Items[J].SymbolID) and
           (ItemsToDelete.IndexOf(I) = -1) and  // ← O(n) adicional
           (ItemsToDelete.IndexOf(J) = -1) then  // ← O(n) adicional
```

### Solución propuesta

Usar TDictionary para O(n):

```pascal
procedure TSearchResultList.RemoveDuplicates;
var
  Seen: TDictionary<Integer, TSearchResult>;
  I: Integer;
  Current, Existing: TSearchResult;
  NewList: TList<TSearchResult>;
begin
  Seen := TDictionary<Integer, TSearchResult>.Create;
  NewList := TList<TSearchResult>.Create;
  try
    for I := 0 to Count - 1 do
    begin
      Current := Items[I];

      if Seen.TryGetValue(Current.SymbolID, Existing) then
      begin
        // Ya existe: mantener el de mayor score o exact match
        if Current.IsExactMatch or (Current.Score > Existing.Score) then
        begin
          // Reemplazar en el diccionario
          Seen[Current.SymbolID] := Current;
          // Liberar el anterior
          Existing.Free;
        end
        else
        begin
          // Descartar el actual
          Current.Free;
        end;
      end
      else
      begin
        // Nuevo: añadir al diccionario
        Seen.Add(Current.SymbolID, Current);
      end;
    end;

    // Reconstruir la lista desde el diccionario
    // Nota: OwnsObjects debe ser False temporalmente
    var OldOwns := OwnsObjects;
    OwnsObjects := False;
    Clear;
    OwnsObjects := OldOwns;

    for Current in Seen.Values do
      Add(Current);

  finally
    Seen.Free;
    NewList.Free;
  end;
end;
```

### Archivos a modificar

| Archivo | Cambios |
|---------|---------|
| `uSearchTypes.pas` | Reescribir `RemoveDuplicates` usando TDictionary |

---

## Plan de Implementación

### Fase 1: Fixes Críticos (Prioridad Alta)

| Orden | Tarea | Archivo | Impacto Estimado |
|-------|-------|---------|------------------|
| 1.1 | Implementar `DistanceToScore` | `uVectorSearch.pas` | +15-20% precisión |
| 1.2 | Corregir filtrado por umbral | `uVectorSearch.pas` | Evita falsos positivos |
| 1.3 | Implementar `FormatQueryText` | `uLookupEmbeddings.Ollama.pas` | +20-30% precisión |
| 1.4 | Añadir `LooksLikeIdentifier` | `uLookupEmbeddings.Ollama.pas` | Soporte para 1.3 |

### Fase 2: Optimización de Métrica (Prioridad Alta)

| Orden | Tarea | Archivo | Impacto Estimado |
|-------|-------|---------|------------------|
| 2.1 | Detectar soporte `vec_distance_cosine` | `uVectorSearch.pas` | Mejor precisión |
| 2.2 | Implementar fallback L2→Cosine | `uVectorSearch.pas` | Compatibilidad |

### Fase 3: Optimización de Rendimiento (Prioridad Media)

| Orden | Tarea | Archivo | Impacto Estimado |
|-------|-------|---------|------------------|
| 3.1 | Implementar `VectorToJSON` con TStringBuilder | `uVectorSearch.pas` | -10-30ms latencia |
| 3.2 | Variable global `GUSFormat` | `uVectorSearch.pas` | -5ms latencia |
| 3.3 | Reescribir `RemoveDuplicates` O(n) | `uSearchTypes.pas` | -5-30ms latencia |

### Fase 4: Mejoras Adicionales (Prioridad Baja)

| Orden | Tarea | Archivo | Impacto Estimado |
|-------|-------|---------|------------------|
| 4.1 | Caché de query embeddings | `uLookupEmbeddings.Ollama.pas` | -400-1000ms (queries repetidos) |
| 4.2 | Consolidar queries de metadata | `uVectorSearch.pas` | -6ms |

---

## Verificación de Cambios

### Tests Sugeridos

```bash
# Test 1: Búsqueda por nombre de clase
delphi-lookup.exe "TStringList" -n 5

# Test 2: Búsqueda por descripción
delphi-lookup.exe "convert string to integer" -n 5

# Test 3: Búsqueda mixta
delphi-lookup.exe "TButton click handler" -n 5

# Test 4: Verificar que scores están en [0,1]
delphi-lookup.exe "any query" -n 10 --verbose
# Todos los scores deben ser >= 0 y <= 1
```

### Métricas a Monitorear

1. **Precisión@10**: Porcentaje de resultados relevantes en top 10
2. **Hit Rate@10**: Porcentaje de queries con al menos 1 resultado relevante
3. **Latencia**: Tiempo de respuesta (debe mantenerse < 5s)
4. **Rango de scores**: Verificar que están en [0, 1]

---

## Notas Adicionales

### Compatibilidad con Base de Datos Existente

Los cambios propuestos son **retrocompatibles**:
- No requieren re-indexación
- Los embeddings existentes siguen siendo válidos
- Solo cambia cómo se procesan los queries y se calculan los scores

### Consideraciones de Modelos

Si se desea mejorar aún más la calidad con código Pascal/Delphi:

1. **Modelos alternativos a considerar:**
   - `nomic-embed-text` - Más general
   - `mxbai-embed-large` - Buen balance
   - Fine-tuning de modelo existente con código Pascal

2. **Para código en español:**
   - Considerar modelos multilingües
   - O traducir queries al inglés antes de embedding

---

## Referencias

- **sqlite-vec documentation**: https://github.com/asg017/sqlite-vec
- **Jina Embeddings**: https://jina.ai/embeddings/
- **Cosine vs L2 for normalized vectors**: Los vectores normalizados tienen norma 1, lo que hace que cosine similarity sea equivalente al producto punto y más eficiente que L2.
