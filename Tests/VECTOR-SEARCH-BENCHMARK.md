# Vector Search Benchmark - Metodología y Dataset

## Objetivo

Evaluar objetivamente la precisión de la búsqueda vectorial (semántica) comparada con la búsqueda tradicional FTS5 para determinar si aporta valor suficiente para justificar su coste computacional.

## Metodología

### Tipos de evaluación

1. **Evaluación automatizada**: Script que ejecuta queries y compara con ground truth
2. **Métricas calculadas**: Precision@K, MRR, Hit Rate

### Criterios de relevancia

Un resultado se considera **relevante** si cumple AL MENOS UNO de estos criterios:

| Criterio | Descripción | Ejemplo |
|----------|-------------|---------|
| **Nombre coincide** | El nombre del símbolo contiene palabras clave de la query | Query: "calcular factura" → `CalcularTotalFactura` ✓ |
| **Funcionalidad relacionada** | El símbolo implementa la funcionalidad buscada | Query: "enviar email" → `SendMail` ✓ |
| **Sinónimo/traducción** | Símbolo en otro idioma pero misma funcionalidad | Query: "invoice total" → `CalcularFactura` ✓ |
| **Clase/módulo correcto** | Pertenece al módulo esperado aunque nombre difiera | Query: "stock control" → `TAlmacenModule.UpdateStock` ✓ |

Un resultado se considera **NO relevante** si:
- Es un símbolo genérico sin relación (ej: `TObject.Create`)
- Es un campo/propiedad sin contexto útil
- Es un símbolo de test o ejemplo
- Es un símbolo deprecado o interno sin uso

### Grados de relevancia

Para análisis más fino, usamos escala 0-2:

| Score | Significado |
|-------|-------------|
| **2** | Altamente relevante - responde directamente a la query |
| **1** | Parcialmente relevante - relacionado pero no exacto |
| **0** | No relevante |

## Métricas

### Precision@K
```
Precision@K = (Resultados relevantes en top K) / K
```

### Mean Reciprocal Rank (MRR)
```
MRR = Promedio de (1 / posición del primer resultado relevante)
```
Si no hay resultado relevante, RR = 0

### Hit Rate@K
```
Hit Rate@K = (Queries con al menos 1 relevante en top K) / Total queries
```

### Objetivos mínimos

| Métrica | Objetivo | Comentario |
|---------|----------|------------|
| Precision@5 | > 60% | Al menos 3 de 5 resultados útiles |
| Precision@10 | > 50% | Al menos 5 de 10 resultados útiles |
| MRR | > 0.5 | Primer resultado relevante en posición 2 o mejor |
| Hit Rate@5 | > 80% | 80% de queries encuentran algo útil |

## Categorías de Queries

### Categoría 1: Búsqueda conceptual (lenguaje natural)
Queries donde el usuario describe QUÉ quiere hacer, no el nombre del símbolo.

### Categoría 2: Búsqueda bilingüe
Queries en un idioma buscando código en otro.

### Categoría 3: Búsqueda por funcionalidad
El usuario sabe qué funcionalidad busca pero no conoce el nombre exacto.

### Categoría 4: Búsqueda por dominio de negocio
Términos de dominio específico del ERP (facturación, contabilidad, stock).

### Categoría 5: Queries abstractas
Casos donde la búsqueda textual tradicional no funcionaría bien.

## Dataset de Ground Truth

Ver archivo: `benchmark-dataset.json`

Formato:
```json
{
  "queries": [
    {
      "id": "C1-001",
      "category": 1,
      "query": "enviar email con archivo adjunto",
      "expected_symbols": ["SendMail", "EmailAttachment", ...],
      "expected_patterns": ["*Mail*", "*Email*", "*Attach*"],
      "notes": "Debe encontrar funciones de envío de correo"
    }
  ]
}
```

## Modos de búsqueda a comparar

| Modo | Comando | Descripción |
|------|---------|-------------|
| **Baseline (FTS5)** | `delphi-lookup.exe "query" -n 10` | Solo búsqueda textual |
| **FTS5 + Vector** | `delphi-lookup.exe "query" --enable-semantic -n 10` | Añade similitud vectorial |
| **FTS5 + Vector + Reranker** | `delphi-lookup.exe "query" --enable-semantic --use-reranker -n 10` | Con reordenamiento |

## Ejecución del Benchmark

### Prerrequisitos
1. Base de datos indexada con embeddings (`delphi_symbols.db`)
2. Ollama corriendo (para semantic search)
3. Reranker configurado (opcional)

### Comando
```bash
# Ejecutar evaluación completa
./run-benchmark.sh

# Solo una categoría
./run-benchmark.sh --category 1

# Solo un modo
./run-benchmark.sh --mode fts5
```

### Output
El script genera:
- `benchmark-results-YYYYMMDD.json`: Resultados detallados
- `benchmark-summary-YYYYMMDD.txt`: Resumen de métricas

## Interpretación de resultados

### Escenarios de decisión

| Resultado | Decisión |
|-----------|----------|
| Vector mejora >10% en todas las categorías | ✅ Mantener y promocionar |
| Vector mejora >10% solo en categorías 1,2,5 | ⚠️ Mantener, documentar casos de uso |
| Vector mejora <5% o empeora | ❌ Eliminar, simplificar sistema |
| Reranker mejora >15% sobre vector | ✅ Recomendar reranker por defecto |

### Análisis por categoría

Se espera que vector search aporte más valor en:
- **Categorías 1, 2, 5**: Búsqueda conceptual, bilingüe, abstracta
- Menos valor en **Categorías 3, 4**: donde FTS5 con buenos nombres ya funciona

---

Última actualización: 2024-12
