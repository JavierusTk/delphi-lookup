#!/bin/bash
# Vector Search Benchmark Runner
# Ejecuta queries del dataset y evalúa precisión

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
DATASET="$SCRIPT_DIR/benchmark-dataset.json"
RESULTS_DIR="$SCRIPT_DIR/results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# Crear directorio de resultados
mkdir -p "$RESULTS_DIR"

# Archivos de salida
RESULTS_JSON="$RESULTS_DIR/benchmark-results-$TIMESTAMP.json"
SUMMARY_TXT="$RESULTS_DIR/benchmark-summary-$TIMESTAMP.txt"

# Colores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo " Vector Search Benchmark"
echo "=========================================="
echo "Dataset: $DATASET"
echo "Results: $RESULTS_DIR"
echo ""

# Verificar que existe RepoSearch
if [[ ! -f "$REPO_DIR/RepoSearch.exe" ]]; then
    echo -e "${RED}ERROR: RepoSearch.exe no encontrado en $REPO_DIR${NC}"
    exit 1
fi

# Verificar dataset
if [[ ! -f "$DATASET" ]]; then
    echo -e "${RED}ERROR: Dataset no encontrado: $DATASET${NC}"
    exit 1
fi

# Parsear argumentos
MODE_FILTER=""
CATEGORY_FILTER=""
NUM_RESULTS=10

while [[ $# -gt 0 ]]; do
    case $1 in
        --mode)
            MODE_FILTER="$2"
            shift 2
            ;;
        --category)
            CATEGORY_FILTER="$2"
            shift 2
            ;;
        -n)
            NUM_RESULTS="$2"
            shift 2
            ;;
        --help)
            echo "Uso: $0 [--mode fts5|fts5_vector|fts5_vector_reranker] [--category 1-5] [-n NUM]"
            exit 0
            ;;
        *)
            echo "Opción desconocida: $1"
            exit 1
            ;;
    esac
done

# Función para ejecutar query y capturar resultados
run_query() {
    local query="$1"
    local mode_args="$2"
    local n="$3"

    cd "$REPO_DIR"
    ./RepoSearch.exe "$query" -n "$n" $mode_args 2>/dev/null || echo "ERROR"
}

# Función para evaluar si un resultado coincide con patrones esperados
matches_pattern() {
    local result_name="$1"
    local patterns="$2"

    # Convertir patrones JSON array a lista
    for pattern in $(echo "$patterns" | jq -r '.[]' 2>/dev/null); do
        # Convertir patrón glob a regex
        regex=$(echo "$pattern" | sed 's/\*/.*/g')
        if echo "$result_name" | grep -qiE "$regex"; then
            return 0
        fi
    done
    return 1
}

# Inicializar JSON de resultados
echo "{" > "$RESULTS_JSON"
echo '  "timestamp": "'$TIMESTAMP'",' >> "$RESULTS_JSON"
echo '  "config": {' >> "$RESULTS_JSON"
echo '    "num_results": '$NUM_RESULTS',' >> "$RESULTS_JSON"
echo '    "mode_filter": "'$MODE_FILTER'",' >> "$RESULTS_JSON"
echo '    "category_filter": "'$CATEGORY_FILTER'"' >> "$RESULTS_JSON"
echo '  },' >> "$RESULTS_JSON"
echo '  "results": [' >> "$RESULTS_JSON"

# Contadores globales para métricas
declare -A total_relevant
declare -A total_results
declare -A hit_count
declare -A rr_sum
declare -A query_count

# Modos a evaluar
MODES=("fts5:" "fts5_vector:--enable-semantic" "fts5_vector_reranker:--enable-semantic --use-reranker")

# Filtrar modos si se especificó
if [[ -n "$MODE_FILTER" ]]; then
    MODES=()
    case $MODE_FILTER in
        fts5)
            MODES=("fts5:")
            ;;
        fts5_vector)
            MODES=("fts5_vector:--enable-semantic")
            ;;
        fts5_vector_reranker)
            MODES=("fts5_vector_reranker:--enable-semantic --use-reranker")
            ;;
    esac
fi

# Inicializar contadores
for mode_entry in "${MODES[@]}"; do
    mode_name="${mode_entry%%:*}"
    total_relevant[$mode_name]=0
    total_results[$mode_name]=0
    hit_count[$mode_name]=0
    rr_sum[$mode_name]=0
    query_count[$mode_name]=0
done

# Procesar cada query del dataset
FIRST_RESULT=true
QUERY_COUNT=$(jq '.queries | length' "$DATASET")

echo "Procesando $QUERY_COUNT queries..."
echo ""

for i in $(seq 0 $((QUERY_COUNT - 1))); do
    # Extraer datos de la query
    query_data=$(jq ".queries[$i]" "$DATASET")
    query_id=$(echo "$query_data" | jq -r '.id')
    query_category=$(echo "$query_data" | jq -r '.category')
    query_text=$(echo "$query_data" | jq -r '.query')
    expected_patterns=$(echo "$query_data" | jq '.expected_patterns')

    # Filtrar por categoría si se especificó
    if [[ -n "$CATEGORY_FILTER" && "$query_category" != "$CATEGORY_FILTER" ]]; then
        continue
    fi

    echo -n "[$query_id] $query_text ... "

    # Ejecutar con cada modo
    for mode_entry in "${MODES[@]}"; do
        mode_name="${mode_entry%%:*}"
        mode_args="${mode_entry#*:}"

        # Ejecutar query
        output=$(run_query "$query_text" "$mode_args" "$NUM_RESULTS")

        if [[ "$output" == "ERROR" ]]; then
            echo -e "${RED}ERROR${NC}"
            continue
        fi

        # Parsear resultados (asumiendo formato: nombre | tipo | archivo)
        relevant_count=0
        first_relevant_pos=0
        result_num=0

        while IFS= read -r line; do
            # Solo procesar líneas que contienen resultados
            # Formato: // Result N (Match Type - CATEGORY): SymbolName (type)
            if [[ "$line" =~ ^"// Result "[0-9]+" " ]]; then
                result_num=$((result_num + 1))

                # Extraer nombre del símbolo: texto entre ": " y " ("
                # Ejemplo: "// Result 1 (Content Match - USER CODE): TdmExportar (type)"
                # Extraemos: TdmExportar
                symbol_name=$(echo "$line" | sed -n 's/.*): \([^ ]*\) (.*/\1/p')

                # Fallback: si no tiene (type) al final
                if [[ -z "$symbol_name" ]]; then
                    symbol_name=$(echo "$line" | sed -n 's/.*): \(.*\)/\1/p')
                fi

                # Verificar si coincide con patrones esperados
                if matches_pattern "$symbol_name" "$expected_patterns"; then
                    relevant_count=$((relevant_count + 1))
                    if [[ $first_relevant_pos -eq 0 ]]; then
                        first_relevant_pos=$result_num
                    fi
                fi
            fi
        done <<< "$output"

        # Actualizar métricas
        total_relevant[$mode_name]=$((${total_relevant[$mode_name]} + relevant_count))
        total_results[$mode_name]=$((${total_results[$mode_name]} + result_num))
        query_count[$mode_name]=$((${query_count[$mode_name]} + 1))

        if [[ $relevant_count -gt 0 ]]; then
            hit_count[$mode_name]=$((${hit_count[$mode_name]} + 1))
        fi

        if [[ $first_relevant_pos -gt 0 ]]; then
            # Calcular reciprocal rank (como entero * 1000 para evitar decimales en bash)
            rr=$((1000 / first_relevant_pos))
            rr_sum[$mode_name]=$((${rr_sum[$mode_name]} + rr))
        fi

        # Escribir resultado JSON
        if [[ "$FIRST_RESULT" != "true" ]]; then
            echo "," >> "$RESULTS_JSON"
        fi
        FIRST_RESULT=false

        cat >> "$RESULTS_JSON" << EOF
    {
      "query_id": "$query_id",
      "mode": "$mode_name",
      "query": "$query_text",
      "total_results": $result_num,
      "relevant_results": $relevant_count,
      "first_relevant_position": $first_relevant_pos
    }
EOF
    done

    echo -e "${GREEN}OK${NC}"
done

# Cerrar JSON
echo "" >> "$RESULTS_JSON"
echo "  ]," >> "$RESULTS_JSON"

# Calcular y escribir métricas finales
echo '  "summary": {' >> "$RESULTS_JSON"

FIRST_MODE=true
for mode_entry in "${MODES[@]}"; do
    mode_name="${mode_entry%%:*}"

    if [[ ${query_count[$mode_name]} -eq 0 ]]; then
        continue
    fi

    # Precision@K
    if [[ ${total_results[$mode_name]} -gt 0 ]]; then
        precision=$((${total_relevant[$mode_name]} * 100 / ${total_results[$mode_name]}))
    else
        precision=0
    fi

    # Hit Rate
    hit_rate=$((${hit_count[$mode_name]} * 100 / ${query_count[$mode_name]}))

    # MRR (Mean Reciprocal Rank) - escalado por 1000
    if [[ ${query_count[$mode_name]} -gt 0 ]]; then
        mrr=$((${rr_sum[$mode_name]} / ${query_count[$mode_name]}))
    else
        mrr=0
    fi

    if [[ "$FIRST_MODE" != "true" ]]; then
        echo "," >> "$RESULTS_JSON"
    fi
    FIRST_MODE=false

    cat >> "$RESULTS_JSON" << EOF
    "$mode_name": {
      "queries_evaluated": ${query_count[$mode_name]},
      "precision_percent": $precision,
      "hit_rate_percent": $hit_rate,
      "mrr_scaled": $mrr
    }
EOF
done

echo "" >> "$RESULTS_JSON"
echo "  }" >> "$RESULTS_JSON"
echo "}" >> "$RESULTS_JSON"

# Generar resumen de texto
echo "=========================================" > "$SUMMARY_TXT"
echo " Vector Search Benchmark Results" >> "$SUMMARY_TXT"
echo " $TIMESTAMP" >> "$SUMMARY_TXT"
echo "=========================================" >> "$SUMMARY_TXT"
echo "" >> "$SUMMARY_TXT"

for mode_entry in "${MODES[@]}"; do
    mode_name="${mode_entry%%:*}"

    if [[ ${query_count[$mode_name]} -eq 0 ]]; then
        continue
    fi

    if [[ ${total_results[$mode_name]} -gt 0 ]]; then
        precision=$((${total_relevant[$mode_name]} * 100 / ${total_results[$mode_name]}))
    else
        precision=0
    fi
    hit_rate=$((${hit_count[$mode_name]} * 100 / ${query_count[$mode_name]}))

    echo "Mode: $mode_name" >> "$SUMMARY_TXT"
    echo "  Queries: ${query_count[$mode_name]}" >> "$SUMMARY_TXT"
    echo "  Precision@$NUM_RESULTS: $precision%" >> "$SUMMARY_TXT"
    echo "  Hit Rate@$NUM_RESULTS: $hit_rate%" >> "$SUMMARY_TXT"
    echo "" >> "$SUMMARY_TXT"
done

echo "" >> "$SUMMARY_TXT"
echo "Detailed results: $RESULTS_JSON" >> "$SUMMARY_TXT"

# Mostrar resumen
echo ""
echo "=========================================="
echo " Resultados"
echo "=========================================="
cat "$SUMMARY_TXT"

echo ""
echo -e "${GREEN}Benchmark completado${NC}"
echo "JSON: $RESULTS_JSON"
echo "Resumen: $SUMMARY_TXT"
