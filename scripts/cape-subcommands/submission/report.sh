#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# Cape Submission Report - Generate HTML reports with client-rendered horizontal bar charts
# Usage: cape submission report <benchmark>
#        cape submission report --all

# Resolve project root relative to this script (CWD-independent)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
# Prefer installed 'cape', fallback to repo script
if command -v cape > /dev/null 2>&1; then CAPE_CMD="cape"; else CAPE_CMD="$PROJECT_ROOT/scripts/cape.sh"; fi

# Help rendering via gomplate
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# IMPORTANT: log_info must go to stderr to avoid contaminating function return values
log_info() { cape_info "$1" >&2; }
log_warn() { cape_warn "$1"; }
log_err() { cape_error "$1"; }

cape_enable_tmp_cleanup

# gomplate infers data-source type from filename extension, so temp JSON files
# used as template contexts must be named *.json.
mktemp_json() {
  local f
  f="$(mktemp --suffix=.json)"
  cape_register_tmp "$f"
  printf '%s\n' "$f"
}

# Parse flags
DRY_RUN=0
KEEP_EXISTING=0
args=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h | --help | help)
      cape_render_help "${BASH_SOURCE[0]}"
      exit 0
      ;;
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    --keep-existing)
      KEEP_EXISTING=1
      shift
      ;;
    --all)
      args+=("--all")
      shift
      ;;
    *)
      args+=("$1")
      shift
      ;;
  esac
done
set -- "${args[@]}"

# Check required tools (gomplate renders index + benchmark HTML shells; jq builds JSON data files)
cape_require_cmds gomplate jq

# Verify repo structure
if [ ! -d "$PROJECT_ROOT/submissions" ]; then
  log_err "Must be run within the project (submissions directory not found)"
  echo "Detected PROJECT_ROOT=$PROJECT_ROOT" >&2
  exit 1
fi

# Validate benchmark name pattern when provided (lowercase, underscores only)
valid_benchmark_name() { [[ $1 =~ ^[a-z][a-z0-9_]*[a-z0-9]$|^[a-z]$ ]]; }

# CSV format: benchmark,timestamp,language,version,user,variant,cpu_units,memory_units,script_size_bytes,term_size,execution_fee_lovelace,reference_script_fee_lovelace,total_fee_lovelace,tx_memory_budget_pct,tx_cpu_budget_pct,block_memory_budget_pct,block_cpu_budget_pct,scripts_per_tx,scripts_per_block,submission_dir
declare -A CSV_COL=(
  [benchmark]=1
  [timestamp]=2
  [language]=3
  [version]=4
  [user]=5
  [variant]=6
  [cpu_units]=7
  [memory_units]=8
  [script_size_bytes]=9
  [term_size]=10
  [execution_fee_lovelace]=11
  [reference_script_fee_lovelace]=12
  [total_fee_lovelace]=13
  [tx_memory_budget_pct]=14
  [tx_cpu_budget_pct]=15
  [block_memory_budget_pct]=16
  [block_cpu_budget_pct]=17
  [scripts_per_tx]=18
  [scripts_per_block]=19
  [submission_dir]=20
)

# Helper function to extract a CSV field by column name
csv_field() {
  local line="$1"
  local col_name="$2"
  local col_num="${CSV_COL[$col_name]}"
  echo "$line" | cut -d, -f"$col_num"
}

# Prepare report directory
report_dir="$PROJECT_ROOT/report"
if [[ $KEEP_EXISTING -eq 0 ]]; then
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would remove existing '$report_dir'"
  else
    rm -rf "$report_dir"
  fi
fi
if [[ $DRY_RUN -eq 1 ]]; then
  log_info "[dry-run] Would create '$report_dir/benchmarks'"
  log_info "[dry-run] Would copy logo to '$report_dir/uplc-cape-logo.png'"
else
  mkdir -p "$report_dir/benchmarks"
  cp -f "$PROJECT_ROOT/uplc-cape-logo.png" "$report_dir/uplc-cape-logo.png"
fi

# Fetch CSV rows for one benchmark, dropping template placeholders and rows with empty core metrics.
fetch_benchmark_csv() {
  local benchmark="$1"
  local target_filter="${2:-}"
  local aggregate_args=() csv_data
  if [ -n "$target_filter" ]; then
    aggregate_args+=("--target=$target_filter")
  fi
  csv_data=$($CAPE_CMD submission aggregate "${aggregate_args[@]}" | grep "^$benchmark," || true)
  if [ -z "$csv_data" ]; then
    return 0
  fi
  echo "$csv_data" | grep -v '<.*>' |
    awk -F, \
      -v cpu="${CSV_COL[cpu_units]}" \
      -v mem="${CSV_COL[memory_units]}" \
      -v size="${CSV_COL[script_size_bytes]}" \
      -v term="${CSV_COL[term_size]}" \
      '$cpu != "" && $mem != "" && $size != "" && $term != ""'
}

# Emit `report/benchmarks/<benchmark>.json` — the data file read by the HTML chart renderer.
generate_benchmark_data_json() {
  local benchmark="$1"
  local output_dir="$2"
  local target_filter="${3:-}"

  local csv_data
  csv_data=$(fetch_benchmark_csv "$benchmark" "$target_filter")
  if [ -z "$csv_data" ]; then
    echo "No valid submissions found for benchmark: $benchmark" >&2
    return 1
  fi

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would ensure directory exists: $output_dir/benchmarks"
  else
    mkdir -p "$output_dir/benchmarks"
  fi

  local out_path="$output_dir/benchmarks/${benchmark}.json"
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would write $out_path"
    return 0
  fi

  # Build submissions array with jq (one line at a time → accumulate).
  local tmp_submissions
  tmp_submissions=$(mktemp_json)
  echo "[]" > "$tmp_submissions"

  while IFS= read -r line; do
    [ -n "$line" ] || continue
    local timestamp language version user variant
    local cpu memory script_size term_size
    local exec_fee ref_fee total_fee
    local tx_mem_pct tx_cpu_pct block_mem_pct block_cpu_pct
    local spt spb submission_dir

    timestamp=$(csv_field "$line" "timestamp")
    language=$(csv_field "$line" "language")
    version=$(csv_field "$line" "version")
    user=$(csv_field "$line" "user")
    variant=$(csv_field "$line" "variant")
    cpu=$(csv_field "$line" "cpu_units")
    memory=$(csv_field "$line" "memory_units")
    script_size=$(csv_field "$line" "script_size_bytes")
    term_size=$(csv_field "$line" "term_size")
    exec_fee=$(csv_field "$line" "execution_fee_lovelace")
    ref_fee=$(csv_field "$line" "reference_script_fee_lovelace")
    total_fee=$(csv_field "$line" "total_fee_lovelace")
    tx_mem_pct=$(csv_field "$line" "tx_memory_budget_pct")
    tx_cpu_pct=$(csv_field "$line" "tx_cpu_budget_pct")
    block_mem_pct=$(csv_field "$line" "block_memory_budget_pct")
    block_cpu_pct=$(csv_field "$line" "block_cpu_budget_pct")
    spt=$(csv_field "$line" "scripts_per_tx")
    spb=$(csv_field "$line" "scripts_per_block")
    submission_dir=$(csv_field "$line" "submission_dir")

    local next
    next=$(jq -n \
      --arg timestamp "$timestamp" \
      --arg compiler "$language" \
      --arg version "$version" \
      --arg user "$user" \
      --arg variant "$variant" \
      --arg submission_dir "$submission_dir" \
      --argjson cpu "${cpu:-null}" \
      --argjson memory "${memory:-null}" \
      --argjson script_size "${script_size:-null}" \
      --argjson term_size "${term_size:-null}" \
      --argjson exec_fee "${exec_fee:-null}" \
      --argjson ref_fee "${ref_fee:-null}" \
      --argjson total_fee "${total_fee:-null}" \
      --argjson tx_mem_pct "${tx_mem_pct:-null}" \
      --argjson tx_cpu_pct "${tx_cpu_pct:-null}" \
      --argjson block_mem_pct "${block_mem_pct:-null}" \
      --argjson block_cpu_pct "${block_cpu_pct:-null}" \
      --argjson spt "${spt:-null}" \
      --argjson spb "${spb:-null}" \
      '{
        timestamp: $timestamp,
        compiler: $compiler,
        version: $version,
        user: $user,
        variant: $variant,
        submission_dir: $submission_dir,
        metrics: {
          cpu_units: $cpu,
          memory_units: $memory,
          script_size_bytes: $script_size,
          term_size: $term_size,
          execution_fee_lovelace: $exec_fee,
          reference_script_fee_lovelace: $ref_fee,
          total_fee_lovelace: $total_fee,
          tx_memory_budget_pct: $tx_mem_pct,
          tx_cpu_budget_pct: $tx_cpu_pct,
          block_memory_budget_pct: $block_mem_pct,
          block_cpu_budget_pct: $block_cpu_pct,
          scripts_per_tx: $spt,
          scripts_per_block: $spb
        }
      }')
    jq --argjson item "$next" '. + [$item]' "$tmp_submissions" > "$tmp_submissions.new"
    mv "$tmp_submissions.new" "$tmp_submissions"
  done <<< "$csv_data"

  jq -n \
    --arg benchmark "$benchmark" \
    --arg generated_at "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" \
    --slurpfile submissions "$tmp_submissions" \
    '{benchmark: $benchmark, generated_at: $generated_at, submissions: $submissions[0]}' \
    > "$out_path"
}

# Render `report/benchmarks/<benchmark>.html` — the static shell that fetches the JSON above.
generate_individual_benchmark_report() {
  local benchmark="$1"
  local output_dir="$2"
  local template_name="${3:-benchmark.html.tmpl}"

  local abs_template_path="$SCRIPT_DIR/$template_name"
  local abs_output_path="$output_dir/benchmarks/${benchmark}.html"

  local temp_json
  temp_json=$(mktemp_json)
  cat > "$temp_json" << EOF
{
  "benchmark": "$benchmark",
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')"
}
EOF

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $abs_output_path using template $abs_template_path"
    return 0
  fi

  if ! gomplate -f "$abs_template_path" -c .="$temp_json" > "$abs_output_path"; then
    echo "ERROR: Template rendering failed" >&2
    exit 1
  fi
}

# Build filtered benchmark stats from CSV data (consumed by index.html.tmpl).
build_filtered_stats() {
  local target_filter="${1:-}"
  local csv_data
  local aggregate_args=()
  if [ -n "$target_filter" ]; then
    aggregate_args+=("--target=$target_filter")
  fi
  csv_data=$($CAPE_CMD submission aggregate "${aggregate_args[@]}")

  echo '{'
  echo '  "generated_at": "'$(date -u +"%Y-%m-%dT%H:%M:%SZ")'",'
  echo '  "benchmarks": ['

  local benchmarks
  benchmarks=$(echo "$csv_data" | tail -n +2 | awk -F, -v col="${CSV_COL[benchmark]}" '{print $col}' | sort -u)

  local first_benchmark=true
  local bench
  for bench in $benchmarks; do
    local benchmark_csv
    benchmark_csv=$(echo "$csv_data" | grep "^$bench,")

    local submission_count
    submission_count=$(echo "$benchmark_csv" | wc -l)

    if [ "$first_benchmark" = "false" ]; then
      echo ','
    fi
    first_benchmark=false

    local category winners_json
    category=$(categorize_scenario "$benchmark")
    winners_json=$(find_winners "$benchmark_csv")

    echo '    {'
    echo '      "name": "'$benchmark'",'
    echo '      "category": "'$category'",'
    echo '      "submission_count": '$submission_count','
    echo '      "winners": '$winners_json','
    echo '      "submissions": ['

    local first_submission=true
    while IFS= read -r line; do
      if [ -z "$line" ]; then continue; fi

      local timestamp language version user variant cpu_units memory_units script_size term_size execution_fee_lovelace reference_script_fee_lovelace total_fee_lovelace tx_memory_budget_pct tx_cpu_budget_pct block_memory_budget_pct block_cpu_budget_pct scripts_per_tx scripts_per_block
      timestamp=$(csv_field "$line" "timestamp")
      language=$(csv_field "$line" "language")
      version=$(csv_field "$line" "version")
      user=$(csv_field "$line" "user")
      variant=$(csv_field "$line" "variant")
      cpu_units=$(csv_field "$line" "cpu_units")
      memory_units=$(csv_field "$line" "memory_units")
      script_size=$(csv_field "$line" "script_size_bytes")
      term_size=$(csv_field "$line" "term_size")
      execution_fee_lovelace=$(csv_field "$line" "execution_fee_lovelace")
      reference_script_fee_lovelace=$(csv_field "$line" "reference_script_fee_lovelace")
      total_fee_lovelace=$(csv_field "$line" "total_fee_lovelace")
      tx_memory_budget_pct=$(csv_field "$line" "tx_memory_budget_pct")
      tx_cpu_budget_pct=$(csv_field "$line" "tx_cpu_budget_pct")
      block_memory_budget_pct=$(csv_field "$line" "block_memory_budget_pct")
      block_cpu_budget_pct=$(csv_field "$line" "block_cpu_budget_pct")
      scripts_per_tx=$(csv_field "$line" "scripts_per_tx")
      scripts_per_block=$(csv_field "$line" "scripts_per_block")

      local date_only
      date_only=$(echo "$timestamp" | cut -d'T' -f1)

      local cpu_formatted memory_formatted total_fee_ada
      cpu_formatted=$(format_number_short "$cpu_units")
      memory_formatted=$(format_number_short "$memory_units")
      total_fee_ada=$(awk -v lovelace="$total_fee_lovelace" 'BEGIN{printf "%.6f", lovelace/1000000}')

      if [ "$first_submission" = "false" ]; then
        echo ','
      fi
      first_submission=false

      cat << EOF
        {
          "compiler": "$language",
          "version": "$version",
          "variant": "$variant",
          "author": "$user",
          "date": "$date_only",
          "timestamp": "$timestamp",
          "cpu_units": {
            "value": $cpu_units,
            "formatted": "$cpu_formatted",
            "is_best": false
          },
          "memory_units": {
            "value": $memory_units,
            "formatted": "$memory_formatted",
            "is_best": false
          },
          "script_size": {
            "value": $script_size,
            "formatted": "$script_size",
            "is_best": false
          },
          "term_size": {
            "value": $term_size,
            "formatted": "$term_size",
            "is_best": false
          },
          "total_fee_lovelace": {
            "value": $total_fee_lovelace,
            "formatted": "$total_fee_lovelace",
            "is_best": false
          },
          "total_fee_ada": {
            "value": $total_fee_ada,
            "formatted": "$total_fee_ada",
            "is_best": false
          },
          "tx_memory_budget_pct": $tx_memory_budget_pct,
          "tx_cpu_budget_pct": $tx_cpu_budget_pct,
          "block_memory_budget_pct": $block_memory_budget_pct,
          "block_cpu_budget_pct": $block_cpu_budget_pct,
          "scripts_per_tx": $scripts_per_tx,
          "scripts_per_block": $scripts_per_block
        }
EOF
    done <<< "$benchmark_csv"

    echo ''
    echo '      ]'
    echo '    }'
  done

  echo ''
  echo '  ]'
  echo '}'
}

format_number_short() {
  local num="$1"
  if [ -z "$num" ] || [ "$num" = "null" ] || [ "$num" = "" ]; then
    echo "0"
    return
  fi
  echo "$num" | awk '{
    if ($1 >= 1000000000) {
      printf "%.2fB", $1/1000000000
    } else if ($1 >= 1000000) {
      printf "%.2fM", $1/1000000
    } else if ($1 >= 1000) {
      printf "%.2fK", $1/1000
    } else {
      print $1
    }
  }'
}

# Category ∈ {fixed, open}, taken from YAML frontmatter of the scenario spec.
categorize_scenario() {
  local scenario="$1"
  local scenario_file="$PROJECT_ROOT/scenarios/$scenario/${scenario}.md"

  if [ ! -f "$scenario_file" ]; then
    log_err "Scenario file not found: $scenario_file"
    echo "ERROR: Cannot categorize scenario '$scenario' - specification file missing" >&2
    exit 1
  fi

  local category
  category=$(awk 'BEGIN{in_fm=0} /^---$/ {in_fm++; next} in_fm==1 && /^category:/ {print $2; exit}' "$scenario_file")

  if [ -z "$category" ]; then
    log_err "Missing 'category' attribute in YAML frontmatter for scenario: $scenario"
    echo "ERROR: Scenario '$scenario' must have 'category: fixed' or 'category: open' in $scenario_file" >&2
    exit 1
  fi

  if [ "$category" != "fixed" ] && [ "$category" != "open" ]; then
    log_err "Invalid category '$category' for scenario: $scenario"
    echo "ERROR: Scenario category must be 'fixed' or 'open', got: '$category'" >&2
    exit 1
  fi

  echo "$category"
}

# Returns JSON with winners for each of the four headline metrics (handles ties).
find_winners() {
  local benchmark_csv="$1"

  build_winners_json() {
    local metric_col="$1"
    local metric_name="$2"

    local best_value
    best_value=$(echo "$benchmark_csv" | awk -F, -v col="$metric_col" '{print $col}' | sort -n | head -1)

    if [ -z "$best_value" ]; then
      echo "null"
      return
    fi

    local tied_submissions
    tied_submissions=$(echo "$benchmark_csv" | awk -F, -v col="$metric_col" -v val="$best_value" '$col == val')

    local total_count
    total_count=$(echo "$tied_submissions" | wc -l)

    echo -n "{\"value\":$best_value,\"formatted\":\"$(format_number_short "$best_value")\",\"total\":$total_count,\"submissions\":["

    local first=true
    while IFS= read -r line; do
      [ -z "$line" ] && continue

      local compiler version variant author
      compiler=$(csv_field "$line" "language")
      version=$(csv_field "$line" "version")
      variant=$(csv_field "$line" "variant")
      author=$(csv_field "$line" "user")

      if [ "$first" = "false" ]; then
        echo -n ","
      fi
      first=false

      echo -n "{\"compiler\":\"$compiler\",\"version\":\"$version\",\"variant\":\"$variant\",\"author\":\"$author\"}"
    done <<< "$tied_submissions"

    echo -n "]}"
  }

  local cpu_json memory_json script_json term_json
  cpu_json=$(build_winners_json "${CSV_COL[cpu_units]}" "cpu")
  memory_json=$(build_winners_json "${CSV_COL[memory_units]}" "memory")
  script_json=$(build_winners_json "${CSV_COL[script_size_bytes]}" "script_size")
  term_json=$(build_winners_json "${CSV_COL[term_size]}" "term_size")

  echo "{\"cpu\":$cpu_json,\"memory\":$memory_json,\"script_size\":$script_json,\"term_size\":$term_json}"
}

generate_index_report() {
  local output_dir="$1"
  local benchmark_list="$2"
  local target_filter="${3:-}"
  local template_name="${4:-index.html.tmpl}"

  local temp_json temp_stats
  temp_json=$(mktemp_json)
  temp_stats=$(mktemp_json)

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would generate benchmark stats"
    echo '{"benchmarks":[]}' > "$temp_stats"
  else
    if [ -n "$target_filter" ]; then
      build_filtered_stats "$target_filter" > "$temp_stats"
    else
      build_filtered_stats > "$temp_stats"
    fi
  fi

  cat > "$temp_json" << EOF
{
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')",
  "benchmarks": [
EOF

  local first=true
  while read -r benchmark; do
    if [ -n "$benchmark" ]; then
      if [ "$first" = "false" ]; then
        echo "," >> "$temp_json"
      fi
      first=false
      echo "    \"$benchmark\"" >> "$temp_json"
    fi
  done <<< "$benchmark_list"

  cat >> "$temp_json" << EOF

  ],
  "stats": $(cat "$temp_stats")
}
EOF

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/index.html using template $SCRIPT_DIR/$template_name"
  else
    if ! gomplate -f "$SCRIPT_DIR/$template_name" -c .="$temp_json" > "$output_dir/index.html"; then
      echo "ERROR: Index template rendering failed" >&2
      exit 1
    fi
  fi
}

generate_no_submissions_report() {
  local benchmark="$1"
  local output_dir="$2"

  local temp_json
  temp_json=$(mktemp_json)
  cat > "$temp_json" << EOF
{
  "benchmark": "$benchmark",
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')"
}
EOF

  local abs_template_path="$SCRIPT_DIR/benchmark-no-submissions.html.tmpl"
  local abs_output_path="$output_dir/benchmarks/${benchmark}.html"

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $abs_output_path using template $abs_template_path"
  else
    mkdir -p "$output_dir/benchmarks"
    if ! gomplate -f "$abs_template_path" -c .="$temp_json" > "$abs_output_path"; then
      echo "ERROR: Template rendering failed for no-submissions report" >&2
      exit 1
    fi
  fi
}

# Build compiler comparison stats for preview report (compiler-centric).
# Correlates production and preview submissions by (benchmark, compiler, author),
# computes delta percentages. Outputs JSON to stdout.
build_compiler_comparison_stats() {
  local current_csv preview_csv
  current_csv=$($CAPE_CMD submission aggregate --target=current 2>/dev/null | tail -n +2)
  preview_csv=$($CAPE_CMD submission aggregate --target=preview 2>/dev/null | tail -n +2)

  if [ -z "$preview_csv" ]; then
    echo '{"generated_at":"'"$(date -u +"%Y-%m-%dT%H:%M:%SZ")"'","compilers":[]}'
    return
  fi

  local result='[]'

  local compiler_keys
  compiler_keys=$(echo "$preview_csv" | awk -F, '{print $3 "," $5}' | sort -u)

  while IFS=',' read -r compiler author; do
    [ -n "$compiler" ] || continue

    local preview_rows prod_version preview_version scenarios_json='[]'
    preview_rows=$(echo "$preview_csv" | awk -F, -v c="$compiler" -v a="$author" '$3==c && $5==a')
    preview_version=$(echo "$preview_rows" | head -1 | cut -d, -f4)

    prod_version=$(echo "$current_csv" | awk -F, -v c="$compiler" -v a="$author" '$3==c && $5==a {print $4; exit}')

    local scenario_count=0

    while IFS= read -r preview_row; do
      [ -n "$preview_row" ] || continue
      local benchmark p_cpu p_mem p_size p_term p_fee
      benchmark=$(echo "$preview_row" | cut -d, -f1)
      p_cpu=$(echo "$preview_row" | cut -d, -f7)
      p_mem=$(echo "$preview_row" | cut -d, -f8)
      p_size=$(echo "$preview_row" | cut -d, -f9)
      p_term=$(echo "$preview_row" | cut -d, -f10)
      p_fee=$(echo "$preview_row" | cut -d, -f13)

      local prod_row c_cpu c_mem c_size c_term c_fee
      prod_row=$(echo "$current_csv" | awk -F, -v b="$benchmark" -v c="$compiler" -v a="$author" '$1==b && $3==c && $5==a' | head -1)

      if [ -n "$prod_row" ]; then
        c_cpu=$(echo "$prod_row" | cut -d, -f7)
        c_mem=$(echo "$prod_row" | cut -d, -f8)
        c_size=$(echo "$prod_row" | cut -d, -f9)
        c_term=$(echo "$prod_row" | cut -d, -f10)
        c_fee=$(echo "$prod_row" | cut -d, -f13)
      else
        c_cpu="" c_mem="" c_size="" c_term="" c_fee=""
      fi

      local d_cpu d_mem d_size d_term d_fee
      if [ -n "$c_cpu" ] && [ "$c_cpu" != "0" ]; then
        d_cpu=$(awk "BEGIN{printf \"%.1f\", (($p_cpu - $c_cpu) / $c_cpu) * 100}")
        d_mem=$(awk "BEGIN{printf \"%.1f\", (($p_mem - $c_mem) / $c_mem) * 100}")
        d_fee=$(awk "BEGIN{printf \"%.1f\", (($p_fee - $c_fee) / $c_fee) * 100}")
      else
        d_cpu="null" d_mem="null" d_fee="null"
      fi
      if [ -n "$c_size" ] && [ "$c_size" != "0" ]; then
        d_size=$(awk "BEGIN{printf \"%.1f\", (($p_size - $c_size) / $c_size) * 100}")
        d_term=$(awk "BEGIN{printf \"%.1f\", (($p_term - $c_term) / $c_term) * 100}")
      else
        d_size="null" d_term="null"
      fi

      scenarios_json=$(echo "$scenarios_json" | jq \
        --arg name "$benchmark" \
        --argjson p_cpu "${p_cpu:-0}" --argjson p_mem "${p_mem:-0}" \
        --argjson p_size "${p_size:-0}" --argjson p_term "${p_term:-0}" \
        --argjson p_fee "${p_fee:-0}" \
        --argjson c_cpu "${c_cpu:-null}" --argjson c_mem "${c_mem:-null}" \
        --argjson c_size "${c_size:-null}" --argjson c_term "${c_term:-null}" \
        --argjson c_fee "${c_fee:-null}" \
        --argjson d_cpu "${d_cpu}" --argjson d_mem "${d_mem}" \
        --argjson d_size "${d_size}" --argjson d_term "${d_term}" \
        --argjson d_fee "${d_fee}" \
        '. + [{
          name: $name,
          prod: {cpu_units: $c_cpu, memory_units: $c_mem, script_size_bytes: $c_size, term_size: $c_term, total_fee_lovelace: $c_fee},
          preview: {cpu_units: $p_cpu, memory_units: $p_mem, script_size_bytes: $p_size, term_size: $p_term, total_fee_lovelace: $p_fee},
          deltas: {cpu_units_pct: $d_cpu, memory_units_pct: $d_mem, script_size_pct: $d_size, term_size_pct: $d_term, total_fee_pct: $d_fee}
        }]')
      scenario_count=$((scenario_count + 1))
    done <<< "$preview_rows"

    result=$(echo "$result" | jq \
      --arg compiler "$compiler" --arg author "$author" \
      --arg prod_version "${prod_version:-N/A}" --arg preview_version "$preview_version" \
      --argjson scenario_count "$scenario_count" \
      --argjson scenarios "$scenarios_json" \
      '. + [{
        compiler: $compiler, author: $author,
        prod_version: $prod_version, preview_version: $preview_version,
        scenario_count: $scenario_count, scenarios: $scenarios
      }]')
  done <<< "$compiler_keys"

  jq -n --argjson compilers "$result" \
    '{generated_at: "'"$(date -u +"%Y-%m-%dT%H:%M:%SZ")"'", compilers: $compilers}'
}

# ─── Entry points ──────────────────────────────────────────────────────
if [ $# -eq 0 ]; then
  log_err "No arguments provided"
  echo "Use 'cape submission report --help' for usage information" >&2
  exit 1
fi

if [ "$1" = "--all" ]; then
  echo "🚀 Generating HTML Performance Reports - All Benchmarks"
  echo "========================================================="

  all_benchmarks=$(find "$PROJECT_ROOT/scenarios" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" -exec basename {} \; 2> /dev/null | sort)

  if [ -z "$all_benchmarks" ]; then
    echo "No benchmark scenarios found" >&2
    exit 1
  fi

  benchmarks_with_submissions=""

  for benchmark in $all_benchmarks; do
    if [ -d "$PROJECT_ROOT/submissions/$benchmark" ] && [ "$(find "$PROJECT_ROOT/submissions/$benchmark" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" 2> /dev/null | wc -l)" -gt 0 ]; then
      echo "Processing $benchmark..."

      if ! generate_benchmark_data_json "$benchmark" "$report_dir" "current"; then
        echo "❌ Error: Failed to build data JSON for benchmark '$benchmark'" >&2
        exit 1
      fi

      if [[ $DRY_RUN -eq 0 ]]; then
        generate_individual_benchmark_report "$benchmark" "$report_dir"
      fi

      if [ -n "$benchmarks_with_submissions" ]; then
        benchmarks_with_submissions="${benchmarks_with_submissions}\n"
      fi
      benchmarks_with_submissions="${benchmarks_with_submissions}${benchmark}"
    else
      echo "No submissions for $benchmark, creating placeholder..."
      generate_no_submissions_report "$benchmark" "$report_dir"
    fi
  done

  if [ -n "$benchmarks_with_submissions" ]; then
    generate_index_report "$report_dir" "$(echo -e "$benchmarks_with_submissions")" "current"
    echo ""
    echo "✅ Current HTML reports generated:"
    echo "   📄 $report_dir/index.html (main index)"
    echo "   📊 Individual benchmark reports in $report_dir/benchmarks/"
    echo "   📦 Benchmark data JSON in $report_dir/benchmarks/<scenario>.json"
  else
    echo "Error: No submissions found for report generation" >&2
    exit 1
  fi

  echo ""
  echo "🔮 Generating Preview Reports (compiler-centric)..."
  preview_dir="$report_dir/preview"
  if [[ $DRY_RUN -eq 0 ]]; then
    rm -rf "$preview_dir"
    mkdir -p "$preview_dir/compilers"
    cp -f "$PROJECT_ROOT/uplc-cape-logo.png" "$preview_dir/uplc-cape-logo.png"
  fi

  if [[ $DRY_RUN -eq 1 ]]; then
    echo "  [dry-run] Would generate compiler-centric preview report"
  else
    temp_compiler_stats=$(mktemp_json)
    build_compiler_comparison_stats > "$temp_compiler_stats"

    compiler_count=$(jq '.compilers | length' "$temp_compiler_stats")

    if [ "$compiler_count" -gt 0 ]; then
      temp_index_json=$(mktemp_json)
      jq --arg ts "$(date '+%Y-%m-%d %H:%M:%S %Z')" \
        '{timestamp: $ts, compilers: .compilers}' "$temp_compiler_stats" > "$temp_index_json"
      gomplate -f "$SCRIPT_DIR/index-preview.html.tmpl" -c .="$temp_index_json" > "$preview_dir/index.html"

      for i in $(seq 0 $((compiler_count - 1))); do
        compiler_name=$(jq -r ".compilers[$i].compiler" "$temp_compiler_stats")
        author=$(jq -r ".compilers[$i].author" "$temp_compiler_stats")

        temp_compiler_json=$(mktemp_json)
        jq --arg ts "$(date '+%Y-%m-%d %H:%M:%S %Z')" \
          ".compilers[$i] + {timestamp: \$ts}" "$temp_compiler_stats" > "$temp_compiler_json"
        gomplate -f "$SCRIPT_DIR/compiler-preview.html.tmpl" -c .="$temp_compiler_json" \
          > "$preview_dir/compilers/${compiler_name}_${author}.html"

        echo "  Preview: $compiler_name by $author ($(jq -r ".compilers[$i].scenario_count" "$temp_compiler_stats") scenarios)"
      done

      echo ""
      echo "✅ Preview HTML reports generated:"
      echo "   📄 $preview_dir/index.html (compiler-centric index)"
      echo "   📊 Compiler detail pages in $preview_dir/compilers/"
    else
      echo "  (No preview submissions found — preview report skipped)"
    fi
  fi

  echo ""
  echo "Open the main report with: xdg-open $report_dir/index.html 2>/dev/null || echo \"Open: $report_dir/index.html\""
else
  benchmark="$1"
  if ! valid_benchmark_name "$benchmark"; then
    log_err "Invalid benchmark name: '$benchmark'"
    echo "Expected pattern: lowercase with optional underscores (e.g., two_party_escrow)" >&2
    exit 1
  fi

  if [ ! -d "$PROJECT_ROOT/scenarios/$benchmark" ]; then
    echo "Error: Benchmark scenario '$benchmark' not found" >&2
    echo "Available benchmarks:" >&2
    find "$PROJECT_ROOT/scenarios" -mindepth 1 -maxdepth 1 -type d \
      -printf '%f\n' | grep -v '^TEMPLATE$' | sort >&2 || true
    exit 1
  fi

  echo "🚀 Generating HTML Performance Report - $benchmark"
  echo "================================================="

  if [ -d "$PROJECT_ROOT/submissions/$benchmark" ] && [ "$(find "$PROJECT_ROOT/submissions/$benchmark" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" 2> /dev/null | wc -l)" -gt 0 ]; then
    if ! generate_benchmark_data_json "$benchmark" "$report_dir"; then
      echo "❌ Error: Failed to build data JSON for benchmark '$benchmark'" >&2
      exit 1
    fi

    generate_individual_benchmark_report "$benchmark" "$report_dir"
  else
    echo "No submissions found for benchmark: $benchmark. Creating placeholder page."
    generate_no_submissions_report "$benchmark" "$report_dir"
  fi

  generate_index_report "$report_dir" "$benchmark"
  echo ""
  echo "✅ HTML reports generated:"
  echo "   📄 $report_dir/index.html (main index)"
  echo "   📊 Individual benchmark report: $report_dir/benchmarks/${benchmark}.html"
  echo "   📦 Benchmark data JSON: $report_dir/benchmarks/${benchmark}.json"
  echo ""
  echo "Open the main report with: xdg-open $report_dir/index.html 2>/dev/null || echo \"Open: $report_dir/index.html\""
fi
