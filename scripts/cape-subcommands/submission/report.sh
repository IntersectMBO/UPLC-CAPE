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

# CSV format: benchmark,timestamp,language,version,user,variant,cpu_units,memory_units,script_size_bytes,term_size,execution_fee_lovelace,reference_script_fee_lovelace,total_fee_lovelace,tx_memory_budget_pct,tx_cpu_budget_pct,block_memory_budget_pct,block_cpu_budget_pct,scripts_per_tx,scripts_per_block,submission_dir,min_protocol_version,excluded_count,excluded_cpu_sum,excluded_mem_sum
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
  [min_protocol_version]=21
  [excluded_count]=22
  [excluded_cpu_sum]=23
  [excluded_mem_sum]=24
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
  cp -f "$SCRIPT_DIR/report-comparison.js" "$report_dir/report-comparison.js"
fi

# Cached aggregate CSV per target filter, populated on first fetch. Lives in the
# outer shell — subshells (command substitution) can only read it, not mutate
# it, so we fill it eagerly via `aggregate_csv_cache` before entering any loop.
declare -A AGGREGATE_CSV=()

# Map a target filter to its cache key. The unfiltered case uses an empty
# filter string, but bash 5.3 rejects an empty associative-array subscript
# ("bad array subscript"), so fold it to a non-empty sentinel that can't
# collide with a real target name.
csv_cache_key() { printf '%s' "${1:-__unfiltered__}"; }

# Populate AGGREGATE_CSV[<key>] if not already set. Call from the outer
# shell before using fetch_benchmark_csv in a $(...) context.
aggregate_csv_cache() {
  local target_filter="${1:-}"
  local key
  key=$(csv_cache_key "$target_filter")
  if [ -n "${AGGREGATE_CSV[$key]+set}" ]; then
    return 0
  fi
  local aggregate_args=()
  if [ -n "$target_filter" ]; then
    aggregate_args+=("--target=$target_filter")
  fi
  AGGREGATE_CSV[$key]=$($CAPE_CMD submission aggregate "${aggregate_args[@]}")
}

# Fetch CSV rows for one benchmark, dropping template placeholders and rows with empty core metrics.
fetch_benchmark_csv() {
  local benchmark="$1"
  local target_filter="${2:-}"
  local key csv_data
  key=$(csv_cache_key "$target_filter")
  csv_data=$(printf '%s' "${AGGREGATE_CSV[$key]:-}" | grep "^$benchmark," || true)
  if [ -z "$csv_data" ]; then
    return 0
  fi
  echo "$csv_data" | grep -v '<.*>' \
    | awk -F, \
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
    local cpu memory
    local exec_fee total_fee
    local tx_mem_pct tx_cpu_pct block_mem_pct block_cpu_pct
    local spt spb submission_dir min_protocol_version
    local excluded_count excluded_cpu_sum excluded_mem_sum

    timestamp=$(csv_field "$line" "timestamp")
    language=$(csv_field "$line" "language")
    version=$(csv_field "$line" "version")
    user=$(csv_field "$line" "user")
    variant=$(csv_field "$line" "variant")
    cpu=$(csv_field "$line" "cpu_units")
    memory=$(csv_field "$line" "memory_units")
    exec_fee=$(csv_field "$line" "execution_fee_lovelace")
    total_fee=$(csv_field "$line" "total_fee_lovelace")
    tx_mem_pct=$(csv_field "$line" "tx_memory_budget_pct")
    tx_cpu_pct=$(csv_field "$line" "tx_cpu_budget_pct")
    block_mem_pct=$(csv_field "$line" "block_memory_budget_pct")
    block_cpu_pct=$(csv_field "$line" "block_cpu_budget_pct")
    spt=$(csv_field "$line" "scripts_per_tx")
    spb=$(csv_field "$line" "scripts_per_block")
    submission_dir=$(csv_field "$line" "submission_dir")
    min_protocol_version=$(csv_field "$line" "min_protocol_version")
    excluded_count=$(csv_field "$line" "excluded_count")
    excluded_cpu_sum=$(csv_field "$line" "excluded_cpu_sum")
    excluded_mem_sum=$(csv_field "$line" "excluded_mem_sum")

    local next
    next=$(jq -n \
      --arg timestamp "$timestamp" \
      --arg compiler "$language" \
      --arg version "$version" \
      --arg user "$user" \
      --arg variant "$variant" \
      --arg submission_dir "$submission_dir" \
      --argjson min_protocol_version "${min_protocol_version:-null}" \
      --argjson cpu "${cpu:-null}" \
      --argjson memory "${memory:-null}" \
      --argjson exec_fee "${exec_fee:-null}" \
      --argjson total_fee "${total_fee:-null}" \
      --argjson tx_mem_pct "${tx_mem_pct:-null}" \
      --argjson tx_cpu_pct "${tx_cpu_pct:-null}" \
      --argjson block_mem_pct "${block_mem_pct:-null}" \
      --argjson block_cpu_pct "${block_cpu_pct:-null}" \
      --argjson spt "${spt:-null}" \
      --argjson spb "${spb:-null}" \
      --argjson excluded_count "${excluded_count:-null}" \
      --argjson excluded_cpu_sum "${excluded_cpu_sum:-null}" \
      --argjson excluded_mem_sum "${excluded_mem_sum:-null}" \
      --slurpfile source "$PROJECT_ROOT/submissions/$benchmark/$submission_dir/metrics.json" \
      '{
        timestamp: $timestamp,
        compiler: $compiler,
        version: $version,
        user: $user,
        variant: $variant,
        submission_dir: $submission_dir,
        min_protocol_version: $min_protocol_version,
        positive_evaluations: [
          $source[0].evaluations[]?
          | select(.included_in_aggregates != false and .execution_result == "success")
          | {name, cpu_units, memory_units}
        ],
        metrics: {
          cpu_units: $cpu,
          memory_units: $memory,
          script_size_bytes: $source[0].measurements.script_size_bytes,
          term_size: $source[0].measurements.term_size,
          execution_fee_lovelace: $exec_fee,
          reference_script_fee_lovelace: $source[0].measurements.reference_script_fee_lovelace,
          total_fee_lovelace: $total_fee,
          tx_memory_budget_pct: $tx_mem_pct,
          tx_cpu_budget_pct: $tx_cpu_pct,
          block_memory_budget_pct: $block_mem_pct,
          block_cpu_budget_pct: $block_cpu_pct,
          scripts_per_tx: $spt,
          scripts_per_block: $spb,
          excluded_count: $excluded_count,
          excluded_cpu_sum: $excluded_cpu_sum,
          excluded_mem_sum: $excluded_mem_sum
        }
      }')
    jq --argjson item "$next" '. + [$item]' "$tmp_submissions" > "$tmp_submissions.new"
    mv "$tmp_submissions.new" "$tmp_submissions"
  done <<< "$csv_data"

  jq -n \
    --arg benchmark "$benchmark" \
    --arg generated_at "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" \
    --slurpfile submissions "$tmp_submissions" \
    --slurpfile suite "$PROJECT_ROOT/scenarios/$benchmark/cape-tests.json" \
    '[($suite[0].measurements // $suite[0].tests // [])[]
        | select(.pending != true and .included_in_aggregates != false and .expected.type != "error")
        | .name] as $cases
      | {benchmark: $benchmark, generated_at: $generated_at,
         positive_cases: $cases,
         submissions: ($submissions[0] | map(.positive_evaluations |=
           map(select(.name as $name | $cases | index($name)))))}' \
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

# Render per-submission evaluation breakdown pages under
# report/benchmarks/<benchmark>/<submission_dir>.html. Each page lists every
# evaluation from that submission's metrics.json (measurements = included in
# aggregates, checks = excluded). The breakdown is read directly from
# metrics.json rather than the positional aggregate CSV, which can't carry a
# variable-length evaluation list.
generate_submission_detail_pages() {
  local benchmark="$1"
  local output_dir="$2"
  local target_filter="${3:-}"

  local csv_data
  csv_data=$(fetch_benchmark_csv "$benchmark" "$target_filter")
  [ -n "$csv_data" ] || return 0

  local detail_dir="$output_dir/benchmarks/$benchmark"
  local abs_template_path="$SCRIPT_DIR/submission-detail.html.tmpl"

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render submission detail pages under $detail_dir/"
    return 0
  fi

  local ts
  ts=$(date '+%Y-%m-%d %H:%M:%S %Z')
  mkdir -p "$detail_dir"

  while IFS= read -r line; do
    [ -n "$line" ] || continue
    local language version user variant submission_dir min_protocol_version
    language=$(csv_field "$line" "language")
    version=$(csv_field "$line" "version")
    user=$(csv_field "$line" "user")
    variant=$(csv_field "$line" "variant")
    submission_dir=$(csv_field "$line" "submission_dir")
    min_protocol_version=$(csv_field "$line" "min_protocol_version")

    [ -n "$submission_dir" ] || continue
    local metrics_path="$PROJECT_ROOT/submissions/$benchmark/$submission_dir/metrics.json"
    if [ ! -f "$metrics_path" ]; then
      log_warn "metrics.json not found for $benchmark/$submission_dir, skipping detail page"
      continue
    fi

    local label="$language $version"
    if [ -n "$variant" ] && [ "$variant" != "default" ]; then
      label="$label · $variant"
    fi
    label="$label · $user"

    local source_url="https://github.com/IntersectMBO/UPLC-CAPE/blob/main/submissions/$benchmark/$submission_dir"

    local temp_json
    temp_json=$(mktemp_json)
    jq \
      --arg benchmark "$benchmark" \
      --arg submission_dir "$submission_dir" \
      --arg label "$label" \
      --arg compiler "$language" \
      --arg version "$version" \
      --arg variant "$variant" \
      --arg user "$user" \
      --arg source_url "$source_url" \
      --arg min_protocol_version "$min_protocol_version" \
      --arg timestamp "$ts" \
      '
      def commafy:
        (tostring | explode | reverse) as $rev
        | [range(0; ($rev | length)) as $i
           | (($rev[$i:$i+1]) | implode)
             + (if ($i > 0 and ($i % 3 == 0)) then "," else "" end)]
        | reverse | join("");
      # gomplate renders *.html.tmpl with Go text/template (no auto-escaping),
      # so every interpolated string must be HTML-escaped here. Free-form
      # fields like an evaluation description can legitimately contain < > &.
      def esc:
        gsub("&"; "&amp;") | gsub("<"; "&lt;") | gsub(">"; "&gt;")
        | gsub("\""; "&quot;") | gsub("'"'"'"; "&#39;");
      def row: {
        name: (.name | esc),
        description: (.description | esc),
        execution_result: (.execution_result | esc),
        cpu_units, memory_units,
        cpu_fmt: (.cpu_units | commafy),
        mem_fmt: (.memory_units | commafy)
      };
      (.evaluations // []) as $evals
      | ($evals | map(select(.included_in_aggregates))       | map(row)) as $measurements
      | ($evals | map(select(.included_in_aggregates | not)) | map(row)) as $checks
      | {
          benchmark: ($benchmark | esc),
          submission_dir: ($submission_dir | esc),
          label: ($label | esc),
          compiler: ($compiler | esc),
          version: ($version | esc),
          variant: ($variant | esc),
          user: ($user | esc),
          source_url: ($source_url | esc),
          min_protocol_version: ($min_protocol_version | esc),
          timestamp: ($timestamp | esc),
          measurements: $measurements,
          checks: $checks,
          measurements_count: ($measurements | length),
          checks_count: ($checks | length)
        }
      ' "$metrics_path" > "$temp_json"

    if ! gomplate -f "$abs_template_path" -c .="$temp_json" > "$detail_dir/${submission_dir}.html"; then
      echo "ERROR: Submission detail template rendering failed for $benchmark/$submission_dir" >&2
      exit 1
    fi
  done <<< "$csv_data"
}

# The index and charts consume the same generated benchmark JSON files.
# Keep only navigation metadata here; ranking is shared in report-comparison.js.
build_index_stats() {
  local benchmark_list="$1"
  local benchmark data category scenario_type submission_count
  local stats
  stats=$(mktemp_json)
  echo '[]' > "$stats"
  while IFS= read -r benchmark; do
    [ -n "$benchmark" ] || continue
    [ -f "$PROJECT_ROOT/scenarios/$benchmark/$benchmark.md" ] || continue
    data="$report_dir/benchmarks/$benchmark.json"
    category=$(categorize_scenario "$benchmark")
    scenario_type=$(classify_scenario "$benchmark")
    submission_count=0
    if [ -f "$data" ]; then
      submission_count=$(jq '.submissions | length' "$data")
    fi
    jq --arg name "$benchmark" --arg category "$category" --arg type "$scenario_type" --argjson count "$submission_count" \
      '. + [{name: $name, category: $category, type: $type, submission_count: $count}]' \
      "$stats" > "$stats.new"
    mv "$stats.new" "$stats"
  done <<< "$benchmark_list"
  jq '{benchmarks: .}' "$stats"
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

# Type is independent of the fixed/open algorithm category.
classify_scenario() {
  local scenario="$1"
  local scenario_file="$PROJECT_ROOT/scenarios/$scenario/${scenario}.md"
  local scenario_type
  scenario_type=$(awk 'BEGIN{in_fm=0} /^---$/ {in_fm++; next} in_fm==1 && /^type:/ {print $2; exit}' "$scenario_file")
  if [[ "$scenario_type" != "synthetic" && "$scenario_type" != "real-world" ]]; then
    log_err "Scenario '$scenario' must have 'type: synthetic' or 'type: real-world' in $scenario_file"
    return 1
  fi
  printf '%s\n' "$scenario_type"
}

generate_index_report() {
  local output_dir="$1"
  local benchmark_list="$2"

  # Single-benchmark updates with --keep-existing retain the other rendered
  # benchmarks in the index. A fresh report lists only pages it generated.
  if [[ $KEEP_EXISTING -eq 1 && -d "$output_dir/benchmarks" ]]; then
    local saved_benchmarks
    saved_benchmarks=$(find "$output_dir/benchmarks" -maxdepth 1 -type f -name '*.json' -printf '%f\n' | sed 's/\.json$//')
    benchmark_list=$(printf '%s\n%s\n' "$benchmark_list" "$saved_benchmarks" | sort -u)
  fi

  local temp_json temp_stats
  temp_json=$(mktemp_json)
  temp_stats=$(mktemp_json)

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would generate benchmark stats"
    echo '{"benchmarks":[]}' > "$temp_stats"
  else
    build_index_stats "$benchmark_list" > "$temp_stats"
  fi

  cat > "$temp_json" << EOF
{
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')",
  "benchmarks": [
EOF

  local first=true
  # `benchmark` is localized so this loop doesn't clobber the caller's
  # `benchmark` variable (the single-benchmark entry point reads it after
  # this returns, for its success messages).
  local benchmark
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
    log_info "[dry-run] Would render $output_dir/index.html using template $SCRIPT_DIR/index.html.tmpl"
  else
    if ! gomplate -f "$SCRIPT_DIR/index.html.tmpl" -c .="$temp_json" > "$output_dir/index.html"; then
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
    # A retained data file must not rank submissions that no longer exist.
    rm -f "$output_dir/benchmarks/${benchmark}.json"
    if ! gomplate -f "$abs_template_path" -c .="$temp_json" > "$abs_output_path"; then
      echo "ERROR: Template rendering failed for no-submissions report" >&2
      exit 1
    fi
  fi
}

# Build evolution stats per (compiler, author) for the evolution report.
# Submissions are partitioned by directory-name variant:
#   - variant `default`  → mainnet timeline (version chain with delta-vs-
#     previous / delta-vs-first)
#   - variant `preview`  → preview teaser column (single
#     delta-vs-latest-mainnet computed against the latest mainnet column,
#     NOT the previous overall column)
# All other variants are sibling experiments and live in the per-scenario
# production report, not on the evolution timeline. Track classification is
# author-declared via the directory suffix, not derived from the runtime
# `min_plutus_version` threshold — that field still drives `cape submission
# verify --all` skipping but no longer steers the report.
build_evolution_stats() {
  local all_csv
  all_csv=$($CAPE_CMD submission aggregate 2> /dev/null | tail -n +2)

  local ts
  ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

  if [ -z "$all_csv" ]; then
    jq -n --arg ts "$ts" '{generated_at: $ts, compilers: []}'
    return
  fi

  printf '%s\n' "$all_csv" | jq -Rs --arg ts "$ts" '
    # Format a number to exactly one decimal place as a string. jq normalizes
    # whole-number floats to integers (e.g. 38.0 → 38), which then survive
    # JSON round-tripping as a different Go type than 28.6 and break gomplate
    # template comparisons. Forcing the textual representation here keeps
    # downstream rendering type-uniform.
    def fmt1:
      ((. * 10 | round) / 10) as $q
      | ($q | tostring) as $s
      | if ($s | test("\\.")) then $s else $s + ".0" end;

    # Build a {text, class} delta object or null when undefined. The template
    # tests truthiness via `{{with}}` and never compares numbers directly.
    def delta_obj(curr; base):
      if (base == null) or (base == 0) or (curr == null) then null
      else
        (((curr - base) * 100.0 / base)) as $d
        | ($d | fmt1) as $signed
        | {
            text:  (if $d > 0 then "+" + $signed + "%" else $signed + "%" end),
            class: (if $d > 0 then "delta-positive"
                    elif $d < 0 then "delta-negative"
                    else "delta-zero" end)
          }
      end;

    def vkey: split(".") | map(tonumber? // 0);

    # Lift one row into the metric record we store per (compiler, author, version, scenario).
    def metrics_of(r):
      {
        version:            r.version,
        cpu_units:          r.cpu_units,
        memory_units:       r.memory_units,
        script_size_bytes:  r.script_size_bytes,
        term_size:          r.term_size,
        total_fee_lovelace: r.total_fee_lovelace
      };

    split("\n")
    | map(select(length > 0) | split(",") | {
        benchmark:          .[0],
        compiler:           .[2],
        version:            .[3],
        author:             .[4],
        variant:            .[5],
        cpu_units:          (.[6]  | tonumber? // null),
        memory_units:       (.[7]  | tonumber? // null),
        script_size_bytes:  (.[8]  | tonumber? // null),
        term_size:          (.[9]  | tonumber? // null),
        total_fee_lovelace: (.[12] | tonumber? // null)
      })
    | map(select(.variant == "default" or .variant == "preview"))
    | group_by({compiler, author})
    | map(
        . as $rows
        | ($rows | map(select(.variant == "default") | .version) | unique | sort_by(vkey)) as $mainnet_versions
        | ($rows | map(select(.variant == "preview") | .version) | unique | sort_by(vkey) | last) as $preview_version
        | {
            compiler: .[0].compiler,
            author:   .[0].author,
            mainnet_versions: $mainnet_versions,
            preview_version:  $preview_version,
            scenarios: (
              $rows
              | group_by(.benchmark)
              | map(
                  . as $srows
                  | {
                      name: .[0].benchmark,
                      mainnet_values: (
                        $mainnet_versions | map(
                          . as $v
                          | ($srows | map(select(.version == $v and .variant == "default")) | .[0]) as $r
                          | (if $r == null then
                               { version: $v, cpu_units: null, memory_units: null, script_size_bytes: null, term_size: null, total_fee_lovelace: null }
                             else metrics_of($r) end)
                        )
                      ),
                      preview_value: (
                        if $preview_version == null then null
                        else
                          ($srows | map(select(.version == $preview_version and .variant == "preview")) | .[0]) as $r
                          | (if $r == null then null else metrics_of($r) end)
                        end
                      )
                    }
                  # Decorate mainnet_values with delta_prev / delta_first.
                  | .mainnet_values |= (
                      . as $vals
                      | [ range(0; length) as $i
                          | $vals[$i] as $curr
                          | ($vals[0:$i] | map(.cpu_units)          | map(select(. != null))) as $cpu_p
                          | ($vals[0:$i] | map(.memory_units)       | map(select(. != null))) as $mem_p
                          | ($vals[0:$i] | map(.script_size_bytes)  | map(select(. != null))) as $size_p
                          | ($vals[0:$i] | map(.term_size)          | map(select(. != null))) as $term_p
                          | ($vals[0:$i] | map(.total_fee_lovelace) | map(select(. != null))) as $fee_p
                          | $curr + {
                              delta_prev: {
                                cpu_units:          delta_obj($curr.cpu_units;          $cpu_p  | last),
                                memory_units:       delta_obj($curr.memory_units;       $mem_p  | last),
                                script_size_bytes:  delta_obj($curr.script_size_bytes;  $size_p | last),
                                term_size:          delta_obj($curr.term_size;          $term_p | last),
                                total_fee_lovelace: delta_obj($curr.total_fee_lovelace; $fee_p  | last)
                              },
                              delta_first: {
                                cpu_units:          delta_obj($curr.cpu_units;          $cpu_p  | first),
                                memory_units:       delta_obj($curr.memory_units;       $mem_p  | first),
                                script_size_bytes:  delta_obj($curr.script_size_bytes;  $size_p | first),
                                term_size:          delta_obj($curr.term_size;          $term_p | first),
                                total_fee_lovelace: delta_obj($curr.total_fee_lovelace; $fee_p  | first)
                              }
                            }
                        ]
                    )
                  # Decorate preview_value with delta_vs_latest_mainnet
                  # (baseline = latest non-null mainnet value for this scenario,
                  # NOT the previous overall column). Pre-compute the bases at
                  # the scenario level because inside `.preview_value |= (…)`
                  # the bound `.` refers to preview_value, not the scenario.
                  | (.mainnet_values | map(.cpu_units)          | map(select(. != null)) | last) as $cpu_base
                  | (.mainnet_values | map(.memory_units)       | map(select(. != null)) | last) as $mem_base
                  | (.mainnet_values | map(.script_size_bytes)  | map(select(. != null)) | last) as $size_base
                  | (.mainnet_values | map(.term_size)          | map(select(. != null)) | last) as $term_base
                  | (.mainnet_values | map(.total_fee_lovelace) | map(select(. != null)) | last) as $fee_base
                  | .preview_value |= (
                      if . == null then null
                      else
                        . + {
                          delta_vs_latest_mainnet: {
                            cpu_units:          delta_obj(.cpu_units;          $cpu_base),
                            memory_units:       delta_obj(.memory_units;       $mem_base),
                            script_size_bytes:  delta_obj(.script_size_bytes;  $size_base),
                            term_size:          delta_obj(.term_size;          $term_base),
                            total_fee_lovelace: delta_obj(.total_fee_lovelace; $fee_base)
                          }
                        }
                      end
                    )
                )
              # A scenario contributes to the timeline only when it has at
              # least two non-null mainnet points. Preview-only scenarios
              # carry no improvement signal and are dropped.
              | map(select([.mainnet_values[] | select(.cpu_units != null)] | length >= 2))
              | sort_by(.name)
            )
          }
      )
    # Drop compilers with no surviving scenarios.
    | map(select(.scenarios | length > 0))
    | sort_by(.compiler, .author)
    | {generated_at: $ts, compilers: .}
  '
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

  # Compute aggregate CSV once up front; each benchmark iteration filters it.
  aggregate_csv_cache "current"

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

      generate_submission_detail_pages "$benchmark" "$report_dir" "current"

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
    generate_index_report "$report_dir" "$(echo -e "$benchmarks_with_submissions")"
    echo ""
    echo "✅ Current HTML reports generated:"
    echo "   📄 $report_dir/index.html (main index)"
    echo "   📊 Individual benchmark reports in $report_dir/benchmarks/"
    echo "   📦 Benchmark data JSON in $report_dir/benchmarks/<scenario>.json"
    echo "   🔍 Per-submission breakdowns in $report_dir/benchmarks/<scenario>/<submission_dir>.html"
  else
    echo "Error: No submissions found for report generation" >&2
    exit 1
  fi

  echo ""
  echo "🌱 Generating Evolution Reports (per-compiler version timeline)..."
  evolution_dir="$report_dir/evolution"
  if [[ $DRY_RUN -eq 0 ]]; then
    rm -rf "$evolution_dir"
    mkdir -p "$evolution_dir/compilers"
    cp -f "$PROJECT_ROOT/uplc-cape-logo.png" "$evolution_dir/uplc-cape-logo.png"
  fi

  if [[ $DRY_RUN -eq 1 ]]; then
    echo "  [dry-run] Would generate per-compiler evolution report"
  else
    temp_compiler_stats=$(mktemp_json)
    build_evolution_stats > "$temp_compiler_stats"

    compiler_count=$(jq '.compilers | length' "$temp_compiler_stats")

    if [ "$compiler_count" -gt 0 ]; then
      temp_index_json=$(mktemp_json)
      jq --arg ts "$(date '+%Y-%m-%d %H:%M:%S %Z')" \
        '{timestamp: $ts, compilers: .compilers}' "$temp_compiler_stats" > "$temp_index_json"
      gomplate -f "$SCRIPT_DIR/index-evolution.html.tmpl" -c .="$temp_index_json" > "$evolution_dir/index.html"

      for i in $(seq 0 $((compiler_count - 1))); do
        compiler_name=$(jq -r ".compilers[$i].compiler" "$temp_compiler_stats")
        author=$(jq -r ".compilers[$i].author" "$temp_compiler_stats")

        temp_compiler_json=$(mktemp_json)
        jq --arg ts "$(date '+%Y-%m-%d %H:%M:%S %Z')" \
          ".compilers[$i] + {timestamp: \$ts}" "$temp_compiler_stats" > "$temp_compiler_json"
        gomplate -f "$SCRIPT_DIR/compiler-evolution.html.tmpl" -c .="$temp_compiler_json" \
          > "$evolution_dir/compilers/${compiler_name}_${author}.html"

        mainnet_count=$(jq -r ".compilers[$i].mainnet_versions | length" "$temp_compiler_stats")
        preview_version=$(jq -r '.compilers['"$i"'].preview_version // "—"' "$temp_compiler_stats")
        echo "  Evolution: $compiler_name by $author (mainnet: $mainnet_count, preview: $preview_version)"
      done

      echo ""
      echo "✅ Evolution HTML reports generated:"
      echo "   📄 $evolution_dir/index.html (compiler-centric index)"
      echo "   📊 Compiler detail pages in $evolution_dir/compilers/"
    else
      echo "  (No multi-version compilers found — evolution report skipped)"
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

  # Compute aggregate CSV once; fetch_benchmark_csv filters it from the cache.
  aggregate_csv_cache ""

  if [ -d "$PROJECT_ROOT/submissions/$benchmark" ] && [ "$(find "$PROJECT_ROOT/submissions/$benchmark" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" 2> /dev/null | wc -l)" -gt 0 ]; then
    if ! generate_benchmark_data_json "$benchmark" "$report_dir"; then
      echo "❌ Error: Failed to build data JSON for benchmark '$benchmark'" >&2
      exit 1
    fi

    generate_individual_benchmark_report "$benchmark" "$report_dir"
    generate_submission_detail_pages "$benchmark" "$report_dir"
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
  echo "   🔍 Per-submission breakdowns: $report_dir/benchmarks/${benchmark}/<submission_dir>.html"
  echo ""
  echo "Open the main report with: xdg-open $report_dir/index.html 2>/dev/null || echo \"Open: $report_dir/index.html\""
fi
