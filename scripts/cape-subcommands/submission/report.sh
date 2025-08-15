#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# Cape Submission Report - Generate HTML reports with chart images comparing submissions
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

# Help rendering via gomplate now via shared lib
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Use shared logging (respect NO_COLOR already handled in lib)
log_info() { cape_info "$1"; }
log_warn() { cape_warn "$1"; }
log_err() { cape_error "$1"; }

# Use shared temp helpers
cape_enable_tmp_cleanup

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

# Check required tools
cape_require_cmds gnuplot gomplate

# Verify repo structure
if [ ! -d "$PROJECT_ROOT/submissions" ]; then
  log_err "Must be run within the project (submissions directory not found)"
  echo "Detected PROJECT_ROOT=$PROJECT_ROOT" >&2
  exit 1
fi

# Validate benchmark name pattern when provided (lowercase, hyphens allowed)
valid_benchmark_name() { [[ $1 =~ ^[a-z][a-z0-9-]*[a-z0-9]$|^[a-z]$ ]]; }

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
  log_info "[dry-run] Would create '$report_dir/benchmarks/images'"
else
  mkdir -p "$report_dir/benchmarks/images"
fi

generate_benchmark_report() {
  local benchmark="$1"
  local output_dir="$2"

  # Get CSV data for this benchmark
  # Guard grep to not fail with pipefail when no matches
  local csv_data valid_csv_data
  csv_data=$($CAPE_CMD submission aggregate | grep "^$benchmark," || true)

  if [ -z "$csv_data" ]; then
    echo "No submissions found for benchmark: $benchmark" >&2
    return 1
  fi

  # Filter out invalid CSV entries (those with empty numeric fields or template placeholders)
  valid_csv_data=$(echo "$csv_data" | grep -v '<.*>' | awk -F, '$6 != "" && $7 != "" && $8 != "" && $9 != ""')
  if [ -z "$valid_csv_data" ]; then
    echo "No valid submissions found for benchmark: $benchmark (all entries have missing data)" >&2
    return 1
  fi

  # Use valid_csv_data for chart generation
  csv_data="$valid_csv_data"

  echo "Generating report for benchmark: $benchmark" >&2

  # Create directories
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would ensure directory exists: $output_dir/benchmarks/images"
  else
    mkdir -p "$output_dir/benchmarks/images"
  fi

  # Create temporary files for gnuplot data
  local temp_dir
  temp_dir="$(cape_mktemp_dir)"
  local data_file="$temp_dir/data.csv"
  local plot_file="$temp_dir/plot.gp"

  # Prepare data
  echo "$csv_data" > "$data_file"

  # Create submission to color index mapping for consistency across charts
  local color_map_file="$temp_dir/global_color_map.txt"
  if [[ $DRY_RUN -eq 1 ]]; then
    # In dry-run, create an empty temp map so lookups don't error
    : > "$color_map_file"
  else
    if [ ! -f "$HOME/.cache/uplc-cape-color-map.txt" ]; then
      mkdir -p "$HOME/.cache"
      : > "$HOME/.cache/uplc-cape-color-map.txt"
    fi
    cp "$HOME/.cache/uplc-cape-color-map.txt" "$color_map_file"
  fi

  # Assign colors
  local all_labels
  all_labels=$(echo "$csv_data" | awk -F, '{print $10}')
  while read -r label; do
    [ -n "$label" ] || continue
    local color_index
    color_index=$(grep "^${label}:" "$color_map_file" | cut -d: -f2 || true)
    if [ -z "$color_index" ]; then
      local hash
      hash=$(printf '%s' "$label" | md5sum | cut -c1-8)
      color_index=$((0x$hash % 256))
      if [[ $DRY_RUN -eq 1 ]]; then
        log_info "[dry-run] Would add color mapping: ${label}:${color_index}"
      else
        echo "${label}:${color_index}" >> "$color_map_file"
      fi
    fi
  done <<< "$all_labels"

  # Update the global color map cache
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would update global color map cache"
  else
    cp "$color_map_file" "$HOME/.cache/uplc-cape-color-map.txt"
  fi

  # Generate PNG charts - always use benchmark prefix for organized file structure
  local chart_prefix="${benchmark}_"

  # CPU Units chart
  local cpu_sorted_data cpu_labels cpu_values max_cpu max_cpu_padded
  cpu_sorted_data=$(echo "$csv_data" | sort -t, -k6,6n)
  cpu_labels=$(echo "$cpu_sorted_data" | awk -F, '{print $10}')
  cpu_values=$(echo "$cpu_sorted_data" | awk -F, '{print $6}')
  max_cpu=$(echo "$cpu_values" | tail -1)
  max_cpu_padded=$(awk -v m="$max_cpu" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}cpu_units.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set output "$output_dir/benchmarks/images/${chart_prefix}cpu_units.png"
set title 'CPU Units Comparison - $benchmark (Lower is Better)'
set xlabel 'Submissions'
set ylabel 'CPU Units'
set style data boxes
set style fill solid 0.8
set boxwidth 0.6
set xtics rotate by -45
set grid y
set key off
set auto x
set yrange [0:$max_cpu_padded]
plot '-' using 2:3:4:xtic(1) with boxes lc variable title 'CPU Units'
EOF
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_idx
      value=$(echo "$cpu_values" | sed -n "${i}p")
      color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      echo "$label $i $value $color_idx" >> "$plot_file"
      ((i++))
    done <<< "$cpu_labels"
    echo "e" >> "$plot_file"
    gnuplot "$plot_file"
  fi

  # Memory Units chart
  local memory_sorted_data memory_labels memory_values max_memory max_memory_padded
  memory_sorted_data=$(echo "$csv_data" | sort -t, -k7,7n)
  memory_labels=$(echo "$memory_sorted_data" | awk -F, '{print $10}')
  memory_values=$(echo "$memory_sorted_data" | awk -F, '{print $7}')
  max_memory=$(echo "$memory_values" | tail -1)
  max_memory_padded=$(awk -v m="$max_memory" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}memory_units.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set output "$output_dir/benchmarks/images/${chart_prefix}memory_units.png"
set title 'Memory Units Comparison - $benchmark (Lower is Better)'
set xlabel 'Submissions'
set ylabel 'Memory Units'
set style data boxes
set style fill solid 0.8
set boxwidth 0.6
set xtics rotate by -45
set grid y
set key off
set auto x
set yrange [0:$max_memory_padded]
plot '-' using 2:3:4:xtic(1) with boxes lc variable title 'Memory Units'
EOF
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_idx
      value=$(echo "$memory_values" | sed -n "${i}p")
      color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      echo "$label $i $value $color_idx" >> "$plot_file"
      ((i++))
    done <<< "$memory_labels"
    echo "e" >> "$plot_file"
    gnuplot "$plot_file"
  fi

  # Script Size chart
  local script_sorted_data script_labels script_values max_script_size max_script_size_padded
  script_sorted_data=$(echo "$csv_data" | sort -t, -k8,8n)
  script_labels=$(echo "$script_sorted_data" | awk -F, '{print $10}')
  script_values=$(echo "$script_sorted_data" | awk -F, '{print $8}')
  max_script_size=$(echo "$script_values" | tail -1)
  max_script_size_padded=$(awk -v m="$max_script_size" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}script_size.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set output "$output_dir/benchmarks/images/${chart_prefix}script_size.png"
set title 'Script Size Comparison - $benchmark (Lower is Better)'
set xlabel 'Submissions'
set ylabel 'Size (bytes)'
set style data boxes
set style fill solid 0.8
set boxwidth 0.6
set xtics rotate by -45
set grid y
set key off
set auto x
set yrange [0:$max_script_size_padded]
plot '-' using 2:3:4:xtic(1) with boxes lc variable title 'Script Size'
EOF
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_idx
      value=$(echo "$script_values" | sed -n "${i}p")
      color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      echo "$label $i $value $color_idx" >> "$plot_file"
      ((i++))
    done <<< "$script_labels"
    echo "e" >> "$plot_file"
    gnuplot "$plot_file"
  fi

  # Term Size chart
  local term_sorted_data term_labels term_values max_term_size max_term_size_padded
  term_sorted_data=$(echo "$csv_data" | sort -t, -k9,9n)
  term_labels=$(echo "$term_sorted_data" | awk -F, '{print $10}')
  term_values=$(echo "$term_sorted_data" | awk -F, '{print $9}')
  max_term_size=$(echo "$term_values" | tail -1)
  max_term_size_padded=$(awk -v m="$max_term_size" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}term_size.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set output "$output_dir/benchmarks/images/${chart_prefix}term_size.png"
set title 'Term Size Comparison - $benchmark (Lower is Better)'
set xlabel 'Submissions'
set ylabel 'AST Nodes'
set style data boxes
set style fill solid 0.8
set boxwidth 0.6
set xtics rotate by -45
set grid y
set key off
set auto x
set yrange [0:$max_term_size_padded]
plot '-' using 2:3:4:xtic(1) with boxes lc variable title 'Term Size'
EOF
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_idx
      value=$(echo "$term_values" | sed -n "${i}p")
      color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      echo "$label $i $value $color_idx" >> "$plot_file"
      ((i++))
    done <<< "$term_labels"
    echo "e" >> "$plot_file"
    gnuplot "$plot_file"
  fi

  # Return chart file names for HTML generation
  echo "${chart_prefix}cpu_units.png,${chart_prefix}memory_units.png,${chart_prefix}script_size.png,${chart_prefix}term_size.png"
}

generate_individual_benchmark_report() {
  local benchmark="$1"
  local output_dir="$2"
  local chart_files="$3"

  # Parse chart files manually to avoid array issues
  local chart1 chart2 chart3 chart4
  chart1=$(echo "$chart_files" | cut -d, -f1)
  chart2=$(echo "$chart_files" | cut -d, -f2)
  chart3=$(echo "$chart_files" | cut -d, -f3)
  chart4=$(echo "$chart_files" | cut -d, -f4)

  # Get CSV data for this benchmark to create the data table
  local csv_data valid_csv_data
  csv_data=$($CAPE_CMD submission aggregate | grep "^$benchmark," || true)

  # Filter out invalid CSV entries (those with empty numeric fields or template placeholders)
  valid_csv_data=$(echo "$csv_data" | grep -v '<.*>' | awk -F, '$6 != "" && $7 != "" && $8 != "" && $9 != ""')
  csv_data="$valid_csv_data"

  # Create JSON data for template
  local temp_json
  temp_json="/tmp/cape_benchmark_report_$$_$(date +%s).json"
  cat > "$temp_json" << EOF
{
  "benchmark": "$benchmark",
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')",
  "charts": {
    "cpu_units": "$chart1",
    "memory_units": "$chart2",
    "script_size": "$chart3",
    "term_size": "$chart4"
  },
  "submissions": [
EOF

  # Generate JSON for submissions table - sorted by CPU units (ascending)
  # Filter out invalid CSV entries (those with empty numeric fields or template placeholders)
  local valid_csv_data table_sorted_data first
  valid_csv_data=$(echo "$csv_data" | grep -v '<.*>' | awk -F, '$6 != "" && $7 != "" && $8 != "" && $9 != ""')
  if [ -z "$valid_csv_data" ]; then
    log_err "No valid submissions found for benchmark $benchmark"
    return 1
  fi

  table_sorted_data=$(echo "$valid_csv_data" | sort -t, -k6,6n)
  first=true
  while IFS= read -r line; do
    if [ -n "$line" ]; then
      local timestamp language version user cpu_units memory_units script_size term_size submission_dir
      timestamp=$(echo "$line" | cut -d, -f2)
      language=$(echo "$line" | cut -d, -f3)
      version=$(echo "$line" | cut -d, -f4)
      user=$(echo "$line" | cut -d, -f5)
      cpu_units=$(echo "$line" | cut -d, -f6)
      memory_units=$(echo "$line" | cut -d, -f7)
      script_size=$(echo "$line" | cut -d, -f8)
      term_size=$(echo "$line" | cut -d, -f9)
      submission_dir=$(echo "$line" | cut -d, -f10)

      # Skip entries with empty numeric fields
      if [ -z "$cpu_units" ] || [ -z "$memory_units" ] || [ -z "$script_size" ] || [ -z "$term_size" ]; then
        continue
      fi

      if [ "$first" = "false" ]; then
        echo "," >> "$temp_json"
      fi
      first=false

      cat >> "$temp_json" << EOF
    {
      "timestamp": "$timestamp",
      "language": "$language",
      "version": "$version",
      "user": "$user",
      "cpu_units": $cpu_units,
      "memory_units": $memory_units,
      "script_size": $script_size,
      "term_size": $term_size,
      "submission_dir": "$submission_dir"
    }
EOF
    fi
  done <<< "$table_sorted_data"

  cat >> "$temp_json" << EOF

  ]
}
EOF

  # Template & output paths
  local abs_template_path="$SCRIPT_DIR/benchmark.html.tmpl"
  local abs_output_path="$output_dir/benchmarks/${benchmark}.html"

  # Use the temporary JSON file directly without copying
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $abs_output_path using template $abs_template_path"
  else
    if ! gomplate -f "$abs_template_path" -c .="$temp_json" > "$abs_output_path"; then
      echo "ERROR: Template rendering failed" >&2
      rm -f "$temp_json"
      exit 1
    fi
  fi

  # Clean up temp file
  rm -f "$temp_json"
}

generate_index_report() {
  local output_dir="$1"
  local benchmark_list="$2"

  # Create JSON data for template
  local temp_json
  temp_json="/tmp/cape_index_report_$$_$(date +%s).json"
  cat > "$temp_json" << EOF
{
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')",
  "benchmarks": [
EOF

  # Generate JSON array for benchmarks
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

  ]
}
EOF

  # Render template with gomplate
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/index.html using template $SCRIPT_DIR/index.html.tmpl"
  else
    if ! gomplate -f "$SCRIPT_DIR/index.html.tmpl" -c .="$temp_json" > "$output_dir/index.html"; then
      echo "ERROR: Index template rendering failed" >&2
      rm -f "$temp_json"
      exit 1
    fi
  fi

  # Clean up temp file
  rm -f "$temp_json"
}

# Parse arguments
if [ $# -eq 0 ]; then
  log_err "No arguments provided"
  echo "Use 'cape submission report --help' for usage information" >&2
  exit 1
fi

if [ "$1" = "--all" ]; then
  # Generate reports for all benchmarks
  all_csv=$($CAPE_CMD submission aggregate)
  if [ -z "$all_csv" ] || [ "$all_csv" = "benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir" ]; then
    echo "No submissions found in any benchmark" >&2
    exit 1
  fi

  # Extract unique benchmark names (skip header)
  benchmarks=$(echo "$all_csv" | tail -n +2 | cut -d, -f1 | sort -u)

  echo "🚀 Generating HTML Performance Reports - All Benchmarks"
  echo "========================================================="

  # Generate charts and individual reports for each benchmark
  generated_benchmarks=""
  for benchmark in $benchmarks; do
    # Validate benchmark names as we iterate
    if ! valid_benchmark_name "$benchmark"; then
      log_warn "Skipping invalid benchmark name: $benchmark"
      continue
    fi
    echo "Processing benchmark: $benchmark"
    chart_files=$(generate_benchmark_report "$benchmark" "$report_dir") || true
    if [ -n "$chart_files" ]; then
      generate_individual_benchmark_report "$benchmark" "$report_dir" "$chart_files"
      if [ -n "$generated_benchmarks" ]; then
        generated_benchmarks="${generated_benchmarks}\n"
      fi
      generated_benchmarks="${generated_benchmarks}${benchmark}"
    fi
  done

  # Generate index page with links to all benchmarks
  if [ -n "$generated_benchmarks" ]; then
    generate_index_report "$report_dir" "$(echo -e "$generated_benchmarks")"
    echo ""
    echo "✅ HTML reports generated:"
    echo "   📄 $report_dir/index.html (main index)"
    echo "   📊 Individual benchmark reports in $report_dir/benchmarks/"
    echo "   🖼️  Chart images in $report_dir/benchmarks/images/"
    echo ""
    echo "Open the main report with: xdg-open $report_dir/index.html 2>/dev/null || echo \"Open: $report_dir/index.html\""
  else
    echo "Error: No valid benchmarks found for report generation" >&2
    exit 1
  fi
else
  # Generate report for specific benchmark
  benchmark="$1"
  if ! valid_benchmark_name "$benchmark"; then
    log_err "Invalid benchmark name: '$benchmark'"
    echo "Expected pattern: lowercase with optional hyphens (e.g., two-party-escrow)" >&2
    exit 1
  fi

  # Validate benchmark exists
  if [ ! -d "$PROJECT_ROOT/submissions/$benchmark" ]; then
    echo "Error: Benchmark '$benchmark' not found" >&2
    echo "Available benchmarks:" >&2
    find "$PROJECT_ROOT/submissions" -mindepth 1 -maxdepth 1 -type d \
      -printf '%f\n' | grep -v '^TEMPLATE$' | sort >&2 || true
    exit 1
  fi

  echo "🚀 Generating HTML Performance Report - $benchmark"
  echo "================================================="

  # Generate charts for the benchmark
  chart_files=$(generate_benchmark_report "$benchmark" "$report_dir") || true
  if [ -n "$chart_files" ]; then
    # Generate individual benchmark report page
    generate_individual_benchmark_report "$benchmark" "$report_dir" "$chart_files"

    # Generate index page with just this benchmark
    generate_index_report "$report_dir" "$benchmark"
    echo ""
    echo "✅ HTML reports generated:"
    echo "   📄 $report_dir/index.html (main index)"
    echo "   📊 Individual benchmark report: $report_dir/benchmarks/${benchmark}.html"
    echo "   🖼️  Chart images in $report_dir/benchmarks/images/"
    echo ""
    echo "Open the main report with: xdg-open $report_dir/index.html 2>/dev/null || echo \"Open: $report_dir/index.html\""
  else
    echo "Error: Failed to generate report for benchmark '$benchmark'" >&2
    exit 1
  fi
fi
