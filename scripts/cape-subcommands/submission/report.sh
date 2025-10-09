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

# CSV format: benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir
# Field positions:
#   1: benchmark
#   2: timestamp
#   3: language
#   4: version
#   5: user
#   6: cpu_units
#   7: memory_units
#   8: script_size_bytes
#   9: term_size
#  10: submission_dir

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
  local csv_data valid_csv_data
  csv_data=$($CAPE_CMD submission aggregate | grep "^$benchmark," || true)

  if [ -z "$csv_data" ]; then
    echo "No submissions found for benchmark: $benchmark" >&2
    return 1
  fi

  # Filter out invalid CSV entries (those with empty numeric fields or template placeholders)
  # CSV format: benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir
  # Field positions: cpu_units=6, memory_units=7, script_size_bytes=8, term_size=9
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

  # Define a set of 100 distinct colors for maximum support
  local distinct_colors=(
    # Primary bright colors
    "#e31a1c" "#1f78b4" "#33a02c" "#ff7f00" "#6a3d9a"
    "#b15928" "#f781bf" "#999999" "#ffff33" "#a65628"
    # Secondary vibrant colors
    "#377eb8" "#4daf4a" "#984ea3" "#ff7f00" "#a6cee3"
    "#b2df8a" "#fdbf6f" "#cab2d6" "#fb9a99" "#ffff99"
    # Tertiary distinct colors
    "#8dd3c7" "#bebada" "#fb8072" "#80b1d3" "#fdb462"
    "#b3de69" "#fccde5" "#d9d9d9" "#bc80bd" "#ccebc5"
    # Quaternary colors - darker variants
    "#1b9e77" "#d95f02" "#7570b3" "#e7298a" "#66a61e"
    "#e6ab02" "#a6761d" "#666666" "#8e0152" "#c51b7d"
    # More bright colors
    "#de2d26" "#238b45" "#081d58" "#2c7fb8" "#7fcdbb"
    "#41b6c4" "#2c7fb8" "#253494" "#fed976" "#feb24c"
    # Extended palette - reds
    "#bd0026" "#e31a1c" "#fc4e2a" "#fd8d3c" "#feb24c"
    "#fed976" "#ffffb2" "#800026" "#bd0026" "#e31a1c"
    # Extended palette - blues
    "#08306b" "#08519c" "#2171b5" "#4292c6" "#6baed6"
    "#9ecae1" "#c6dbef" "#deebf7" "#08589e" "#2b8cbe"
    # Extended palette - greens
    "#00441b" "#006d2c" "#238b45" "#41ab5d" "#74c476"
    "#a1d99b" "#c7e9c0" "#e5f5e0" "#005a32" "#238b45"
    # Extended palette - purples
    "#3f007d" "#54278f" "#6a51a3" "#807dba" "#9e9ac8"
    "#bcbddc" "#dadaeb" "#efedf5" "#4a1486" "#6a51a3"
    # Extended palette - oranges
    "#7f2704" "#a63603" "#d94801" "#f16913" "#fd8d3c"
    "#fdae6b" "#fdd0a2" "#feedde" "#8c2d04" "#cc4c02"
    # Extended palette - teals and cyans
    "#00363d" "#006064" "#00838f" "#00acc1" "#26c6da"
    "#4dd0e1" "#80deea" "#b2ebf2" "#006064" "#00838f"
  )

  # Assign colors using distinct palette
  # Field 10 is submission_dir (used as label)
  local all_labels
  all_labels=$(echo "$csv_data" | awk -F, '{print $10}')
  while read -r label; do
    [ -n "$label" ] || continue
    local color_spec
    color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2 || true)
    if [ -z "$color_spec" ]; then
      # Count existing assignments to get next index
      local next_index
      next_index=$(wc -l < "$color_map_file")
      local palette_index=$((next_index % ${#distinct_colors[@]}))
      color_spec="${distinct_colors[$palette_index]}"
      if [[ $DRY_RUN -eq 1 ]]; then
        log_info "[dry-run] Would add color mapping: ${label}:${color_spec}"
      else
        echo "${label}:${color_spec}" >> "$color_map_file"
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
  # Field 6 is cpu_units, field 10 is submission_dir (label)
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
EOF
    # Generate plot command with individual series for each submission
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_spec
      value=$(echo "$cpu_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false
      plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" notitle"
      ((i++))
    done <<< "$cpu_labels"
    echo "$plot_cmd" >> "$plot_file"

    # Generate data sections for each submission
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value
      value=$(echo "$cpu_values" | sed -n "${i}p")
      echo "$i $value $label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$cpu_labels"
    gnuplot "$plot_file"
  fi

  # Memory Units chart
  # Field 7 is memory_units, field 10 is submission_dir (label)
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
EOF
    # Generate plot command with individual series for each submission
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_spec
      value=$(echo "$memory_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false
      plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" notitle"
      ((i++))
    done <<< "$memory_labels"
    echo "$plot_cmd" >> "$plot_file"

    # Generate data sections for each submission
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value
      value=$(echo "$memory_values" | sed -n "${i}p")
      echo "$i $value $label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$memory_labels"
    gnuplot "$plot_file"
  fi

  # Script Size chart
  # Field 8 is script_size_bytes, field 10 is submission_dir (label)
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
EOF
    # Generate plot command with individual series for each submission
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_spec
      value=$(echo "$script_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false
      plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" notitle"
      ((i++))
    done <<< "$script_labels"
    echo "$plot_cmd" >> "$plot_file"

    # Generate data sections for each submission
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value
      value=$(echo "$script_values" | sed -n "${i}p")
      echo "$i $value $label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$script_labels"
    gnuplot "$plot_file"
  fi

  # Term Size chart
  # Field 9 is term_size, field 10 is submission_dir (label)
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
EOF
    # Generate plot command with individual series for each submission
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_spec
      value=$(echo "$term_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false
      plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" notitle"
      ((i++))
    done <<< "$term_labels"
    echo "$plot_cmd" >> "$plot_file"

    # Generate data sections for each submission
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value
      value=$(echo "$term_values" | sed -n "${i}p")
      echo "$i $value $label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$term_labels"
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
  # CSV format: benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir
  # Field positions: cpu_units=6, memory_units=7, script_size_bytes=8, term_size=9
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
  # CSV data is already filtered and valid from above
  # CSV format: benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir
  local table_sorted_data first
  table_sorted_data=$(echo "$csv_data" | sort -t, -k6,6n)
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

# Build filtered benchmark stats from CSV data
build_filtered_stats() {
  # Get CSV data
  local csv_data
  csv_data=$($CAPE_CMD submission aggregate)

  # Start JSON output
  echo '{'
  echo '  "generated_at": "'$(date -u +"%Y-%m-%dT%H:%M:%SZ")'",'
  echo '  "benchmarks": ['

  # Get unique benchmarks from CSV (skip header)
  local benchmarks
  benchmarks=$(echo "$csv_data" | tail -n +2 | cut -d, -f1 | sort -u)

  local first_benchmark=true
  for benchmark in $benchmarks; do
    # Get submissions for this benchmark from CSV
    local benchmark_csv
    benchmark_csv=$(echo "$csv_data" | grep "^$benchmark,")

    local submission_count
    submission_count=$(echo "$benchmark_csv" | wc -l)

    if [ "$first_benchmark" = "false" ]; then
      echo ','
    fi
    first_benchmark=false

    echo '    {'
    echo '      "name": "'$benchmark'",'
    echo '      "submission_count": '$submission_count','
    echo '      "submissions": ['

    # Process each submission for this benchmark
    local first_submission=true
    while IFS= read -r line; do
      if [ -z "$line" ]; then continue; fi

      # Parse CSV fields (benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir)
      local timestamp language version user cpu_units memory_units script_size term_size
      timestamp=$(echo "$line" | cut -d, -f2)
      language=$(echo "$line" | cut -d, -f3)
      version=$(echo "$line" | cut -d, -f4)
      user=$(echo "$line" | cut -d, -f5)
      cpu_units=$(echo "$line" | cut -d, -f6)
      memory_units=$(echo "$line" | cut -d, -f7)
      script_size=$(echo "$line" | cut -d, -f8)
      term_size=$(echo "$line" | cut -d, -f9)

      # Extract date from timestamp
      local date_only
      date_only=$(echo "$timestamp" | cut -d'T' -f1)

      # Format numbers for display
      local cpu_formatted memory_formatted
      cpu_formatted=$(format_number_short "$cpu_units")
      memory_formatted=$(format_number_short "$memory_units")

      if [ "$first_submission" = "false" ]; then
        echo ','
      fi
      first_submission=false

      cat << EOF
        {
          "compiler": "$language",
          "version": "$version",
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
          }
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

# Helper function to format numbers (used by build_filtered_stats)
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

generate_index_report() {
  local output_dir="$1"
  local benchmark_list="$2"

  # Create JSON data for template
  local temp_json temp_stats
  temp_json="/tmp/cape_index_report_$$_$(date +%s).json"
  temp_stats="/tmp/cape_stats_$$_$(date +%s).json"

  # Get benchmark stats
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would generate benchmark stats"
    echo '{"benchmarks":[]}' > "$temp_stats"
  else
    # Build stats from CSV data
    build_filtered_stats > "$temp_stats"
  fi

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

  ],
  "stats": $(cat "$temp_stats")
}
EOF

  # Clean up temp stats file
  rm -f "$temp_stats"

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

generate_no_submissions_report() {
  local benchmark="$1"
  local output_dir="$2"

  # Create JSON data for template with empty submissions
  local temp_json
  temp_json="/tmp/cape_no_submissions_$$_$(date +%s).json"
  cat > "$temp_json" << EOF
{
  "benchmark": "$benchmark",
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')",
  "charts": {
    "cpu_units": "",
    "memory_units": "",
    "script_size": "",
    "term_size": ""
  },
  "submissions": [],
  "has_submissions": false
}
EOF

  # Template & output paths
  local abs_template_path="$SCRIPT_DIR/benchmark-no-submissions.html.tmpl"
  local abs_output_path="$output_dir/benchmarks/${benchmark}.html"

  # Check if we have a specific template for no submissions, otherwise use the regular template
  if [ ! -f "$abs_template_path" ]; then
    abs_template_path="$SCRIPT_DIR/benchmark.html.tmpl"
  fi

  # Render template
  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $abs_output_path using template $abs_template_path"
  else
    if ! gomplate -f "$abs_template_path" -c .="$temp_json" > "$abs_output_path"; then
      echo "ERROR: Template rendering failed for no-submissions report" >&2
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
  echo "🚀 Generating HTML Performance Reports - All Benchmarks"
  echo "========================================================="

  # Get all benchmarks from scenarios directory (not just those with submissions)
  all_benchmarks=$(find "$PROJECT_ROOT/scenarios" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" -exec basename {} \; 2> /dev/null | sort)

  if [ -z "$all_benchmarks" ]; then
    echo "No benchmark scenarios found" >&2
    exit 1
  fi

  # Collect benchmarks that have submissions
  benchmarks_with_submissions=""

  # Generate individual benchmark reports
  for benchmark in $all_benchmarks; do
    # Check if benchmark has submissions
    if [ -d "$PROJECT_ROOT/submissions/$benchmark" ] && [ "$(find "$PROJECT_ROOT/submissions/$benchmark" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" 2> /dev/null | wc -l)" -gt 0 ]; then
      echo "Processing $benchmark..."

      # Generate charts and benchmark report
      chart_files=$(generate_benchmark_report "$benchmark" "$report_dir") || true
      if [ -n "$chart_files" ]; then
        if [[ $DRY_RUN -eq 0 ]]; then
          generate_individual_benchmark_report "$benchmark" "$report_dir" "$chart_files"
        fi

        # Add to benchmarks list
        if [ -n "$benchmarks_with_submissions" ]; then
          benchmarks_with_submissions="${benchmarks_with_submissions}\n"
        fi
        benchmarks_with_submissions="${benchmarks_with_submissions}${benchmark}"
      fi
    else
      # Generate placeholder page for benchmark without submissions
      echo "No submissions for $benchmark, creating placeholder..."
      generate_no_submissions_report "$benchmark" "$report_dir"
    fi
  done

  # Generate index page
  if [ -n "$benchmarks_with_submissions" ]; then
    generate_index_report "$report_dir" "$(echo -e "$benchmarks_with_submissions")"
    echo ""
    echo "✅ HTML reports generated:"
    echo "   📄 $report_dir/index.html (main index)"
    echo "   📊 Individual benchmark reports in $report_dir/benchmarks/"
    echo "   🖼️  Chart images in $report_dir/benchmarks/images/"
    echo ""
    echo "Open the main report with: xdg-open $report_dir/index.html 2>/dev/null || echo \"Open: $report_dir/index.html\""
  else
    echo "Error: No submissions found for report generation" >&2
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

  # Validate benchmark exists in scenarios (allow benchmarks without submissions yet)
  if [ ! -d "$PROJECT_ROOT/scenarios/$benchmark" ]; then
    echo "Error: Benchmark scenario '$benchmark' not found" >&2
    echo "Available benchmarks:" >&2
    find "$PROJECT_ROOT/scenarios" -mindepth 1 -maxdepth 1 -type d \
      -printf '%f\n' | grep -v '^TEMPLATE$' | sort >&2 || true
    exit 1
  fi

  echo "🚀 Generating HTML Performance Report - $benchmark"
  echo "================================================="

  # Check if benchmark has submissions
  if [ -d "$PROJECT_ROOT/submissions/$benchmark" ] && [ "$(find "$PROJECT_ROOT/submissions/$benchmark" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" 2> /dev/null | wc -l)" -gt 0 ]; then
    # Generate charts for the benchmark with submissions
    chart_files=$(generate_benchmark_report "$benchmark" "$report_dir") || true
    if [ -n "$chart_files" ]; then
      # Generate individual benchmark report page with charts
      generate_individual_benchmark_report "$benchmark" "$report_dir" "$chart_files"
    else
      echo "Error: Failed to generate report for benchmark '$benchmark'" >&2
      exit 1
    fi
  else
    # Generate placeholder page for benchmark without submissions
    echo "No submissions found for benchmark: $benchmark. Creating placeholder page."
    generate_no_submissions_report "$benchmark" "$report_dir"
  fi

  # Generate index page with just this benchmark
  generate_index_report "$report_dir" "$benchmark"
  echo ""
  echo "✅ HTML reports generated:"
  echo "   📄 $report_dir/index.html (main index)"
  echo "   📊 Individual benchmark report: $report_dir/benchmarks/${benchmark}.html"
  echo "   🖼️  Chart images in $report_dir/benchmarks/images/"
  echo ""
  echo "Open the main report with: xdg-open $report_dir/index.html 2>/dev/null || echo \"Open: $report_dir/index.html\""
fi
