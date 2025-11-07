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
# IMPORTANT: log_info must go to stderr to avoid contaminating function return values
log_info() { cape_info "$1" >&2; }
log_warn() { cape_warn "$1"; }
log_err() { cape_error "$1"; }

# Escape special characters for gnuplot enhanced mode
# In enhanced mode, @ escapes the next character to treat it literally
gnuplot_escape() {
  echo "$1" | sed 's/_/@_/g'
}

# Format submission directory name for display on charts
# Input: Compiler_Version_Contributor (or Compiler_Version_Variant_Contributor)
# Output: Compiler_Version\n\nContributor (raw, no escaping - xtics use noenhanced mode)
format_submission_label() {
  echo "$1" | awk -F_ '{if (NF >= 4) NF = NF - 1; printf "%s_%s\\n\\n%s", $1, $2, $NF}'
}

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

# Validate benchmark name pattern when provided (lowercase, underscores only)
valid_benchmark_name() { [[ $1 =~ ^[a-z][a-z0-9_]*[a-z0-9]$|^[a-z]$ ]]; }

# CSV format: benchmark,timestamp,language,version,user,variant,cpu_units,memory_units,script_size_bytes,term_size,execution_fee_lovelace,reference_script_fee_lovelace,total_fee_lovelace,tx_memory_budget_pct,tx_cpu_budget_pct,block_memory_budget_pct,block_cpu_budget_pct,scripts_per_tx,scripts_per_block,submission_dir
# Field positions:
#   1: benchmark
#   2: timestamp
#   3: language
#   4: version
#   5: user
#   6: variant
#   7: cpu_units
#   8: memory_units
#   9: script_size_bytes
#  10: term_size
#  11: execution_fee_lovelace
#  12: reference_script_fee_lovelace
#  13: total_fee_lovelace
#  14: tx_memory_budget_pct
#  15: tx_cpu_budget_pct
#  16: block_memory_budget_pct
#  17: block_cpu_budget_pct
#  18: scripts_per_tx
#  19: scripts_per_block
#  20: submission_dir

# CSV column mapping - use these to reference columns by name
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
# Usage: csv_field "$line" "column_name"
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
  log_info "[dry-run] Would create '$report_dir/benchmarks/images'"
  log_info "[dry-run] Would copy logo to '$report_dir/uplc-cape-logo.png'"
else
  mkdir -p "$report_dir/benchmarks/images"
  # Copy logo to report directory
  cp -f "$PROJECT_ROOT/uplc-cape-logo.png" "$report_dir/uplc-cape-logo.png"
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
  valid_csv_data=$(echo "$csv_data" | grep -v '<.*>' | awk -F, -v cpu="${CSV_COL[cpu_units]}" -v mem="${CSV_COL[memory_units]}" -v size="${CSV_COL[script_size_bytes]}" -v term="${CSV_COL[term_size]}" '$cpu != "" && $mem != "" && $size != "" && $term != ""')
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
  local all_labels
  all_labels=$(echo "$csv_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
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
  local cpu_sorted_data cpu_labels cpu_values cpu_variants max_cpu max_cpu_padded
  cpu_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[cpu_units]},${CSV_COL[cpu_units]}n)
  cpu_labels=$(echo "$cpu_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  cpu_values=$(echo "$cpu_sorted_data" | awk -F, -v col="${CSV_COL[cpu_units]}" '{print $col}')
  cpu_variants=$(echo "$cpu_sorted_data" | awk -F, -v col="${CSV_COL[variant]}" '{print $col}')
  max_cpu=$(echo "$cpu_values" | tail -1)
  max_cpu_padded=$(awk -v m="$max_cpu" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}cpu_units.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}cpu_units.png"
set ylabel 'CPU Units'
set style data boxes
set style fill solid 1.0 border -1
set boxwidth 0.6
set xtics rotate by -45 noenhanced
set grid y
set key off
set auto x
set logscale y
set yrange [*:*]
EOF
    # Add variant labels vertically on bars (skip "default")
    # For log scale: place all labels at graph center (geometric mean of min/max)
    local min_cpu
    min_cpu=$(echo "$cpu_values" | head -1)
    local label_y_center
    label_y_center=$(awk -v min="$min_cpu" -v max="$max_cpu" 'BEGIN{printf "%.0f", sqrt(min*max)}')
    local i=1
    while read -r variant; do
      [ -n "$variant" ] || continue
      # Only add label if variant is not "default"
      if [ "$variant" != "default" ]; then
        echo "set label \"$variant\" at $i,$label_y_center center rotate by 90 font 'Arial,12' textcolor rgb 'gray30' front" >> "$plot_file"
      fi
      ((i++))
    done <<< "$cpu_variants"

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
      # Format label as two lines: "Compiler_Version\nContributor"
      local display_label
      display_label=$(format_submission_label "$label")
      echo "$i $value $display_label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$cpu_labels"
    gnuplot "$plot_file"
  fi

  # Memory Units chart
  local memory_sorted_data memory_labels memory_values memory_variants max_memory max_memory_padded
  memory_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[memory_units]},${CSV_COL[memory_units]}n)
  memory_labels=$(echo "$memory_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  memory_values=$(echo "$memory_sorted_data" | awk -F, -v col="${CSV_COL[memory_units]}" '{print $col}')
  memory_variants=$(echo "$memory_sorted_data" | awk -F, -v col="${CSV_COL[variant]}" '{print $col}')
  max_memory=$(echo "$memory_values" | tail -1)
  max_memory_padded=$(awk -v m="$max_memory" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}memory_units.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}memory_units.png"
set ylabel 'Memory Units'
set style data boxes
set style fill solid 1.0 border -1
set boxwidth 0.6
set xtics rotate by -45 noenhanced
set grid y
set key off
set auto x
set logscale y
set yrange [*:*]
EOF
    # Add variant labels vertically on bars (skip "default")
    # For log scale: place all labels at graph center (geometric mean of min/max)
    local min_memory
    min_memory=$(echo "$memory_values" | head -1)
    local label_y_center
    label_y_center=$(awk -v min="$min_memory" -v max="$max_memory" 'BEGIN{printf "%.0f", sqrt(min*max)}')
    local i=1
    while read -r variant; do
      [ -n "$variant" ] || continue
      # Only add label if variant is not "default"
      if [ "$variant" != "default" ]; then
        echo "set label \"$variant\" at $i,$label_y_center center rotate by 90 font 'Arial,12' textcolor rgb 'gray30' front" >> "$plot_file"
      fi
      ((i++))
    done <<< "$memory_variants"

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
      # Format label as two lines: "Compiler_Version\nContributor"
      local display_label
      display_label=$(format_submission_label "$label")
      echo "$i $value $display_label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$memory_labels"
    gnuplot "$plot_file"
  fi

  # Script Size chart
  local script_sorted_data script_labels script_values script_variants max_script_size max_script_size_padded
  script_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[script_size_bytes]},${CSV_COL[script_size_bytes]}n)
  script_labels=$(echo "$script_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  script_values=$(echo "$script_sorted_data" | awk -F, -v col="${CSV_COL[script_size_bytes]}" '{print $col}')
  script_variants=$(echo "$script_sorted_data" | awk -F, -v col="${CSV_COL[variant]}" '{print $col}')
  max_script_size=$(echo "$script_values" | tail -1)
  max_script_size_padded=$(awk -v m="$max_script_size" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}script_size.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}script_size.png"
set ylabel 'Size (bytes)'
set style data boxes
set style fill solid 1.0 border -1
set boxwidth 0.6
set xtics rotate by -45 noenhanced
set grid y
set key off
set auto x
set logscale y
set yrange [*:*]
EOF
    # Add variant labels vertically on bars (skip "default")
    # For log scale: place all labels at graph center (geometric mean of min/max)
    local min_script_size
    min_script_size=$(echo "$script_values" | head -1)
    local label_y_center
    label_y_center=$(awk -v min="$min_script_size" -v max="$max_script_size" 'BEGIN{printf "%.0f", sqrt(min*max)}')
    local i=1
    while read -r variant; do
      [ -n "$variant" ] || continue
      # Only add label if variant is not "default"
      if [ "$variant" != "default" ]; then
        echo "set label \"$variant\" at $i,$label_y_center center rotate by 90 font 'Arial,12' textcolor rgb 'gray30' front" >> "$plot_file"
      fi
      ((i++))
    done <<< "$script_variants"

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
      # Format label as two lines: "Compiler_Version\nContributor"
      local display_label
      display_label=$(format_submission_label "$label")
      echo "$i $value $display_label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$script_labels"
    gnuplot "$plot_file"
  fi

  # Term Size chart
  local term_sorted_data term_labels term_values term_variants max_term_size max_term_size_padded
  term_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[term_size]},${CSV_COL[term_size]}n)
  term_labels=$(echo "$term_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  term_values=$(echo "$term_sorted_data" | awk -F, -v col="${CSV_COL[term_size]}" '{print $col}')
  term_variants=$(echo "$term_sorted_data" | awk -F, -v col="${CSV_COL[variant]}" '{print $col}')
  max_term_size=$(echo "$term_values" | tail -1)
  max_term_size_padded=$(awk -v m="$max_term_size" 'BEGIN{printf "%.0f", (m==""?0:m)*1.03}')

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would render $output_dir/benchmarks/images/${chart_prefix}term_size.png"
  else
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}term_size.png"
set ylabel 'AST Nodes'
set style data boxes
set style fill solid 1.0 border -1
set boxwidth 0.6
set xtics rotate by -45 noenhanced
set grid y
set key off
set auto x
set logscale y
set yrange [*:*]
EOF
    # Add variant labels vertically on bars (skip "default")
    # For log scale: place all labels at graph center (geometric mean of min/max)
    local min_term_size
    min_term_size=$(echo "$term_values" | head -1)
    local label_y_center
    label_y_center=$(awk -v min="$min_term_size" -v max="$max_term_size" 'BEGIN{printf "%.0f", sqrt(min*max)}')
    local i=1
    while read -r variant; do
      [ -n "$variant" ] || continue
      # Only add label if variant is not "default"
      if [ "$variant" != "default" ]; then
        echo "set label \"$variant\" at $i,$label_y_center center rotate by 90 font 'Arial,12' textcolor rgb 'gray30' front" >> "$plot_file"
      fi
      ((i++))
    done <<< "$term_variants"

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
      # Format label as two lines: "Compiler_Version\nContributor"
      local display_label
      display_label=$(format_submission_label "$label")
      echo "$i $value $display_label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$term_labels"
    gnuplot "$plot_file"
  fi

  # Total Fee (ADA) chart
  local fee_sorted_data fee_labels fee_values fee_variants max_fee max_fee_padded
  fee_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[total_fee_lovelace]},${CSV_COL[total_fee_lovelace]}n)
  fee_labels=$(echo "$fee_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  fee_values=$(echo "$fee_sorted_data" | awk -F, -v col="${CSV_COL[total_fee_lovelace]}" '{printf "%.6f\n", $col/1000000}')
  fee_variants=$(echo "$fee_sorted_data" | awk -F, -v col="${CSV_COL[variant]}" '{print $col}')
  max_fee=$(echo "$fee_values" | tail -1)
  max_fee_padded=$(awk -v m="$max_fee" 'BEGIN{printf "%.6f", (m==""?0:m)*1.03}')

  # Determine if log scale is needed based on value range
  local min_fee max_fee_calc fee_ratio use_log_scale fee_scale_label
  min_fee=$(echo "$fee_values" | head -1)
  max_fee_calc=$(echo "$fee_values" | tail -1)

  # Calculate ratio, handling edge cases
  if [ -n "$min_fee" ] && [ -n "$max_fee_calc" ] && [ "$(echo "$min_fee > 0" | bc -l)" -eq 1 ]; then
    fee_ratio=$(echo "scale=2; $max_fee_calc / $min_fee" | bc -l)
    # Use log scale if ratio > 100 (more than 2 orders of magnitude)
    if [ "$(echo "$fee_ratio > 100" | bc -l)" -eq 1 ]; then
      use_log_scale=true
      fee_scale_label="Log Scale"
    else
      use_log_scale=false
      fee_scale_label="Linear Scale"
    fi
  else
    use_log_scale=false
    fee_scale_label="Linear Scale"
  fi

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would generate total fee chart: ${output_dir}/benchmarks/images/${chart_prefix}total_fee.png (${fee_scale_label})"
  else
    plot_file="$temp_dir/plot_fee.gp"
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}total_fee.png"
set ylabel "Total Fee (Ada)"
set style fill solid 1.0 border -1
set boxwidth 0.8
set xtics rotate by -45 noenhanced
set grid y
set key off
set auto x
EOF

    if [ "$use_log_scale" = true ]; then
      cat >> "$plot_file" << EOF
set logscale y
set yrange [*:*]
EOF
    else
      cat >> "$plot_file" << EOF
set yrange [0:$max_fee_padded]
EOF
    fi
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local value color_spec
      value=$(echo "$fee_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false
      plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" notitle"
      ((i++))
    done <<< "$fee_labels"
    echo "$plot_cmd" >> "$plot_file"

    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local value
      value=$(echo "$fee_values" | sed -n "${i}p")
      local display_label
      display_label=$(format_submission_label "$label")
      echo "$i $value $display_label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$fee_labels"
    gnuplot "$plot_file"
  fi

  # Transaction Memory Budget % chart
  local mem_budget_sorted_data mem_budget_labels mem_budget_values mem_budget_cpu_values
  mem_budget_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[tx_memory_budget_pct]},${CSV_COL[tx_memory_budget_pct]}n)
  mem_budget_labels=$(echo "$mem_budget_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  mem_budget_values=$(echo "$mem_budget_sorted_data" | awk -F, -v col="${CSV_COL[tx_memory_budget_pct]}" '{print $col}')
  mem_budget_cpu_values=$(echo "$mem_budget_sorted_data" | awk -F, -v col="${CSV_COL[tx_cpu_budget_pct]}" '{print $col}')

  # Calculate Y-axis range from non-exceeding memory submissions only (≤100%)
  local mem_viable_max
  mem_viable_max=$(echo "$mem_budget_values" | awk '{if($1<=100) print $1}' | sort -n | tail -1)

  # If no viable submissions (all exceed 100%), fall back to normal range
  local mem_max_padded
  if [ -z "$mem_viable_max" ]; then
    local mem_max
    mem_max=$(echo "$mem_budget_values" | sort -n | tail -1)
    mem_max_padded=$(awk -v m="$mem_max" 'BEGIN{printf "%.2f", (m==""?0:m)*1.03}')
  else
    # Use 110% of max viable submission for better visualization
    mem_max_padded=$(awk -v m="$mem_viable_max" 'BEGIN{printf "%.2f", m*1.1}')
  fi

  # Check if any submission exceeds 100%
  local mem_has_exceeding
  mem_has_exceeding=$(paste <(echo "$mem_budget_values") <(echo "$mem_budget_cpu_values") \
    | awk '{if($1>100 || $2>100) print "1"}' | head -1)

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would generate memory budget chart: ${output_dir}/benchmarks/images/${chart_prefix}tx_budget_mem.png"
  else
    plot_file="$temp_dir/plot_tx_budget_mem.gp"
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}tx_budget_mem.png"
set ylabel "Budget Usage (%)"
set style fill solid 1.0 border -1
set boxwidth 0.8
set xtics rotate by -45 noenhanced
set grid ytics
set yrange [0:$mem_max_padded]
set key top left
# Add reference line at 50%
set arrow from graph 0, first 50 to graph 1, first 50 nohead lc rgb "orange" lw 2 dt 2
# Add reference line at 100% (transaction limit)
set arrow from graph 0, first 100 to graph 1, first 100 nohead lc rgb "red" lw 2 dt 1
EOF

    # Build plot command
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local mem_value cpu_value color_spec
      mem_value=$(echo "$mem_budget_values" | sed -n "${i}p")
      cpu_value=$(echo "$mem_budget_cpu_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      local exceeds
      exceeds=$(awk -v m="$mem_value" -v c="$cpu_value" 'BEGIN{print (m>100 || c>100)?"1":"0"}')

      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false

      # Use hatched pattern if this submission exceeds on either metric
      if [ "$exceeds" = "1" ]; then
        plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" fs pattern 4 notitle"
      else
        plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" fs solid notitle"
      fi
      ((i++))
    done <<< "$mem_budget_labels"

    # Add legend entry only if there are exceeding submissions
    if [ -n "$mem_has_exceeding" ]; then
      plot_cmd="$plot_cmd, NaN with boxes lc rgb \"#888888\" fs pattern 4 title 'Exceeds limit (>100%)'"
    fi

    # Add legend entries for reference lines
    plot_cmd="$plot_cmd, NaN with lines lc rgb \"red\" lw 2 dt 1 title '100% tx limit'"
    plot_cmd="$plot_cmd, NaN with lines lc rgb \"orange\" lw 2 dt 2 title '50% threshold'"

    # Add text labels for exceeding memory values
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local mem_value color_spec
      mem_value=$(echo "$mem_budget_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)

      if awk -v m="$mem_value" 'BEGIN{exit(m>100?0:1)}'; then
        local label_text
        label_text=$(awk -v m="$mem_value" 'BEGIN{printf "%.0f%%", m}')
        plot_cmd="$plot_cmd, '-' using 1:2:3 with labels offset 0,1 font \"Arial,10\" textcolor rgb \"$color_spec\" notitle"
      fi

      ((i++))
    done <<< "$mem_budget_labels"

    echo "$plot_cmd" >> "$plot_file"

    # Bar data
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local mem_value
      mem_value=$(echo "$mem_budget_values" | sed -n "${i}p")
      # Cap display value at Y-max for visual consistency
      local display_value
      display_value=$(awk -v m="$mem_value" -v max="$mem_max_padded" 'BEGIN{print (m>max)?max:m}')
      local display_label
      display_label=$(format_submission_label "$label")
      echo "$i $display_value \"$display_label\"" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$mem_budget_labels"

    # Text label data for exceeding memory values
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local mem_value
      mem_value=$(echo "$mem_budget_values" | sed -n "${i}p")

      if awk -v m="$mem_value" 'BEGIN{exit(m>100?0:1)}'; then
        local label_text
        label_text=$(awk -v m="$mem_value" 'BEGIN{printf "%.0f%%", m}')
        echo "$i $mem_max_padded \"$label_text\"" >> "$plot_file"
        echo "e" >> "$plot_file"
      fi

      ((i++))
    done <<< "$mem_budget_labels"

    gnuplot "$plot_file"
  fi

  # Transaction CPU Budget % chart
  local cpu_budget_sorted_data cpu_budget_labels cpu_budget_values cpu_budget_mem_values
  cpu_budget_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[tx_cpu_budget_pct]},${CSV_COL[tx_cpu_budget_pct]}n)
  cpu_budget_labels=$(echo "$cpu_budget_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  cpu_budget_values=$(echo "$cpu_budget_sorted_data" | awk -F, -v col="${CSV_COL[tx_cpu_budget_pct]}" '{print $col}')
  cpu_budget_mem_values=$(echo "$cpu_budget_sorted_data" | awk -F, -v col="${CSV_COL[tx_memory_budget_pct]}" '{print $col}')

  # Calculate Y-axis range from non-exceeding CPU submissions only (≤100%)
  local cpu_viable_max
  cpu_viable_max=$(echo "$cpu_budget_values" | awk '{if($1<=100) print $1}' | sort -n | tail -1)

  # If no viable submissions (all exceed 100%), fall back to normal range
  local cpu_max_padded
  if [ -z "$cpu_viable_max" ]; then
    local cpu_max
    cpu_max=$(echo "$cpu_budget_values" | sort -n | tail -1)
    cpu_max_padded=$(awk -v m="$cpu_max" 'BEGIN{printf "%.2f", (m==""?0:m)*1.03}')
  else
    # Use 110% of max viable submission for better visualization
    cpu_max_padded=$(awk -v m="$cpu_viable_max" 'BEGIN{printf "%.2f", m*1.1}')
  fi

  # Check if any submission exceeds 100%
  local cpu_has_exceeding
  cpu_has_exceeding=$(paste <(echo "$cpu_budget_values") <(echo "$cpu_budget_mem_values") \
    | awk '{if($1>100 || $2>100) print "1"}' | head -1)

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would generate CPU budget chart: ${output_dir}/benchmarks/images/${chart_prefix}tx_budget_cpu.png"
  else
    plot_file="$temp_dir/plot_tx_budget_cpu.gp"
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}tx_budget_cpu.png"
set ylabel "Budget Usage (%)"
set style fill solid 1.0 border -1
set boxwidth 0.8
set xtics rotate by -45 noenhanced
set grid ytics
set yrange [0:$cpu_max_padded]
set key top left
# Add reference line at 50%
set arrow from graph 0, first 50 to graph 1, first 50 nohead lc rgb "orange" lw 2 dt 2
# Add reference line at 100% (transaction limit)
set arrow from graph 0, first 100 to graph 1, first 100 nohead lc rgb "red" lw 2 dt 1
EOF

    # Build plot command
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local cpu_value mem_value color_spec
      cpu_value=$(echo "$cpu_budget_values" | sed -n "${i}p")
      mem_value=$(echo "$cpu_budget_mem_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
      local exceeds
      exceeds=$(awk -v m="$mem_value" -v c="$cpu_value" 'BEGIN{print (m>100 || c>100)?"1":"0"}')

      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false

      # Use hatched pattern if this submission exceeds on either metric
      if [ "$exceeds" = "1" ]; then
        plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" fs pattern 4 notitle"
      else
        plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" fs solid notitle"
      fi
      ((i++))
    done <<< "$cpu_budget_labels"

    # Add legend entry only if there are exceeding submissions
    if [ -n "$cpu_has_exceeding" ]; then
      plot_cmd="$plot_cmd, NaN with boxes lc rgb \"#888888\" fs pattern 4 title 'Exceeds limit (>100%)'"
    fi

    # Add legend entries for reference lines
    plot_cmd="$plot_cmd, NaN with lines lc rgb \"red\" lw 2 dt 1 title '100% tx limit'"
    plot_cmd="$plot_cmd, NaN with lines lc rgb \"orange\" lw 2 dt 2 title '50% threshold'"

    # Add text labels for exceeding CPU values
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local cpu_value color_spec
      cpu_value=$(echo "$cpu_budget_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)

      if awk -v c="$cpu_value" 'BEGIN{exit(c>100?0:1)}'; then
        local label_text
        label_text=$(awk -v c="$cpu_value" 'BEGIN{printf "%.0f%%", c}')
        plot_cmd="$plot_cmd, '-' using 1:2:3 with labels offset 0,1 font \"Arial,10\" textcolor rgb \"$color_spec\" notitle"
      fi

      ((i++))
    done <<< "$cpu_budget_labels"

    echo "$plot_cmd" >> "$plot_file"

    # Bar data
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local cpu_value
      cpu_value=$(echo "$cpu_budget_values" | sed -n "${i}p")
      # Cap display value at Y-max for visual consistency
      local display_value
      display_value=$(awk -v c="$cpu_value" -v max="$cpu_max_padded" 'BEGIN{print (c>max)?max:c}')
      local display_label
      display_label=$(format_submission_label "$label")
      echo "$i $display_value \"$display_label\"" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$cpu_budget_labels"

    # Text label data for exceeding CPU values
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local cpu_value
      cpu_value=$(echo "$cpu_budget_values" | sed -n "${i}p")

      if awk -v c="$cpu_value" 'BEGIN{exit(c>100?0:1)}'; then
        local label_text
        label_text=$(awk -v c="$cpu_value" 'BEGIN{printf "%.0f%%", c}')
        echo "$i $cpu_max_padded \"$label_text\"" >> "$plot_file"
        echo "e" >> "$plot_file"
      fi

      ((i++))
    done <<< "$cpu_budget_labels"

    gnuplot "$plot_file"
  fi

  # Scripts per Transaction (capacity) chart
  # Include ALL submissions, sorted by capacity (descending, higher is better)
  local capacity_sorted_data capacity_labels capacity_values
  capacity_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[scripts_per_tx]},${CSV_COL[scripts_per_tx]}nr)
  capacity_labels=$(echo "$capacity_sorted_data" | awk -F, -v col="${CSV_COL[submission_dir]}" '{print $col}')
  capacity_values=$(echo "$capacity_sorted_data" | awk -F, -v col="${CSV_COL[scripts_per_tx]}" '{print $col}')

  # Calculate Y-axis range from non-zero capacity submissions only (>0)
  local capacity_viable_max capacity_viable_min
  capacity_viable_max=$(echo "$capacity_values" | awk '{if($1>0) print $1}' | sort -n | tail -1)
  capacity_viable_min=$(echo "$capacity_values" | awk '{if($1>0) print $1}' | sort -n | head -1)

  # If no viable submissions (all zero capacity), use 1 as max
  local max_capacity_padded
  if [ -z "$capacity_viable_max" ]; then
    max_capacity_padded=1
  else
    # Use 110% of max viable submission for better visualization
    max_capacity_padded=$(awk -v m="$capacity_viable_max" 'BEGIN{printf "%.0f", m*1.1}')
  fi

  # Check if any submission has zero capacity (exceeds transaction limits)
  local capacity_has_exceeding
  capacity_has_exceeding=$(echo "$capacity_values" | awk '{if($1==0) print "1"}' | head -1)

  # Determine if log scale is needed based on value range
  local capacity_ratio use_capacity_log_scale capacity_scale_label
  if [ -n "$capacity_viable_min" ] && [ -n "$capacity_viable_max" ] && [ "$(echo "$capacity_viable_min > 0" | bc -l)" -eq 1 ]; then
    capacity_ratio=$(echo "scale=2; $capacity_viable_max / $capacity_viable_min" | bc -l)
    # Use log scale if ratio > 100 (more than 2 orders of magnitude)
    if [ "$(echo "$capacity_ratio > 100" | bc -l)" -eq 1 ]; then
      use_capacity_log_scale=true
      capacity_scale_label="Log Scale"
    else
      use_capacity_log_scale=false
      capacity_scale_label="Linear Scale"
    fi
  else
    use_capacity_log_scale=false
    capacity_scale_label="Linear Scale"
  fi

  if [[ $DRY_RUN -eq 1 ]]; then
    log_info "[dry-run] Would generate capacity chart: ${output_dir}/benchmarks/images/${chart_prefix}scripts_per_tx.png"
  else
    plot_file="$temp_dir/plot_capacity.gp"
    cat > "$plot_file" << EOF
set terminal png size 800,600 enhanced font 'Arial,12'
set bmargin 10
set tmargin 3
set output "$output_dir/benchmarks/images/${chart_prefix}scripts_per_tx.png"
set ylabel "Scripts per Transaction"
set style fill solid 1.0 border -1
set boxwidth 0.8
set xtics rotate by -45 noenhanced
set grid y
set auto x
EOF

    if [ "$use_capacity_log_scale" = true ]; then
      cat >> "$plot_file" << EOF
set logscale y
set yrange [*:*]
set key off
EOF
    else
      cat >> "$plot_file" << EOF
set yrange [0:$max_capacity_padded]
EOF
      # Add legend for exceeded limits if applicable
      if [ -n "$capacity_has_exceeding" ]; then
        cat >> "$plot_file" << EOF
set key top left
EOF
      else
        cat >> "$plot_file" << EOF
set key off
EOF
      fi
    fi

    # Build plot command with conditional hatching for zero capacity
    local plot_cmd="plot "
    local i=1
    local first=true
    while read -r label; do
      [ -n "$label" ] || continue
      local capacity_value color_spec
      capacity_value=$(echo "$capacity_values" | sed -n "${i}p")
      color_spec=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)

      if [ "$first" = "false" ]; then
        plot_cmd="$plot_cmd, "
      fi
      first=false

      # Use hatched pattern if capacity is zero (exceeds transaction limits)
      if [ "$capacity_value" = "0" ]; then
        plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" fs pattern 4 notitle"
      else
        plot_cmd="$plot_cmd'-' using 1:2:xtic(3) with boxes lc rgb \"$color_spec\" fs solid notitle"
      fi
      ((i++))
    done <<< "$capacity_labels"

    # Add legend entry if any submission exceeds limits
    if [ -n "$capacity_has_exceeding" ]; then
      plot_cmd="$plot_cmd, NaN with boxes lc rgb \"#888888\" fs pattern 4 title 'Exceeds limits (0)'"
    fi

    # Add text labels for zero-capacity bars (displayed at top of chart)
    local i=1
    local has_zero_labels=false
    while read -r label; do
      [ -n "$label" ] || continue
      local capacity_value
      capacity_value=$(echo "$capacity_values" | sed -n "${i}p")
      if [ "$capacity_value" = "0" ]; then
        has_zero_labels=true
        break
      fi
      ((i++))
    done <<< "$capacity_labels"

    if [ "$has_zero_labels" = true ]; then
      plot_cmd="$plot_cmd, '-' using 1:2:3 with labels offset 0,1 font 'Arial,10' notitle"
    fi

    echo "$plot_cmd" >> "$plot_file"

    # Generate data blocks for each bar
    local i=1
    while read -r label; do
      [ -n "$label" ] || continue
      local capacity_value display_label display_value
      capacity_value=$(echo "$capacity_values" | sed -n "${i}p")
      display_label=$(format_submission_label "$label")

      # Display zero-capacity bars at full height for visibility
      if [ "$capacity_value" = "0" ]; then
        display_value="$max_capacity_padded"
      else
        display_value="$capacity_value"
      fi

      echo "$i $display_value $display_label" >> "$plot_file"
      echo "e" >> "$plot_file"
      ((i++))
    done <<< "$capacity_labels"

    # Add text labels data block for zero values
    if [ "$has_zero_labels" = true ]; then
      local i=1
      while read -r label; do
        [ -n "$label" ] || continue
        local capacity_value
        capacity_value=$(echo "$capacity_values" | sed -n "${i}p")
        if [ "$capacity_value" = "0" ]; then
          echo "$i $max_capacity_padded 0" >> "$plot_file"
        fi
        ((i++))
      done <<< "$capacity_labels"
      echo "e" >> "$plot_file"
    fi

    gnuplot "$plot_file"
  fi

  # Return chart file names for HTML generation
  echo "${chart_prefix}cpu_units.png,${chart_prefix}memory_units.png,${chart_prefix}script_size.png,${chart_prefix}term_size.png,${chart_prefix}total_fee.png,${chart_prefix}tx_budget_mem.png,${chart_prefix}tx_budget_cpu.png,${chart_prefix}scripts_per_tx.png"
}

generate_individual_benchmark_report() {
  local benchmark="$1"
  local output_dir="$2"
  local chart_files="$3"

  # Parse chart files manually to avoid array issues
  local chart1 chart2 chart3 chart4 chart5 chart6 chart7 chart8
  chart1=$(echo "$chart_files" | cut -d, -f1)
  chart2=$(echo "$chart_files" | cut -d, -f2)
  chart3=$(echo "$chart_files" | cut -d, -f3)
  chart4=$(echo "$chart_files" | cut -d, -f4)
  chart5=$(echo "$chart_files" | cut -d, -f5)
  chart6=$(echo "$chart_files" | cut -d, -f6)
  chart7=$(echo "$chart_files" | cut -d, -f7)
  chart8=$(echo "$chart_files" | cut -d, -f8)

  # Get CSV data for this benchmark to create the data table
  local csv_data valid_csv_data
  csv_data=$($CAPE_CMD submission aggregate | grep "^$benchmark," || true)

  # Filter out invalid CSV entries (those with empty numeric fields or template placeholders)
  valid_csv_data=$(echo "$csv_data" | grep -v '<.*>' | awk -F, -v cpu="${CSV_COL[cpu_units]}" -v mem="${CSV_COL[memory_units]}" -v size="${CSV_COL[script_size_bytes]}" -v term="${CSV_COL[term_size]}" '$cpu != "" && $mem != "" && $size != "" && $term != ""')
  csv_data="$valid_csv_data"

  # Check for exceeding conditions to show in HTML subtitles
  local has_cpu_exceeding has_mem_exceeding has_capacity_exceeding
  has_cpu_exceeding=$(echo "$csv_data" | awk -F, -v col="${CSV_COL[tx_cpu_budget_pct]}" '{if($col>100) print "1"}' | head -1)
  has_mem_exceeding=$(echo "$csv_data" | awk -F, -v col="${CSV_COL[tx_memory_budget_pct]}" '{if($col>100) print "1"}' | head -1)
  has_capacity_exceeding=$(echo "$csv_data" | awk -F, -v col="${CSV_COL[scripts_per_tx]}" '{if($col==0) print "1"}' | head -1)

  # Create JSON data for template
  local temp_json
  temp_json="/tmp/cape_benchmark_report_$$_$(date +%s).json"
  cat > "$temp_json" << EOF
{
  "benchmark": "$benchmark",
  "timestamp": "$(date '+%Y-%m-%d %H:%M:%S %Z')",
  "has_cpu_exceeding": $([ -n "$has_cpu_exceeding" ] && echo "true" || echo "false"),
  "has_mem_exceeding": $([ -n "$has_mem_exceeding" ] && echo "true" || echo "false"),
  "has_capacity_exceeding": $([ -n "$has_capacity_exceeding" ] && echo "true" || echo "false"),
  "charts": {
    "cpu_units": "$chart1",
    "memory_units": "$chart2",
    "script_size": "$chart3",
    "term_size": "$chart4",
    "total_fee": "$chart5",
    "tx_budget_mem": "$chart6",
    "tx_budget_cpu": "$chart7",
    "scripts_per_tx": "$chart8"
  },
  "submissions": [
EOF

  # Generate JSON for submissions table - sorted by CPU units (ascending)
  local table_sorted_data first
  table_sorted_data=$(echo "$csv_data" | sort -t, -k${CSV_COL[cpu_units]},${CSV_COL[cpu_units]}n)
  first=true
  while IFS= read -r line; do
    if [ -n "$line" ]; then
      local timestamp language version user variant cpu_units memory_units script_size term_size execution_fee_lovelace reference_script_fee_lovelace total_fee_lovelace tx_memory_budget_pct tx_cpu_budget_pct block_memory_budget_pct block_cpu_budget_pct scripts_per_tx scripts_per_block submission_dir
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
      submission_dir=$(csv_field "$line" "submission_dir")

      # Skip entries with empty numeric fields
      if [ -z "$cpu_units" ] || [ -z "$memory_units" ] || [ -z "$script_size" ] || [ -z "$term_size" ]; then
        continue
      fi

      if [ "$first" = "false" ]; then
        echo "," >> "$temp_json"
      fi
      first=false

      # Calculate ADA from lovelace (1 ADA = 1,000,000 lovelace)
      local total_fee_ada
      total_fee_ada=$(awk -v lovelace="$total_fee_lovelace" 'BEGIN{printf "%.2f", lovelace/1000000}')

      cat >> "$temp_json" << EOF
    {
      "timestamp": "$timestamp",
      "language": "$language",
      "version": "$version",
      "user": "$user",
      "variant": "$variant",
      "cpu_units": $cpu_units,
      "memory_units": $memory_units,
      "script_size": $script_size,
      "term_size": $term_size,
      "execution_fee_lovelace": $execution_fee_lovelace,
      "reference_script_fee_lovelace": $reference_script_fee_lovelace,
      "total_fee_lovelace": $total_fee_lovelace,
      "total_fee_ada": $total_fee_ada,
      "tx_memory_budget_pct": $tx_memory_budget_pct,
      "tx_cpu_budget_pct": $tx_cpu_budget_pct,
      "block_memory_budget_pct": $block_memory_budget_pct,
      "block_cpu_budget_pct": $block_cpu_budget_pct,
      "scripts_per_tx": $scripts_per_tx,
      "scripts_per_block": $scripts_per_block,
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
  benchmarks=$(echo "$csv_data" | tail -n +2 | awk -F, -v col="${CSV_COL[benchmark]}" '{print $col}' | sort -u)

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

    # Get category and winners
    local category winners_json
    category=$(categorize_scenario "$benchmark")
    winners_json=$(find_winners "$benchmark_csv")

    echo '    {'
    echo '      "name": "'$benchmark'",'
    echo '      "category": "'$category'",'
    echo '      "submission_count": '$submission_count','
    echo '      "winners": '$winners_json','
    echo '      "submissions": ['

    # Process each submission for this benchmark
    local first_submission=true
    while IFS= read -r line; do
      if [ -z "$line" ]; then continue; fi

      # Parse CSV fields using column mapping
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

      # Extract date from timestamp
      local date_only
      date_only=$(echo "$timestamp" | cut -d'T' -f1)

      # Format numbers for display
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

# Categorize scenario as fixed algorithm or open optimization
# Reads from YAML frontmatter in scenario specification file
categorize_scenario() {
  local scenario="$1"
  local scenario_file="$PROJECT_ROOT/scenarios/$scenario/${scenario}.md"

  # Check if scenario file exists
  if [ ! -f "$scenario_file" ]; then
    log_err "Scenario file not found: $scenario_file"
    echo "ERROR: Cannot categorize scenario '$scenario' - specification file missing" >&2
    exit 1
  fi

  # Extract category from YAML frontmatter
  # Look for "category: <value>" between the first two "---" lines
  local category
  category=$(awk 'BEGIN{in_fm=0} /^---$/ {in_fm++; next} in_fm==1 && /^category:/ {print $2; exit}' "$scenario_file")

  # Fail if category not specified
  if [ -z "$category" ]; then
    log_err "Missing 'category' attribute in YAML frontmatter for scenario: $scenario"
    echo "ERROR: Scenario '$scenario' must have 'category: fixed' or 'category: open' in $scenario_file" >&2
    echo "Add YAML frontmatter at the top of the file:" >&2
    echo "---" >&2
    echo "category: fixed  # or 'open'" >&2
    echo "---" >&2
    exit 1
  fi

  # Validate category value
  if [ "$category" != "fixed" ] && [ "$category" != "open" ]; then
    log_err "Invalid category '$category' for scenario: $scenario"
    echo "ERROR: Scenario category must be 'fixed' or 'open', got: '$category'" >&2
    exit 1
  fi

  echo "$category"
}

# Find winning submissions for a benchmark
# Returns JSON with winners for each metric (handles ties)
find_winners() {
  local benchmark_csv="$1"

  # Helper function to build JSON array of tied winners for a metric
  build_winners_json() {
    local metric_col="$1"
    local metric_name="$2"

    # Find best (lowest) value
    local best_value
    best_value=$(echo "$benchmark_csv" | awk -F, -v col="$metric_col" '{print $col}' | sort -n | head -1)

    if [ -z "$best_value" ]; then
      echo "null"
      return
    fi

    # Find all submissions with this value
    local tied_submissions
    tied_submissions=$(echo "$benchmark_csv" | awk -F, -v col="$metric_col" -v val="$best_value" '$col == val')

    local total_count
    total_count=$(echo "$tied_submissions" | wc -l)

    # Build JSON array
    echo -n "{\"value\":$best_value,\"formatted\":\"$(format_number_short "$best_value")\",\"total\":$total_count,\"submissions\":["

    local count=0
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

      count=$((count + 1))
    done <<< "$tied_submissions"

    echo -n "]}"
  }

  # Build JSON for each metric
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
    "term_size": "",
    "total_fee": "",
    "tx_budget": "",
    "scripts_per_tx": ""
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
      if ! chart_files=$(generate_benchmark_report "$benchmark" "$report_dir"); then
        echo "❌ Error: Failed to generate report for benchmark '$benchmark'" >&2
        exit 1
      fi

      if [ -z "$chart_files" ]; then
        echo "❌ Error: generate_benchmark_report returned empty chart files for '$benchmark'" >&2
        exit 1
      fi

      if [[ $DRY_RUN -eq 0 ]]; then
        generate_individual_benchmark_report "$benchmark" "$report_dir" "$chart_files"
      fi

      # Add to benchmarks list
      if [ -n "$benchmarks_with_submissions" ]; then
        benchmarks_with_submissions="${benchmarks_with_submissions}\n"
      fi
      benchmarks_with_submissions="${benchmarks_with_submissions}${benchmark}"
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
    echo "Expected pattern: lowercase with optional underscores (e.g., two_party_escrow)" >&2
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
    if ! chart_files=$(generate_benchmark_report "$benchmark" "$report_dir"); then
      echo "❌ Error: Failed to generate report for benchmark '$benchmark'" >&2
      exit 1
    fi

    if [ -z "$chart_files" ]; then
      echo "❌ Error: generate_benchmark_report returned empty chart files for '$benchmark'" >&2
      exit 1
    fi

    # Generate individual benchmark report page with charts
    generate_individual_benchmark_report "$benchmark" "$report_dir" "$chart_files"
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
