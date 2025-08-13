#!/usr/bin/env bash
set -euo pipefail

# Cape Submission Report - Generate HTML reports with chart images comparing submissions
# Usage: cape submission report <benchmark>
#        cape submission report --all

show_help() {
  cat << EOF
Usage: cape submission report <benchmark>
       cape submission report --all

Generate HTML performance reports with chart images comparing submission performance.

Arguments:
  <benchmark>    Name of the benchmark to generate report for
  --all          Generate reports for all benchmarks with submissions

The report includes:
- Main index page (report/index.html) listing all benchmarks
- Individual benchmark report pages (report/benchmarks/<benchmark>.html)
- 4 PNG chart images per benchmark comparing submissions by:
  - CPU Units: Execution cost in CPU units
  - Memory Units: Memory consumption
  - Script Size: Serialized UPLC script size in bytes
  - Term Size: Number of AST nodes in UPLC term

Each submission is labeled as: {language}_{version}_{user}

Output:
  report/index.html                           # Main index with all benchmarks
  report/benchmarks/<benchmark>.html          # Individual benchmark reports
  report/benchmarks/images/<benchmark>_*.png  # Chart image files

Examples:
  cape submission report fibonacci    # Report for fibonacci benchmark
  cape submission report --all        # Reports for all benchmarks

Requirements:
  - gnuplot must be installed
  - Submissions must have metrics.json files (use 'cape submission measure')
EOF
}

# Check for help flag
if [[ "${1:-}" =~ ^(-h|--help|help)$ ]]; then
  show_help
  exit 0
fi

# Check if gnuplot is available
if ! command -v gnuplot > /dev/null 2>&1; then
  echo "Error: gnuplot is not installed or not in PATH" >&2
  echo "Please install gnuplot to use this command" >&2
  exit 1
fi

# Check if gomplate is available
if ! command -v gomplate > /dev/null 2>&1; then
  echo "Error: gomplate is not installed or not in PATH" >&2
  echo "Please install gomplate to use this command" >&2
  exit 1
fi

# Find project root (should already be set by cape.sh)
if [ ! -d "submissions" ]; then
  echo "Error: Must be run from project root (submissions directory not found)" >&2
  exit 1
fi

generate_benchmark_report() {
  local benchmark="$1"
  local output_dir="$2"

  # Get CSV data for this benchmark
  csv_data=$(cape submission aggregate | grep "^$benchmark,")

  if [ -z "$csv_data" ]; then
    echo "No submissions found for benchmark: $benchmark" >&2
    return 1
  fi

  echo "Generating report for benchmark: $benchmark" >&2

  # Create directories
  mkdir -p "$output_dir/benchmarks/images"

  # Create temporary files for gnuplot data
  local temp_dir=$(mktemp -d)
  local data_file="$temp_dir/data.csv"
  local plot_file="$temp_dir/plot.gp"

  # Prepare data
  echo "$csv_data" > "$data_file"

  # Create submission to color index mapping for consistency across charts
  # Use a global color map file to ensure same submissions get same colors across all benchmarks
  local color_map_file="$temp_dir/global_color_map.txt"
  if [ ! -f "$HOME/.cache/uplc-cape-color-map.txt" ]; then
    mkdir -p "$HOME/.cache"
    touch "$HOME/.cache/uplc-cape-color-map.txt"
  fi
  cp "$HOME/.cache/uplc-cape-color-map.txt" "$color_map_file"

  # Assign colors to all submissions from this benchmark and update the global map
  local all_labels=$(echo "$csv_data" | awk -F, '{print $3 "_" $4 "_" $5}' | tr ' ' '_')
  while read -r label; do
    local color_index=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
    if [ -z "$color_index" ]; then
      # Assign new color index based on hash of submission name for deterministic coloring
      local hash=$(echo -n "$label" | md5sum | cut -c1-8)
      color_index=$((0x$hash % 256)) # Use 256 different color indices
      echo "${label}:${color_index}" >> "$color_map_file"
    fi
  done <<< "$all_labels"

  # Update the global color map cache
  cp "$color_map_file" "$HOME/.cache/uplc-cape-color-map.txt"

  # Generate PNG charts - always use benchmark prefix for organized file structure
  local chart_prefix="${benchmark}_"

  # CPU Units chart - sort data by CPU units (ascending - lower is better)
  local cpu_sorted_data=$(echo "$csv_data" | sort -t, -k6,6n)
  local cpu_labels=$(echo "$cpu_sorted_data" | awk -F, '{print $3 "_" $4 "_" $5}' | tr ' ' '_')
  local cpu_values=$(echo "$cpu_sorted_data" | awk -F, '{print $6}')
  local max_cpu=$(echo "$cpu_values" | tail -1)
  local max_cpu_padded=$(echo "$max_cpu * 1.03" | bc -l | cut -d. -f1)

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

  # Create data format: label position value color_index
  local i=1
  while read -r label; do
    local value=$(echo "$cpu_values" | sed -n "${i}p")
    # Get color index for this submission
    local color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
    echo "$label $i $value $color_idx" >> "$plot_file"
    ((i++))
  done <<< "$cpu_labels"
  echo "e" >> "$plot_file"
  gnuplot "$plot_file"

  # Memory Units chart - sort data by Memory units (ascending - lower is better)
  local memory_sorted_data=$(echo "$csv_data" | sort -t, -k7,7n)
  local memory_labels=$(echo "$memory_sorted_data" | awk -F, '{print $3 "_" $4 "_" $5}' | tr ' ' '_')
  local memory_values=$(echo "$memory_sorted_data" | awk -F, '{print $7}')
  local max_memory=$(echo "$memory_values" | tail -1)
  local max_memory_padded=$(echo "$max_memory * 1.03" | bc -l | cut -d. -f1)

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
    local value=$(echo "$memory_values" | sed -n "${i}p")
    local color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
    echo "$label $i $value $color_idx" >> "$plot_file"
    ((i++))
  done <<< "$memory_labels"
  echo "e" >> "$plot_file"
  gnuplot "$plot_file"

  # Script Size chart
  local script_sorted_data=$(echo "$csv_data" | sort -t, -k8,8n)
  local script_labels=$(echo "$script_sorted_data" | awk -F, '{print $3 "_" $4 "_" $5}' | tr ' ' '_')
  local script_values=$(echo "$script_sorted_data" | awk -F, '{print $8}')
  local max_script_size=$(echo "$script_values" | tail -1)
  local max_script_size_padded=$(echo "$max_script_size * 1.03" | bc -l | cut -d. -f1)

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
    local value=$(echo "$script_values" | sed -n "${i}p")
    local color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
    echo "$label $i $value $color_idx" >> "$plot_file"
    ((i++))
  done <<< "$script_labels"
  echo "e" >> "$plot_file"
  gnuplot "$plot_file"

  # Term Size chart
  local term_sorted_data=$(echo "$csv_data" | sort -t, -k9,9n)
  local term_labels=$(echo "$term_sorted_data" | awk -F, '{print $3 "_" $4 "_" $5}' | tr ' ' '_')
  local term_values=$(echo "$term_sorted_data" | awk -F, '{print $9}')
  local max_term_size=$(echo "$term_values" | tail -1)
  local max_term_size_padded=$(echo "$max_term_size * 1.03" | bc -l | cut -d. -f1)

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
    local value=$(echo "$term_values" | sed -n "${i}p")
    local color_idx=$(grep "^${label}:" "$color_map_file" | cut -d: -f2)
    echo "$label $i $value $color_idx" >> "$plot_file"
    ((i++))
  done <<< "$term_labels"
  echo "e" >> "$plot_file"
  gnuplot "$plot_file"

  # Clean up
  rm -rf "$temp_dir"

  # Return chart file names for HTML generation
  echo "${chart_prefix}cpu_units.png,${chart_prefix}memory_units.png,${chart_prefix}script_size.png,${chart_prefix}term_size.png"
}

generate_individual_benchmark_report() {
  local benchmark="$1"
  local output_dir="$2"
  local chart_files="$3"

  # Parse chart files manually to avoid array issues
  local chart1=$(echo "$chart_files" | cut -d, -f1)
  local chart2=$(echo "$chart_files" | cut -d, -f2)
  local chart3=$(echo "$chart_files" | cut -d, -f3)
  local chart4=$(echo "$chart_files" | cut -d, -f4)

  # Get CSV data for this benchmark to create the data table
  local csv_data=$(cape submission aggregate | grep "^$benchmark,")

  # Create JSON data for template
  local temp_json=$(mktemp)
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
  local table_sorted_data=$(echo "$csv_data" | sort -t, -k6,6n)
  local first=true
  while IFS= read -r line; do
    if [ -n "$line" ]; then
      # Parse CSV line manually
      local timestamp=$(echo "$line" | cut -d, -f2)
      local language=$(echo "$line" | cut -d, -f3)
      local version=$(echo "$line" | cut -d, -f4)
      local user=$(echo "$line" | cut -d, -f5)
      local cpu_units=$(echo "$line" | cut -d, -f6)
      local memory_units=$(echo "$line" | cut -d, -f7)
      local script_size=$(echo "$line" | cut -d, -f8)
      local term_size=$(echo "$line" | cut -d, -f9)

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
      "term_size": $term_size
    }
EOF
    fi
  done <<< "$table_sorted_data"

  cat >> "$temp_json" << EOF

  ]
}
EOF

  # Get the directory where this script is located
  local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

  # Render template with gomplate
  local abs_template_path="$(realpath "$script_dir/benchmark.html.tmpl")"
  local abs_output_path="$(realpath "$output_dir")/benchmarks/${benchmark}.html"

  # Copy to stable location for template rendering
  cp "$temp_json" "/tmp/temp_benchmark_data.json"

  if ! gomplate -f "$abs_template_path" -d data="/tmp/temp_benchmark_data.json" > "$abs_output_path"; then
    echo "ERROR: Template rendering failed" >&2
    exit 1
  fi

  # Clean up temp file
  rm -f "/tmp/temp_benchmark_data.json"

  # Clean up temporary file
  rm -f "$temp_json"
}

generate_index_report() {
  local output_dir="$1"
  local benchmark_list="$2"

  # Create JSON data for template
  local temp_json=$(mktemp)
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

  # Get the directory where this script is located
  local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

  # Copy to a stable location and render template
  cp "$temp_json" "/tmp/temp_index_data.json"

  # Render template with gomplate
  if ! gomplate -f "$script_dir/index.html.tmpl" -d data="/tmp/temp_index_data.json" > "$output_dir/index.html"; then
    echo "ERROR: Index template rendering failed" >&2
    exit 1
  fi

  # Clean up temp file
  rm -f "/tmp/temp_index_data.json"

  # Clean up temporary file
  rm -f "$temp_json"
}

# Parse arguments
if [ $# -eq 0 ]; then
  echo "Error: No arguments provided" >&2
  echo "Use 'cape submission report --help' for usage information" >&2
  exit 1
fi

# Create report output directory and clean previous reports
report_dir="report"
rm -rf "$report_dir"
mkdir -p "$report_dir/benchmarks/images"

if [ "$1" = "--all" ]; then
  # Generate reports for all benchmarks
  all_csv=$(cape submission aggregate)
  if [ -z "$all_csv" ] || [ "$all_csv" = "benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size" ]; then
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
    echo "Processing benchmark: $benchmark"
    chart_files=$(generate_benchmark_report "$benchmark" "$report_dir")
    if [ $? -eq 0 ] && [ -n "$chart_files" ]; then
      # Generate individual benchmark report page
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
    echo "Open the main report with: open $report_dir/index.html"
  else
    echo "Error: No valid benchmarks found for report generation" >&2
    exit 1
  fi
else
  # Generate report for specific benchmark
  benchmark="$1"

  # Validate benchmark exists
  if [ ! -d "submissions/$benchmark" ]; then
    echo "Error: Benchmark '$benchmark' not found" >&2
    echo "Available benchmarks:" >&2
    ls submissions/ | grep -v TEMPLATE >&2
    exit 1
  fi

  echo "🚀 Generating HTML Performance Report - $benchmark"
  echo "================================================="

  # Generate charts for the benchmark
  chart_files=$(generate_benchmark_report "$benchmark" "$report_dir")
  if [ $? -eq 0 ] && [ -n "$chart_files" ]; then
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
    echo "Open the main report with: open $report_dir/index.html"
  else
    echo "Error: Failed to generate report for benchmark '$benchmark'" >&2
    exit 1
  fi
fi
