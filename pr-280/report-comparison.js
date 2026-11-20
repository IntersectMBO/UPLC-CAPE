/* Shared by the scenario cards and comparison charts. No build step required. */
(function (root, factory) {
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.CapeComparison = factory();
})(typeof globalThis === "object" ? globalThis : this, function () {
  "use strict";

  const aggregations = ["sum", "mean", "median", "minimum", "maximum"];
  const aggregationLabels = {
    sum: "Sum",
    mean: "Mean",
    median: "Median",
    minimum: "Minimum",
    maximum: "Maximum",
  };
  const metrics = [
    { key: "total_fee_lovelace", title: "Fee", aggregate: true, unit: "ADA", direction: "asc" },
    { key: "cpu_units", title: "CPU", aggregate: true, unit: "units", direction: "asc" },
    { key: "memory_units", title: "Memory", aggregate: true, unit: "units", direction: "asc" },
    { key: "script_size_bytes", title: "Flat Size", unit: "bytes", direction: "asc" },
    { key: "term_size", title: "AST Size", unit: "nodes", direction: "asc" },
    { key: "scripts_per_tx", title: "Scripts/Tx", unit: "scripts", direction: "desc" },
    { key: "tx_memory_budget_pct", title: "Mem Tx", unit: "%", direction: "asc" },
    { key: "tx_cpu_budget_pct", title: "CPU Tx", unit: "%", direction: "asc" },
  ];

  function summarize(values) {
    if (!values.length) return null;
    const xs = [...values].sort((a, b) => a - b);
    const sum = xs.reduce((a, b) => a + b, 0);
    const mid = Math.floor(xs.length / 2);
    return {
      sum,
      mean: sum / xs.length,
      median: xs.length % 2 ? xs[mid] : (xs[mid - 1] + xs[mid]) / 2,
      minimum: xs[0],
      maximum: xs[xs.length - 1],
    };
  }

  // Match Cape.Protocol.Parameters using integers, including the fee ceiling.
  // Denominator 10^7: memory costs 577/10000 and CPU costs 721/10000000.
  function feeStatistics(evaluations, referenceFee) {
    if (!Number.isSafeInteger(referenceFee) || referenceFee < 0) return null;
    const xs = evaluations
      .map(e => BigInt(e.memory_units) * 577000n + BigInt(e.cpu_units) * 721n)
      .sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));
    if (!xs.length) return null;
    const ceiling = (n, d = 1n) =>
      Number((n + 10000000n * d - 1n) / (10000000n * d)) + referenceFee;
    const sum = xs.reduce((a, b) => a + b, 0n);
    const mid = Math.floor(xs.length / 2);
    return {
      sum: ceiling(sum),
      mean: ceiling(sum, BigInt(xs.length)),
      median: xs.length % 2 ? ceiling(xs[mid]) : ceiling(xs[mid - 1] + xs[mid], 2n),
      minimum: ceiling(xs[0]),
      maximum: ceiling(xs[xs.length - 1]),
    };
  }

  function prepareSubmission(submission, expectedCases) {
    const evaluations = submission.positive_evaluations || [];
    const names = evaluations.map(e => e.name).sort();
    const expected = [...expectedCases].sort();
    const validBudget = n => Number.isSafeInteger(n) && n >= 0;
    const complete =
      expected.length > 0 &&
      names.length === expected.length &&
      names.every((name, i) => name === expected[i]) &&
      evaluations.every(e => validBudget(e.cpu_units) && validBudget(e.memory_units));
    const comparison = { complete, case_count: evaluations.length };
    if (complete) {
      comparison.cpu_units = summarize(evaluations.map(e => e.cpu_units));
      comparison.memory_units = summarize(evaluations.map(e => e.memory_units));
      comparison.total_fee_lovelace = feeStatistics(
        evaluations,
        submission.metrics.reference_script_fee_lovelace
      );
      const cpuMax = comparison.cpu_units.maximum;
      const memMax = comparison.memory_units.maximum;
      comparison.tx_cpu_budget_pct = (cpuMax / 10000000000) * 100;
      comparison.tx_memory_budget_pct = (memMax / 16500000) * 100;
      const capacity = Math.min(
        cpuMax > 0 ? Math.floor(10000000000 / cpuMax) : Infinity,
        memMax > 0 ? Math.floor(16500000 / memMax) : Infinity
      );
      comparison.scripts_per_tx = Number.isFinite(capacity) ? capacity : null;
    }
    return { ...submission, comparison };
  }

  function value(submission, metric, aggregation) {
    const c = submission.comparison || {};
    const v = metric.aggregate
      ? c[metric.key]?.[aggregation]
      : metric.key in c
        ? c[metric.key]
        : submission.metrics[metric.key];
    // Budget/capacity values must come from the same positive cases as fees.
    if (
      ["scripts_per_tx", "tx_cpu_budget_pct", "tx_memory_budget_pct"].includes(metric.key) &&
      !c.complete
    )
      return null;
    return Number.isFinite(v) && v >= 0 ? v : null;
  }

  function rank(submissions, metric, aggregation) {
    return submissions
      .filter(s => value(s, metric, aggregation) !== null)
      .sort((a, b) => {
        const delta = value(a, metric, aggregation) - value(b, metric, aggregation);
        return (
          (metric.direction === "asc" ? delta : -delta) ||
          String(a.submission_dir).localeCompare(String(b.submission_dir), "en")
        );
      });
  }

  function winners(submissions, metric, aggregation) {
    const rows = rank(submissions, metric, aggregation);
    if (!rows.length) return [];
    const best = value(rows[0], metric, aggregation);
    if (metric.key === "scripts_per_tx" && best === 0) return [];
    return rows.filter(s => value(s, metric, aggregation) === best);
  }

  // Competition ranking: ties share a place (1, 1, 3). Cards show at most three
  // submissions; the full comparison contains every tied participant.
  function podium(submissions, metric, aggregation) {
    const rows = rank(submissions, metric, aggregation);
    let previous;
    let place = 0;
    return rows.slice(0, 3).flatMap((submission, i) => {
      const score = value(submission, metric, aggregation);
      if (score !== previous) place = i + 1;
      previous = score;
      if (place > 3 || (metric.key === "scripts_per_tx" && score === 0)) return [];
      return [{ submission, place, value: score }];
    });
  }

  function readSelection(hash) {
    const params = new URLSearchParams(hash.replace(/^#/, ""));
    const key = params.get("tab");
    return {
      metric: metrics.find(m => m.key === key) || metrics[0],
      aggregation: aggregations.includes(params.get("aggregation"))
        ? params.get("aggregation")
        : "sum",
    };
  }

  function selectionHash(metric, aggregation) {
    return `#tab=${encodeURIComponent(metric.key)}&aggregation=${encodeURIComponent(aggregation)}`;
  }

  function readScenarioSelection(hash, scenario) {
    const params = new URLSearchParams(hash.replace(/^#/, ""));
    const selected = params.get(`scenario.${scenario}`);
    if (!selected) return readSelection(hash);
    const [key, aggregation] = selected.split(":");
    return readSelection(selectionHash({ key }, aggregation));
  }

  function scenarioSelectionHash(hash, scenario, metric, aggregation) {
    const params = new URLSearchParams(hash.replace(/^#/, ""));
    params.set(`scenario.${scenario}`, `${metric.key}:${aggregation}`);
    return `#${params}`;
  }

  function format(metric, n) {
    if (n == null) return "—";
    if (metric.key === "total_fee_lovelace") return (n / 1000000).toFixed(6) + " ₳";
    if (metric.unit === "%") return n.toFixed(2) + "%";
    return n.toLocaleString("en-US", { maximumFractionDigits: 2 }) + " " + metric.unit;
  }

  function description(metric, aggregation) {
    const direction = metric.direction === "asc" ? "Lower is better." : "Higher is better.";
    if (metric.key === "total_fee_lovelace") {
      const basis =
        aggregation === "sum"
          ? "Sum of execution costs across positive measured cases"
          : `${aggregationLabels[aggregation]} execution cost across positive measured cases`;
      return `${basis}, plus the reference-script fee once. Base transaction fees are excluded. ${direction}`;
    }
    if (metric.aggregate)
      return `${aggregationLabels[aggregation]} across positive measured cases. ${direction}`;
    if (metric.key === "scripts_per_tx")
      return "Capacity from the largest measured CPU and memory costs of positive cases. Other transaction limits may reduce it; 0 means none fit. Higher is better.";
    if (metric.unit === "%")
      return "Largest measured positive-case cost as a percentage of the transaction budget. Lower is better; above 100% exceeds the budget.";
    return `${metric.title} in ${metric.unit}. ${direction}`;
  }

  return {
    metrics,
    aggregations,
    aggregationLabels,
    summarize,
    prepareSubmission,
    value,
    rank,
    winners,
    podium,
    readSelection,
    selectionHash,
    readScenarioSelection,
    scenarioSelectionHash,
    format,
    description,
  };
});
