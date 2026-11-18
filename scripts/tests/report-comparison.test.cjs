const assert = require("node:assert/strict");
const { test } = require("node:test");
const {
  mkdtempSync,
  mkdirSync,
  writeFileSync,
  readFileSync,
  symlinkSync,
  rmSync,
} = require("node:fs");
const { tmpdir } = require("node:os");
const path = require("node:path");
const { execFileSync } = require("node:child_process");
const C = require("../cape-subcommands/submission/report-comparison.js");
const root = path.resolve(__dirname, "../..");
const metric = key => C.metrics.find(m => m.key === key);
const cases = ["a", "b", "c"];

function submission(name, cpus, memories = cpus, referenceFee = 15) {
  return C.prepareSubmission(
    {
      submission_dir: name,
      metrics: { reference_script_fee_lovelace: referenceFee, script_size_bytes: 1 },
      positive_evaluations: cpus.map((cpu, i) => ({
        name: cases[i],
        cpu_units: cpu,
        memory_units: memories[i],
      })),
    },
    cases
  );
}

test("changing aggregation changes the winner, preserving every tie", () => {
  const a = submission("a", [1, 1, 100]);
  const b = submission("b", [20, 20, 20]);
  const b2 = submission("b2", [20, 20, 20]);
  assert.deepEqual(C.winners([a, b2, b], metric("cpu_units"), "sum"), [b, b2]);
  assert.deepEqual(C.winners([a, b], metric("cpu_units"), "median"), [a]);
  assert.deepEqual(C.winners([a, b], metric("cpu_units"), "maximum"), [b]);
  assert.deepEqual(C.winners([a, b], metric("cpu_units"), "minimum"), [a]);
});

test("mean and even-count median preserve fractions", () => {
  assert.deepEqual(C.summarize([9, 1, 2, 3]), {
    sum: 15,
    mean: 3.75,
    median: 2.5,
    minimum: 1,
    maximum: 9,
  });
  assert.equal(C.summarize([]), null);
});

test("podium medals reflect shared places with at most three submissions", () => {
  const a = submission("a", [1, 1, 1]);
  const a2 = submission("a2", [1, 1, 1]);
  const b = submission("b", [2, 2, 2]);
  const c = submission("c", [3, 3, 3]);
  const c2 = submission("c2", [3, 3, 3]);
  const places = rows => rows.map(p => [p.submission.submission_dir, p.place]);
  assert.deepEqual(places(C.podium([c, b, a2, a], metric("cpu_units"), "sum")), [
    ["a", 1],
    ["a2", 1],
    ["b", 3],
  ]);
  assert.deepEqual(places(C.podium([c2, c, b, a], metric("cpu_units"), "sum")), [
    ["a", 1],
    ["b", 2],
    ["c", 3],
  ]);
  const oversized = submission("oversized", [20000000, 20000000, 20000000]);
  assert.deepEqual(C.podium([oversized], metric("scripts_per_tx"), "sum"), []);
  assert.deepEqual(C.podium([], metric("cpu_units"), "sum"), []);
});

test("scenario selections are independent and survive a link round-trip", () => {
  let hash = C.scenarioSelectionHash("", "ecd", metric("cpu_units"), "median");
  hash = C.scenarioSelectionHash(hash, "fibonacci", metric("memory_units"), "mean");
  assert.equal(C.readScenarioSelection(hash, "ecd").metric.key, "cpu_units");
  assert.equal(C.readScenarioSelection(hash, "ecd").aggregation, "median");
  assert.equal(C.readScenarioSelection(hash, "fibonacci").metric.key, "memory_units");
  assert.equal(C.readScenarioSelection(hash, "fibonacci").aggregation, "mean");
  assert.equal(C.readScenarioSelection(hash, "htlc").metric.key, "total_fee_lovelace");
  assert.equal(C.readScenarioSelection(hash, "htlc").aggregation, "sum");
  const url = new URL("https://example.test/benchmarks/ecd.html");
  url.searchParams.set("index", hash);
  const restored = new URL(url.href).searchParams.get("index");
  const updated = C.scenarioSelectionHash(restored, "ecd", metric("cpu_units"), "maximum");
  assert.equal(C.readScenarioSelection(updated, "ecd").aggregation, "maximum");
  assert.equal(C.readScenarioSelection(updated, "fibonacci").aggregation, "mean");
  assert.equal(
    C.readScenarioSelection("#tab=memory_units&aggregation=mean", "ecd").metric.key,
    "memory_units"
  );
  assert.equal(
    C.readScenarioSelection("#scenario.ecd=bad:bad", "ecd").metric.key,
    "total_fee_lovelace"
  );
});

test("fee median is calculated from paired execution costs", () => {
  const s = submission("paired", [10000000, 0, 0], [0, 10000, 0], 15);
  assert.deepEqual(s.comparison.total_fee_lovelace, {
    sum: 1313,
    mean: 448,
    median: 592,
    minimum: 15,
    maximum: 736,
  });
  // median(CPU) + median(memory) would incorrectly give only the reference fee.
  assert.equal(s.comparison.cpu_units.median, 0);
  assert.equal(s.comparison.memory_units.median, 0);
});

test("fee sum charges the reference fee once", () => {
  const s = submission("fee", [10000000, 20000000, 30000000], [10000, 20000, 30000], 150);
  assert.equal(s.comparison.total_fee_lovelace.sum, 7938);
  assert.equal(s.comparison.total_fee_lovelace.maximum, 4044);
  assert.equal(s.comparison.scripts_per_tx, 333);
});

test("fee arithmetic and capacity agree with the repository's protocol parameters", () => {
  const source = readFileSync(path.join(root, "lib/Cape/Protocol/Parameters.hs"), "utf8");
  const rational = name => {
    const match = source.match(new RegExp(`${name} = ([\\d_]+) % ([\\d_]+)`));
    assert.ok(match, `cannot find ${name}`);
    return match.slice(1).map(v => Number(v.replaceAll("_", "")));
  };
  const integer = name =>
    Number(source.match(new RegExp(`${name} = ([\\d_]+)`))[1].replaceAll("_", ""));
  const [memN, memD] = rational("priceMemory");
  const [cpuN, cpuD] = rational("priceSteps");
  const s = submission("parameters", [1098765, 2000000, 3000000], [12345, 30000, 90000]);
  assert.equal(
    s.comparison.total_fee_lovelace.sum,
    Math.ceil((132345 * memN) / memD + (6098765 * cpuN) / cpuD) + 15
  );
  assert.equal(
    s.comparison.scripts_per_tx,
    Math.min(Math.floor(integer("maxTxMemory") / 90000), Math.floor(integer("maxTxCpu") / 3000000))
  );
});

test("incomplete, duplicated, invalid and absent measurements never win by looking cheap", () => {
  const good = submission("good", [10, 20, 30]);
  const missing = submission("missing", [0]);
  const duplicate = C.prepareSubmission(
    {
      metrics: {},
      positive_evaluations: [
        { name: "a", cpu_units: 0, memory_units: 0 },
        { name: "a", cpu_units: 0, memory_units: 0 },
        { name: "c", cpu_units: 0, memory_units: 0 },
      ],
    },
    cases
  );
  const invalid = submission("invalid", [1, -1, 1]);
  const absent = C.prepareSubmission({ metrics: { total_fee_lovelace: 0 } }, cases);
  for (const s of [missing, duplicate, invalid, absent]) {
    assert.equal(C.value(s, metric("cpu_units"), "sum"), null);
    assert.equal(C.value(s, metric("total_fee_lovelace"), "sum"), null);
    assert.equal(C.value(s, metric("scripts_per_tx"), "sum"), null);
  }
  assert.deepEqual(
    C.winners([missing, duplicate, invalid, absent, good], metric("cpu_units"), "sum"),
    [good]
  );
});

test("missing reference-script fee is not interpreted as free", () => {
  const s = submission("no-ref", [1, 2, 3], [1, 2, 3], null);
  assert.equal(C.value(s, metric("total_fee_lovelace"), "sum"), null);
  assert.equal(C.value(s, metric("cpu_units"), "sum"), 6);
});

test("capacity selects the maximum; all-zero capacity has no winner", () => {
  const small = submission("small", [1000000, 1000000, 1000000]);
  const large = submission("large", [20000000, 20000000, 20000000]);
  assert.deepEqual(C.winners([large, small], metric("scripts_per_tx"), "sum"), [small]);
  assert.deepEqual(C.winners([large], metric("scripts_per_tx"), "sum"), []);
  assert.equal(C.value(large, metric("scripts_per_tx"), "sum"), 0);
});

test("selection round-trips in links, supports existing tab links and rejects unknown values", () => {
  const selected = C.readSelection(C.selectionHash(metric("memory_units"), "median"));
  assert.equal(selected.metric.key, "memory_units");
  assert.equal(selected.aggregation, "median");
  assert.equal(C.readSelection("#tab=cpu_units").aggregation, "sum");
  for (const hash of ["", "#tab=unknown&aggregation=unknown", "#tab=%E0%A4%A"]) {
    assert.equal(C.readSelection(hash).metric.key, "total_fee_lovelace");
    assert.equal(C.readSelection(hash).aggregation, "sum");
  }
});

test("generator exports only the scenario's positive cases without rewriting metrics", () => {
  const fixture = mkdtempSync(path.join(tmpdir(), "cape-report-comparison-"));
  const write = (name, data) => {
    const dest = path.join(fixture, name);
    mkdirSync(path.dirname(dest), { recursive: true });
    writeFileSync(dest, typeof data === "string" ? data : JSON.stringify(data));
  };
  try {
    symlinkSync(path.join(root, "scripts"), path.join(fixture, "scripts"));
    write("uplc-cape-logo.png", "fixture");
    write("scenarios/sample/sample.md", "---\noptimization: compiler-only\nworkload: synthetic\n---\n");
    const definition = (name, extra = {}) => ({ name, expected: { type: "value" }, ...extra });
    write("scenarios/sample/cape-tests.json", {
      measurements: [
        definition("a"),
        definition("b"),
        definition("pending", { pending: true }),
        definition("rejection", { expected: { type: "error" } }),
      ],
      checks: [definition("check")],
    });
    const metadata = {
      compiler: { name: "Fixture", version: "1" },
      contributors: [{ name: "User" }],
    };
    const evaluation = (name, result = "success", included = true) => ({
      name,
      description: name,
      cpu_units: 1000000,
      memory_units: 1000,
      execution_result: result,
      included_in_aggregates: included,
    });
    const measurements = {
      cpu_units: { sum: 10000000 },
      memory_units: { sum: 10000 },
      script_size_bytes: 10,
      term_size: 20,
      execution_fee_lovelace: 0,
      reference_script_fee_lovelace: 150,
      total_fee_lovelace: 0,
      scripts_per_tx: 0,
      tx_cpu_budget_pct: 0,
      tx_memory_budget_pct: 0,
    };
    const source = {
      timestamp: "2026-11-17T00:00:00Z",
      measurements,
      evaluations: [
        evaluation("a"),
        evaluation("b"),
        evaluation("pending", "success", false),
        evaluation("rejection", "error"),
        evaluation("check", "success", false),
      ],
    };
    write("submissions/sample/Fixture_1_User/metadata.json", metadata);
    const sourcePath = "submissions/sample/Fixture_1_User/metrics.json";
    write(sourcePath, source);
    const before = readFileSync(path.join(fixture, sourcePath), "utf8");
    // Prefer this checkout's dispatcher over an installed (possibly older) cape.
    write(
      "bin/cape",
      `#!/usr/bin/env bash\nexec bash '${path.join(root, "scripts/cape.sh")}' --project-root "$PROJECT_ROOT" "$@"\n`
    );
    require("node:fs").chmodSync(path.join(fixture, "bin/cape"), 0o755);
    const report = (...args) =>
      execFileSync(
        "bash",
        [path.join(root, "scripts/cape-subcommands/submission/report.sh"), ...args],
        {
          env: {
            ...process.env,
            PROJECT_ROOT: fixture,
            PATH: `${fixture}/bin:${process.env.PATH}`,
          },
          timeout: 30000,
          stdio: "pipe",
        }
      );
    report("sample");
    const json = JSON.parse(readFileSync(path.join(fixture, "report/benchmarks/sample.json")));
    assert.deepEqual(json.positive_cases, ["a", "b"]);
    assert.deepEqual(
      json.submissions[0].positive_evaluations.map(e => e.name),
      ["a", "b"]
    );
    const prepared = C.prepareSubmission(json.submissions[0], json.positive_cases);
    assert.equal(C.value(prepared, metric("total_fee_lovelace"), "sum"), 410);
    const html = readFileSync(path.join(fixture, "report/index.html"), "utf8");
    assert.match(html, /Ranking criterion for/);
    assert.match(html, /data-benchmark="sample"/);
    assert.match(
      readFileSync(path.join(fixture, "report/benchmarks/sample.html"), "utf8"),
      /comparison-aggregation/
    );
    // A placeholder has no data JSON, and --keep-existing must preserve the
    // previously rendered scenario in the index as well as on disk.
    write("scenarios/empty/empty.md", "---\noptimization: unrestricted\nworkload: synthetic\n---\n");
    report("--keep-existing", "empty");
    const retained = readFileSync(path.join(fixture, "report/index.html"), "utf8");
    assert.match(retained, /data-benchmark="sample"/);
    assert.match(retained, /data-benchmark="empty"/);
    assert.match(
      readFileSync(path.join(fixture, "report/benchmarks/empty.html"), "utf8"),
      /No Submissions Yet/
    );
    write("report/benchmarks/empty.json", json);
    report("--keep-existing", "empty");
    assert.throws(() => readFileSync(path.join(fixture, "report/benchmarks/empty.json")), {
      code: "ENOENT",
    });
    report("empty");
    const fresh = readFileSync(path.join(fixture, "report/index.html"), "utf8");
    assert.match(fresh, /data-benchmark="empty"/);
    assert.doesNotMatch(fresh, /data-benchmark="sample"/);
    assert.equal(readFileSync(path.join(fixture, sourcePath), "utf8"), before);
    assert.equal(
      readFileSync(path.join(fixture, "report/report-comparison.js"), "utf8"),
      readFileSync(
        path.join(root, "scripts/cape-subcommands/submission/report-comparison.js"),
        "utf8"
      )
    );
  } finally {
    rmSync(fixture, { recursive: true, force: true });
  }
});
