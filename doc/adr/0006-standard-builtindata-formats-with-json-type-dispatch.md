# Standard BuiltinData formats with JSON-type dispatch

- Status: accepted
- Date: 2026-04-30
- Tags: testing, builtin-data, parsing, interop

Technical Story: Replace the custom compact BuiltinData notation introduced in
[ADR-0005](0005-custom-builtindata-text-encoding-for-test-cases.md) with two
standard, widely-tooled formats so that compiler submissions can reuse existing
parsers.

## Context and Problem Statement

In issue [#148](https://github.com/IntersectMBO/UPLC-CAPE/issues/148), nau (the
author of the Scalus compiler) reported that the bespoke compact notation used
in `cape-tests.json` (`0(#cafe 1000)`, `[1 2 3]`, `{1:42}`, `#deadbeef`) is
not formally specified anywhere outside this repository, forcing every compiler
submission that wants to read CAPE test cases to ship its own parser. The
existing implementation lives in `lib/PlutusCore/Data/Compact/Parser.hs` (~150
lines, Megaparsec).

How should we encode BuiltinData values in `cape-tests.json` to lower the
barrier for new compiler submissions while preserving authoring ergonomics?

## Decision Drivers

- **Toolchain compatibility**: every Cardano toolchain should be able to parse
  the format out of the box.
- **Authoring ergonomics**: short scalars should stay terse; complex datums
  shouldn't force readers to count brackets.
- **Specifiability**: the format should already be documented somewhere
  authoritative (CIP, Plutus spec, cardano-cli docs).
- **Single source of truth**: avoid the maintenance cost of a hand-rolled
  parser.

## Considered Options

- **Option A (Plutus JSON detailed schema)** — `{"constructor": 0, "fields":
  [{"int": 42}]}`, `{"int": 42}`, `{"bytes": "deadbeef"}`, `{"list": [...]}`,
  `{"map": [{"k": ..., "v": ...}]}`. Used by `cardano-cli`, Aiken, Plu-ts,
  bloxbean.
- **Option B (UPLC text Data syntax)** — `Constr 0 [I 42]`, `I 42`,
  `B #deadbeef`, `List [...]`, `Map [(...)]`. Defined in the Plutus Core
  specification, parsed by every UPLC-aware tool inside `(con data ...)`.
- **Custom format (ADR-0005)** — keep the bespoke compact notation.

## Decision Outcome

**Support both Option A and Option B simultaneously**, dispatching on the JSON
type of the `value` field — no extra discriminator:

| JSON shape | Format                                 | Example                                                  |
| ---------- | -------------------------------------- | -------------------------------------------------------- |
| `string`   | UPLC text Data syntax                  | `"Constr 0 [I 1000, B #cafe]"`                           |
| `object`   | Plutus JSON detailed schema            | `{"constructor": 0, "fields": [{"int": 1000}]}`          |

A string of the form `"@name"` continues to mean a reference into
`data_structures` and is resolved before parsing.

The custom format is **removed entirely** — `lib/PlutusCore/Data/Compact/`
modules are deleted, the schema version is bumped from `1.0.0` to `2.0.0`, and
all eight existing `cape-tests.json` files are migrated.

### Migration of existing `cape-tests.json` files

- The five scenarios with Scalus submissions (`factorial`,
  `factorial_naive_recursion`, `fibonacci`, `fibonacci_naive_recursion`,
  `htlc`) are converted to **Option A** because that is the format nau
  explicitly preferred in #148 and Scala/JVM tooling has trivial support for
  it through cardano-cli-compatible libraries.
- The remaining three scenarios (`ecd`, `two_party_escrow`, `linear_vesting`)
  are converted to **Option B** as the closest readable analogue of the
  current compact notation. Authors are free to switch individual values to
  Option A later.

A one-shot migration script lives at `scripts/migrate-cape-tests.py` and is
deleted after the migration commits.

### Implementation Details

- `lib/Cape/Data/UplcText.hs` wraps `PlutusCore.Parser.conData` for parsing
  and provides `renderUplcDataText` for printing.
- `lib/Cape/Data/PlutusJson.hs` implements `parsePlutusJsonData` and
  `encodePlutusJsonData` directly (no `cardano-api` dep needed; the schema is
  ~50 lines).
- `lib/Cape/Data/Decode.hs` exports the dispatch:
  `decodeBuiltinData :: Aeson.Value -> Either Text Data`.
- `Cape.Tests` holds a `decodeBuiltinDataValue` helper that handles
  `@reference` resolution, then delegates to `decodeBuiltinData`. All slots
  that previously took compact-format strings (test inputs, redeemers,
  datums, output datums, asset bytes) flow through it.
- `resolveTestInput` returns a new `ResolvedInput` ADT (`ResolvedUplc Text` /
  `ResolvedBuiltinData V3.BuiltinData`) eliminating the previous
  Data → text → Data round-trip in `resolveScriptContextInput`.

### Positive Consequences

- New compiler submissions reuse off-the-shelf parsers (`cardano-cli`-style
  JSON for Option A, the project's UPLC parser for Option B).
- One less hand-rolled parser to maintain, plus one less Megaparsec dep
  surface.
- The schema is now self-describing in a recognised standard, lowering the
  onboarding cost for new contributors.

### Negative Consequences

- Existing test files become more verbose in places (`Constr 0 [I 1]` is
  longer than `0(1)`; full JSON form even more so).
- ADR-0005's compact notation is no longer accepted; out-of-tree forks that
  relied on it must migrate.

## Links

- Originating issue: [#148](https://github.com/IntersectMBO/UPLC-CAPE/issues/148)
- Supersedes [ADR-0005](0005-custom-builtindata-text-encoding-for-test-cases.md)
- cardano-cli `ScriptDataJsonDetailedSchema` documentation
- Plutus Core specification, section on built-in constants (`(con data ...)`
  syntax)
