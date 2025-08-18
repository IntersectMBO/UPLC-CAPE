# Relude guidance for UPLC-CAPE

This document explains the Relude custom Prelude, how it differs from the stock Prelude, and how to use it effectively in this repository.

## Why Relude

Relude is a modern, safe, minimal alternative to the standard Prelude focused on:

- Safety by avoiding partial functions and surfacing failure via types
- Type-driven design (e.g., preferring NonEmpty where emptiness is invalid)
- Performance defaults (Text/ByteString over String; space-leak-free folds)
- Convenience without large dependency footprints
- Clear module structure with optional Extra.\* modules

## Key differences vs stock Prelude

- Partial list functions become total:
  - `head`, `last`, `tail`, `init` operate on `NonEmpty a` instead of `[a]`
  - Utilities to safely consume lists are provided (e.g., `uncons :: [a] -> Maybe (a,[a])`, `viaNonEmpty :: (NonEmpty a -> b) -> [a] -> Maybe b`)
- Safer defaults and nudges:
  - `undefined` triggers a compiler warning; prefer explicit failure (e.g., `error`, or better: return `Either`/`Maybe`/`ExceptT`)
- Text-first IO and printing helpers:
  - `putText`, `putTextLn`, `readFileText`, `writeFileText` (prefer Text over String)
- Collections and utilities reexported:
  - Common types and functions from `containers`, `bytestring`, `text`, etc., are in scope or easy to import
- Better folds and numerics:
  - `sum`, `product`, and related folds are implemented to avoid common space leaks
- Useful extras available on demand (not exported by default):
  - `Relude.Extra.*` modules provide additional utilities you can opt into
  - Potentially unsafe helpers live in `Relude.Unsafe` and must be imported explicitly

## Import and project setup in this repo

UPLC-CAPE uses Relude as the default Prelude via Cabal mixins (preferred). This means individual modules do not need to import `Relude` explicitly.

Recommended Cabal configuration (conceptual):

- Add Relude dependency (project guidance suggests: `relude ^>= 1.2.2`)
- Use mixins to make Relude the project Prelude and hide base’s Prelude:
  - `mixins: base (Prelude as OldPrelude), relude (Relude as Prelude)` or equivalently `base hiding (Prelude), relude (Relude as Prelude)`
- Keep common language extensions in Cabal via `default-extensions` (see repository standards)

Per-file imports style (consistent with repo guidelines):

- Do not hide the Prelude in modules; Relude is already the Prelude
- Prefer qualified imports for large namespaces:
  - `import qualified Data.Text as Text`
  - `import qualified Data.Map.Strict as Map`
  - `import qualified Data.ByteString as BS`
- Import optional Relude modules explicitly and locally when needed:
  - `import Relude.Extra.<Module>` for opt‑in utilities
  - `import Relude.Unsafe qualified as Unsafe` when you intentionally need unsafe helpers (keep usage minimal and contained)

Notes for PlutusTx/on-chain code in this repo:

- Code compiled to on-chain (inside Template Haskell quotes) should use PlutusTx modules; Relude only affects the host Haskell code
- Keep on‑chain and off‑chain imports separated; qualify PlutusTx imports as needed (e.g., `PlutusTx`-qualified)

## Everyday recipes

- Prefer Text everywhere:
  - Use `putTextLn` for logging; `readFileText`/`writeFileText` for files
  - Convert with `toText`/`show` as appropriate; avoid `String` unless interop requires it
- Safe list processing:
  - Destructure with `uncons` or process via `viaNonEmpty` when you need `head`/`last` semantics
  - Reach for `NonEmpty` in APIs where emptiness is invalid
- Effects and loops:
  - Use standard `for_`, `forM_`, `when`, `unless`; Relude provides lifted and convenient variants consistent with common Haskell practice
- Errors and exceptions:
  - Prefer returning `Either`/`Maybe`/`ExceptT` for recoverable errors
  - Reserve `error` for truly unrecoverable programmer mistakes (will crash)
  - Avoid `undefined`; it emits a warning and hides intent

## Working with Relude.Extra and Unsafe

- `Relude.Extra.*` modules are opt‑in; import them locally to a module when they add clarity or performance
- `Relude.Unsafe` is not reexported by default; import it explicitly and in a narrow scope only when you need the functionality and have proven it safe in context

## HLint and style

- Relude ships HLint rules that suggest safer/clearer variants; enable HLint in your editor/CI to benefit from these suggestions
- Follow the repository’s import style rules:
  - Prefer Relude Prelude via mixins
  - Use qualified imports to resolve name clashes rather than hiding Prelude names
  - Keep imports minimal and consistent

## Migration tips (from stock Prelude)

- Replace partial list accessors with `NonEmpty` or safe combinators (`uncons`, `viaNonEmpty`)
- Switch IO helpers to Text versions (`putTextLn`, `readFileText`, etc.)
- Replace ad‑hoc `String` with `Text`; keep conversions at the boundary
- Use explicit error channels (`Either`/`Maybe`) rather than exceptions or `undefined`

## Do and Don’t

Do:

- Use `NonEmpty` when a list must not be empty
- Prefer Text and bytes over String for performance
- Keep unsafe/partial helpers behind explicit imports and small scopes
- Qualify large external modules (Text, ByteString, Map, Set)

Don’t:

- Import `Prelude hiding (...)` — Relude is already your Prelude
- Rely on partial functions or `undefined`
- Mix on‑chain PlutusTx code with Relude-only features inside TH quotations

## References

- Relude Haddocks and README (Hackage) provide comprehensive, example‑driven docs
- Explore `Relude.Extra.*` for optional utilities and `Relude.Unsafe` for explicitly unsafe helpers
