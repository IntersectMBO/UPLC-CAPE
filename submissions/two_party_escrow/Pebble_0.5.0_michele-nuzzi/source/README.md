# two_party_escrow Pebble 0.5.0 source

**Repository**: <https://github.com/HarmonicLabs/pebble>

**Tag**: `v0.5.0`

**Commit**: `4e0e403b81bd18033b55529eea3337747c06807c`

**Path**: `examples/uplc-cape/src/two-party-escrow.pebble` (vendored in this directory)

## Reproducing the compilation

```bash
git clone https://github.com/HarmonicLabs/pebble pebble
cd pebble
git checkout 4e0e403b81bd18033b55529eea3337747c06807c
npm i -g @harmoniclabs/pebble-cli@0.5.0
cd examples/uplc-cape
pebble export --function-name two_party_escrow --entry ./src/two-party-escrow.pebble
mv out/out.flat out/two_party_escrow.flat
pebble uplc pretty --canonical -i ./out/two_party_escrow.flat -o ./out/two_party_escrow.uplc
```

The resulting `out/two_party_escrow.uplc` is the submitted program; the committed artifact is the same program laid out by this repository's `pretty-uplc` formatter (`treefmt`).
