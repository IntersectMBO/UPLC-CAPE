# factorial Pebble 0.5.0 source

**Repository**: <https://github.com/HarmonicLabs/pebble>

**Tag**: `v0.5.0`

**Commit**: `4e0e403b81bd18033b55529eea3337747c06807c`

**Path**: `examples/uplc-cape/src/factorial.pebble` (vendored in this directory)

## Reproducing the compilation

```bash
git clone https://github.com/HarmonicLabs/pebble pebble
cd pebble
git checkout 4e0e403b81bd18033b55529eea3337747c06807c
npm i -g @harmoniclabs/pebble-cli@0.5.0
cd examples/uplc-cape
pebble export --function-name factorial --entry ./src/factorial.pebble
mv out/out.flat out/factorial.flat
pebble uplc pretty --canonical -i ./out/factorial.flat -o ./out/factorial.uplc
```

The resulting `out/factorial.uplc` is the submitted program; the committed artifact is the same program laid out by this repository's `pretty-uplc` formatter (`treefmt`).
