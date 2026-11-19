# two_party_escrow Scalus 1.5.0 source

**Repository**: <https://github.com/scalus3/scalus>

**Commit**: `7f0d171797f0d705fef5bef38caa968116b45e65`

**Path**: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/twopartyescrow/TwoPartyEscrowValidator.scala`

The validator is one self-contained file. This directory holds a [scala-cli](https://scala-cli.virtuslab.org/) build that compiles it with the released Scalus 1.5.0: [project.scala](project.scala) pins the library `org.scalus::scalus:1.5.0` and the compiler plugin `org.scalus:::scalus-plugin:1.5.0` from Maven Central, so the compiler does not come from the source checkout. [Build.scala](Build.scala) compiles the validator with `Options.releaseUntagged` and writes the UPLC.

## Reproducing the compilation

Run from the root of a UPLC-CAPE checkout. Needs scala-cli and JDK 17 or later.

```bash
git clone https://github.com/scalus3/scalus /tmp/scalus
git -C /tmp/scalus checkout 7f0d171797f0d705fef5bef38caa968116b45e65
scala-cli run submissions/two_party_escrow/Scalus_1.5.0_nau/source \
  /tmp/scalus/scalus-examples/jvm/src/main/scala/scalus/examples/cape/twopartyescrow/TwoPartyEscrowValidator.scala \
  -- /tmp/two_party_escrow.uplc
pretty-uplc /tmp/two_party_escrow.uplc
diff /tmp/two_party_escrow.uplc submissions/two_party_escrow/Scalus_1.5.0_nau/two_party_escrow.uplc
```

The build prints `1040 bytes`. `pretty-uplc` (from the UPLC-CAPE dev shell) applies the layout of the committed file and changes only whitespace, so `diff` prints nothing.
