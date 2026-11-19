import scalus.compiler.Options
import scalus.examples.cape.twopartyescrow.TwoPartyEscrowValidator
import scalus.uplc.PlutusV3

import java.nio.file.{Files, Path}

/** Compiles `TwoPartyEscrowValidator` with the options of the Scalus CAPE submissions and writes
  * the program as textual UPLC to the path given as the first argument.
  */
@main def build(out: String): Unit = {
    given Options = Options.releaseUntagged
    val program = PlutusV3.compile(TwoPartyEscrowValidator.validate).program
    Files.writeString(Path.of(out), program.show)
    println(s"${program.cborEncoded.length} bytes -> $out")
}
