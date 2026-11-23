import scalus.compiler.Options
import scalus.examples.cape.twopartyescrow.TwoPartyEscrowValidator
import scalus.uplc.{DeBruijn, PlutusV3}

import java.nio.file.{Files, Path}

/** Compiles `TwoPartyEscrowValidator` with the options of the Scalus CAPE submissions and writes
  * the program as textual UPLC to the path given as the first argument.
  *
  * The compiler names binders with a counter that is global to the JVM, so the names change with
  * the build environment while the Flat bytes do not. The round trip through De Bruijn indices
  * renames every binder to `i0`, `i1`, ... in the order the binders occur. The text then depends
  * only on the program, and every build that gives the same bytes gives the same text.
  */
@main def build(out: String): Unit = {
    given Options = Options.releaseUntagged
    val compiled = PlutusV3.compile(TwoPartyEscrowValidator.validate).program
    val program = DeBruijn.fromDeBruijnProgram(DeBruijn.deBruijnProgram(compiled))
    Files.writeString(Path.of(out), program.show)
    println(s"${program.cborEncoded.length} bytes -> $out")
}
