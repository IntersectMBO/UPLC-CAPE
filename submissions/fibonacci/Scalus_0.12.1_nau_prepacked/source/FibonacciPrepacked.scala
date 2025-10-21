package fibonacci_prepacked

import scalus.*
import scalus.Compiler.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.*
import scalus.uplc.transform.{CaseConstrApply, Inliner}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

/** UPLC-CAPE Fibonacci Prepacked Scenario
  *
  * Optimized implementation using pre-computed Fibonacci numbers stored in a ByteString.
  * Based on Plutarch implementation by SeungheonOh.
  * Generates a lambda function that accepts n as a parameter.
  *
  * Performance characteristics:
  * - O(1) constant-time lookup
  * - Pre-computed values for fib(0) through fib(25)
  * - Each Fibonacci number encoded as 3 bytes in big-endian format
  */

// Helper to generate Fibonacci sequence as ByteString
// Each Fibonacci number is encoded as 3 bytes in big-endian format
def fibSeqByteString(n: Int): ByteString = {
    @tailrec
    def fib(a: Int, b: Int, index: Int, acc: Array[Byte]): Array[Byte] =
        if index >= n then acc
        else
            val offset = index * 3
            acc(offset) = ((a >> 16) & 0xff).toByte
            acc(offset + 1) = ((a >> 8) & 0xff).toByte
            acc(offset + 2) = (a & 0xff).toByte
            fib(b, a + b, index + 1, acc)

    ByteString.fromArray(fib(0, 1, 0, new Array[Byte](n * 3)))
}

// Pre-computed Fibonacci sequence up to fib(25)
val packedFibonacci = fibSeqByteString(26)

@main def compileFibonacciPrepacked(): Unit =
  // Compile Scalus function to lookup Fibonacci number from packed ByteString
  val fib = compile: (packedFibonacci: ByteString) =>
      (x: BigInt) =>
          if x < BigInt(0) then x
          else byteStringToInteger(true, sliceByteString(x * 3, 3, packedFibonacci))

  // Apply the packed fibonacci ByteString
  val fibTerm = fib.toUplc() $ packedFibonacci.asTerm

  // Optimize the term by inlining the constant ByteString
  val optimized = fibTerm |> Inliner.apply |> CaseConstrApply.apply
  val program = optimized.plutusV3

  // Test the compiled program
  given PlutusVM = PlutusVM.makePlutusV3VM()
  val testResult = (program $ 10.asTerm).term.evaluateDebug
  println(s"✓ Fibonacci(10) = $testResult") // Should print 55

  // Write to src/fibonacci_prepacked/fibonacci.uplc file
  val uplcText = program.pretty
      .render(80)
      .replace(".", "_") // Sanitize all dots to underscores
      .replace("$", "_") // Sanitize dollar signs to underscores
      .replace("1_1_0", "1.1.0") // Restore version number
  val outputPath = Paths.get("src/fibonacci_prepacked/fibonacci.uplc")
  Files.createDirectories(outputPath.getParent)
  Files.write(outputPath, uplcText.getBytes(StandardCharsets.UTF_8))

  println(s"✓ Successfully compiled FibonacciPrepacked to fibonacci.uplc")
  println(s"  Output: ${outputPath.toAbsolutePath}")
  println(s"  Size: ${uplcText.length} bytes")
  println(s"  Scenario: fibonacci_prepacked")
