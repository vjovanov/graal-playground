package playground
package inlining

object Helper {
  val foo = (x:Int) => 3*x
}

class TestInline1 extends FileDiffSuite {

  val prefix = "test-out/test1-"

  // compiled function references other function object (stored in external field)

  def testADefault = withOutFileChecked(prefix+"inc") {
    assert(GraphBuilder.inc(2) == 3)
  }
}
