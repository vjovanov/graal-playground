package playground
package inlining

object Helper {
  val foo = (x:Int) => 3*x
}

class TestInline1 extends FileDiffSuite {

  val prefix = "test-out/test1-"

  def testRet = withOutFileChecked(prefix+"ret") {
    assert(GraphBuilder.ret(1) == 1)
  }

  def testInc = withOutFileChecked(prefix+"inc") {
    assert(GraphBuilder.inc(2) == 3)
  }

  def testIf = withOutFileChecked(prefix+"if") {
    assert(GraphBuilder.cond(2) == 1)
    assert(GraphBuilder.cond(-2) == -1)
  }

}