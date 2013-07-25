package playground
package inlining

object Helper {
  val foo = (x:Int) => 3*x
}

class TestInline1 extends FileDiffSuite {

  val prefix = "test-out/test1-"

  // def testRet = withOutFileChecked(prefix+"ret") {
    // assert(GraphBuilder.ret(1) == 1)
  // }

  // def testInc = withOutFileChecked(prefix+"inc") {
  //   assert(GraphBuilder.inc(2) == 3)
  // }

  //def testIf = withOutFileChecked(prefix+"if") {
  //  assert(GraphBuilder.cond(2) == 1)
  //  assert(GraphBuilder.cond(-2) == -1)
  //}

  // def testNestedLoop = withOutFileChecked(prefix+"nested-loop") {
  //   GraphBuilder.nestedLoop(2)
  // }

  def testLoop = withOutFileChecked(prefix+"loop") {
    assert(GraphBuilder.loop(1) == 0)
  //   assert(GraphBuilder.loop(2) == 1)
  //   assert(GraphBuilder.loop(3) == 3)
  //   assert(GraphBuilder.loop(4) == 6)
  }

  // def testMethods = withOutFileChecked(prefix+"methods") {
  //   assert(GraphBuilder.methodCalls(1) == 1)
  // }

  // def testArrays = withOutFileChecked(prefix+"arrays") {
  //   assert(GraphBuilder.arrays(1) == 1)
  // }
}