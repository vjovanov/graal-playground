package playground
package inlining

object Helper {
  val foo = (x:Int) => 3*x
}

class TestInline1 extends FileDiffSuite {

  val prefix = "test-out/test1-"

  // def testRet = withOutFile(prefix+"ret") {
    // assert(GraphBuilder.ret(1) == 1)
  // }

  // def testInc = withOutFile(prefix+"inc") {
  //   assert(GraphBuilder.inc(2) == 3)
  // }

  //def testIf = withOutFile(prefix+"if") {
  //  assert(GraphBuilder.cond(2) == 1)
  //  assert(GraphBuilder.cond(-2) == -1)
  //}

  // def testNestedLoop = withOutFile(prefix+"nested-loop") {
  //   GraphBuilder.nestedLoop(2)
  // }

//  def testLoop = withOutFile(prefix+"loop") {
//    println(GraphBuilder.loop(2))
//    println(GraphBuilder.loop(3))
//    println(GraphBuilder.loop(4))
//    println(GraphBuilder.loop(5))
//    println(GraphBuilder.loop(6))
  //   assert(GraphBuilder.loop(2) == 1)
  //   assert(GraphBuilder.loop(3) == 3)
  //   assert(GraphBuilder.loop(4) == 6)
  // }

  /*def testLoopCond = withOutFile(prefix+"loopcond") {
    println(GraphBuilder.loopCond(0))
    println(GraphBuilder.loopCond(4))
    println(GraphBuilder.loopCond(5))
    println(GraphBuilder.loopCond(6))
    // assert(GraphBuilder.loop(2) == 1)
  //   assert(GraphBuilder.loop(3) == 3)
  //   assert(GraphBuilder.loop(4) == 6)
  }*/

  def testMethods = withOutFile(prefix+"methods") {
    assert(GraphBuilder.methodCalls(1) == 1)
  }

  // def testArrays = withOutFile(prefix+"arrays") {
  //   assert(GraphBuilder.arrays(1) == 1)
  // }
}