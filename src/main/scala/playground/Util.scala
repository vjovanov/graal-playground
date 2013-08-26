package playground

import java.util.concurrent.Callable

import com.oracle.graal.{java=>J,_}
import com.oracle.graal.debug._         // Debug
import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.api.code._      // Assumptions
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import com.oracle.graal.compiler._      // GraalCompiler
import com.oracle.graal.java._          // GraphBuilderConfiguration
import com.oracle.graal.graph._
import com.oracle.graal.nodes.{java=>J,_}   // StructuredGraph
import com.oracle.graal.nodes.java._        // MethodCallTargetNode
import com.oracle.graal.phases._   // PhasePlan
import com.oracle.graal.phases.PhasePlan.PhasePosition
import com.oracle.graal.phases.common._
import com.oracle.graal.debug._;
import collection.JavaConversions._
import com.oracle.graal.debug.internal._
import com.oracle.graal.printer._

object Util {

  val runtime = HotSpotGraalRuntime.graalRuntime().getRuntime()
  val compiler = HotSpotGraalRuntime.graalRuntime().getCompilerToVM()

  def topScope[A](method: ResolvedJavaMethod)(body: => A) = {
        import GraalDebugConfig._
    val hotspotDebugConfig =
      new GraalDebugConfig(
       Log.getValue(),
       Meter.getValue(),
       Time.getValue(),
       "Playground",   // Dump.getValue(),
       "playground.GraphBuilder$$anonfun$1.apply$mcII$sp",// MethodFilter.getValue()
       System.out,
       List(new GraphPrinterDumpHandler())
      )
    Debug.setConfig(hotspotDebugConfig)

    Debug.setConfig(hotspotDebugConfig)
    Debug.scope("Playground", method, new Callable[A] {
        def call: A = {
          body
        }
    });
  }

  def phase(f: StructuredGraph => Unit) = new Phase {
    def run(graph: StructuredGraph) = f(graph)
  }

  def printGraph(s: String, verbosity: Node.Verbosity = Node.Verbosity.Short) = phase { graph =>
    println("===== " + s)
    graph.getNodes.foreach(n => println(n.toString(verbosity) + n.inputs().map(_.toString(Node.Verbosity.Id)).mkString("(",",",")")))
    println("----- " + s + " method calls ")
    graph.getNodes(classOf[InvokeNode]).foreach(printInvoke)
  }

  def printInvoke(invoke: InvokeNode): Unit = {
    if (invoke.callTarget.isInstanceOf[MethodCallTargetNode]) {
      val methodCallTarget = invoke.callTarget()

      println("  invoke: " + invoke)
      println("    args: " + methodCallTarget.arguments())

      val assumptions = new Assumptions(true)

      // TODO: this could be useful
      // val info = InliningUtil.getInlineInfo(methodCallTarget.invoke(), assumptions, OptimisticOptimizations.ALL)
      // println("    info: " + info)
    } else {
      println("Invoke Node: " + invoke)
    }
  }

}

