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
import com.oracle.graal.phases.common._
import com.oracle.graal.phases.PhasePlan.PhasePosition
import com.oracle.graal.nodes.calc._
import com.oracle.graal.debug.internal._;
import collection.JavaConversions._

trait LancetIR {
  var stackIndex = 0
  case class Sym(id: Int)
  trait Exp {
    val index = stackIndex
    stackIndex += 1
  }
  
  var syms: Map[Sym, Exp] 

  case class Const(l: Int) extends Exp
  case class Var() extends Exp
  case class Plus(l: Sym, r: Sym) extends Exp
  case class Times(l: Sym, r: Sym) extends Exp  
}

class TestLancetBuilder(
  val ir: LancetIR,
  rt: MetaAccessProvider, 
  config: GraphBuilderConfiguration, 
  optimisticOpts: OptimisticOptimizations) 
extends LancetGraphBuilder(rt, config, optimisticOpts) {    

  def generateGraalIR(): Unit ={
    lastInstr = currentGraph.start()
    // finish the start block
    lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));
    frameState.cleanupDeletedPhis();

    // TODO exact numbers will be acquired from a future traversal
    val bs = new java.util.BitSet()
    bs.set(1)
    frameState.clearNonLiveLocals(bs)

    import ir._

    def pushOp(e: Sym) = syms(e) match {      
      case Const(l) => 
        frameState.ipush(appendConstant(Constant.forInt(l)))
      case x => loadLocal(x.index, Kind.Int)
    }    

    // Will traversal in LMS need to change?
    ir.syms.foreach { tp => tp._2 match {
      case Plus(l, r) => 
        pushOp(l)
        pushOp(r)
        // int plus
      case Const(k) =>
      case Var() =>
    }}

    // function return. No exceptions, no loops
    frameState.cleanupDeletedPhis();
    // Exceptions are not currently covered
    frameState.setRethrowException(false);

    val node = frameState.pop(Kind.Int)
    frameState.clearStack();
    val retNode = new ReturnNode(node)
    currentGraph.add(retNode);
    lastInstr.setNext(retNode)
  }

}  


object LancetTest { 


  val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
  val compiler = HotSpotGraalRuntime.getInstance().getCompiler();
  val backend = HotSpotGraalRuntime.getInstance().getBackend();
  val target = HotSpotGraalRuntime.getInstance().getTarget();
  val cache = HotSpotGraalRuntime.getInstance().getCache();
  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise

  // run default graal compiler on argument 7closure
  // f is here because I do not know how to create a new class :)
  def compile[A:Manifest,B:Manifest](f: A => B)(ir: LancetIR): A => B = {
    assert(manifest[A] == manifest[Int] && manifest[B] == manifest[Int]) // for now ...

    val cls = f.getClass
    val reflectMeth = cls.getDeclaredMethod("apply$mcII$sp", classOf[Int])
    val method = runtime.lookupJavaMethod(reflectMeth)

    val plan = new PhasePlan();    
    plan.addPhase(PhasePosition.HIGH_LEVEL, Util.printGraph("HIGH_LEVEL"))
    plan.addPhase(PhasePosition.MID_LEVEL, Util.printGraph("MID_LEVEL"))
    val result = Util.topScope(method) {

      // Building how the graph should look like
      val sampleGraph = new StructuredGraph(method)
      val lancetBuilder = new TestLancetBuilder(ir, runtime, config, OptimisticOptimizations.ALL)
      val graph = new StructuredGraph(method)
      lancetBuilder.apply(graph)
      Util.printGraph("AFTER_CONSTRUCTION", Node.Verbosity.Debugger)(graph)
      new DeadCodeEliminationPhase().apply(graph)
      Util.printGraph("AFTER_CONSTRUCTION (dead-code)", Node.Verbosity.Debugger)(graph)
      Debug.dump(graph, "Constructed")
      val res = GraalCompiler.compileMethod(runtime, backend, target ,method, graph, cache, plan, OptimisticOptimizations.ALL)
      println("Scope " + com.oracle.graal.debug.internal.DebugScope.getInstance.getQualifiedName)
      Util.printGraph("FINAL")(graph)
      println("===== DONE")

      res
    }    

    val compiledMethod = runtime.addMethod(method, result, null)

    { (x:A) =>
      val y = compiledMethod.executeVarargs(f, x.asInstanceOf[AnyRef])
      y.asInstanceOf[B]
    }  
  }

}     