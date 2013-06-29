package playground

import java.util.concurrent.Callable

import com.oracle.graal.{java=>J,_}
import com.oracle.graal.debug._         // Debug
import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.api.code._      // Assumptions
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import com.oracle.graal.compiler._      // GraalCompiler
// import com.oracle.graal.compiler.types._ // PropagateTypeCachePhase
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

object GraphBuilder {

  val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
  val compiler = HotSpotGraalRuntime.getInstance().getCompiler();
  val backend = HotSpotGraalRuntime.getInstance().getBackend();
  val target = HotSpotGraalRuntime.getInstance().getTarget();
  val cache = HotSpotGraalRuntime.getInstance().getCache();


  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise

  def ret(i: Int): Int = {
    val f = compile((x: Int) => x)(buildRet)
    1 // f(i)
  }

  def inc(t: Int): Int = {
    val f = compile((x: Int) => x + 1)(buildInc)
    3 // f(t)
  }

  def buildRet(graph: StructuredGraph) = {
       // Initialization
    var method = graph.method();
    var entryBCI = graph.getEntryBCI();
    var profilingInfo = method.getProfilingInfo();
    var frameState = new FrameStateBuilder(method, graph, config.eagerResolving());


    // Construction
    var lastInstr = graph.start();
    // finish the start block
    lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));


    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);
    frameState.push(Kind.Int, frameState.loadLocal(1)); // ILOAD_1

    val removeLocals = new java.util.BitSet()
    removeLocals.set(1)
    frameState.clearNonLiveLocals(removeLocals);

    // return
    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);
    graph.add(new ReturnNode(frameState.pop(Kind.Int)))

    val rl1 = new java.util.BitSet()
    rl1.set(1)
    frameState.clearNonLiveLocals(rl1);

    graph
  }

  def buildInc(graph: StructuredGraph) = {
    // Initialization
    var method = graph.method();
    println(method)
    var entryBCI = graph.getEntryBCI();
    var profilingInfo = method.getProfilingInfo();
    var frameState = new FrameStateBuilder(method, graph, config.eagerResolving());


    // Construction
    var lastInstr = graph.start()
    // finish the start block
    lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));

    Debug.dump(graph, "After fullUnroll %s");

    val removeLocals = new java.util.BitSet()
    removeLocals.set(0)
    removeLocals.set(1)
    frameState.clearNonLiveLocals(removeLocals);
    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);
    frameState.push(Kind.Int, frameState.loadLocal(1));
    frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))

    val y = frameState.pop(Kind.Int)
    val x = frameState.pop(Kind.Int)
    val v = new IntegerAddNode(Kind.Int, x, y)
    val result = graph.unique(v)
    frameState.push(Kind.Int, result)
    // return
    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);
    graph.add(new ReturnNode(frameState.pop(Kind.Int)))
    graph
  }

  // run default graal compiler on argument 7closure
  // f is here because I do not know how to create a new class :)
  def compile[A:Manifest,B:Manifest](f: A => B)(build: StructuredGraph => StructuredGraph): A => B = {
    assert(manifest[A] == manifest[Int] && manifest[B] == manifest[Int]) // for now ...

    val cls = f.getClass
    val reflectMeth = cls.getDeclaredMethod("apply$mcII$sp", classOf[Int])
    val method = runtime.lookupJavaMethod(reflectMeth)

    val sampleGraph = new StructuredGraph(method)
    val graphBuilderPhase = new GraphBuilderPhase(runtime, config, OptimisticOptimizations.ALL)
    graphBuilderPhase(sampleGraph)
    new DeadCodeEliminationPhase().apply(sampleGraph);
    Util.printGraph("AFTER_PARSING (required)", Node.Verbosity.Debugger)(sampleGraph)
    val graph = build(new StructuredGraph(method))
    Util.printGraph("AFTER_PARSING ", Node.Verbosity.Debugger)(graph)
    new DeadCodeEliminationPhase().apply(graph)
    Util.printGraph("AFTER_PARSING (dead-code)", Node.Verbosity.Debugger)(graph)
    val plan = new PhasePlan();
    plan.addPhase(PhasePosition.AFTER_PARSING, graphBuilderPhase);
    plan.addPhase(PhasePosition.HIGH_LEVEL, Util.printGraph("HIGH_LEVEL"))
    plan.addPhase(PhasePosition.MID_LEVEL, Util.printGraph("MID_LEVEL"))
    val result = Util.topScope {
      Debug.dump(graph, "adsf")
      println("Dump enabled: " + DebugScope.getInstance.isDumpEnabled)
      GraalCompiler.compileMethod(runtime, backend, target ,method, graph, cache, plan, OptimisticOptimizations.ALL)
    }

    Util.printGraph("FINAL")(graph)
    println("===== DONE")

    val compiledMethod = runtime.addMethod(method, result, null)

    { (x:A) =>
      val y = compiledMethod.executeVarargs(f, x.asInstanceOf[AnyRef])
      y.asInstanceOf[B]
    }
  }

}