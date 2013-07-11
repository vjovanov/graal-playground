;package playground

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

  def loop(arg: Int) {
    val f = compile{arg: Int =>
      var sum = 0
      var i = 0
      while (i < arg) {
        sum += i
        i += 1
      }
      sum
    }(buildIf)
    (0 until arg).sum    
  }

  def cond(arg: Int): Int = {
    val f = compile{x: Int => if(x > 0) 1 else -1}(buildIf)
    f(arg)
  }
  def ret(i: Int): Int = {
    val f = compile((x: Int) => x)(buildRet)
    f(i)
  }

  def inc(t: Int): Int = {
    val f = compile((x: Int) => x + 1)(buildInc)
    f(t)
  }


  def buildIf(graph: StructuredGraph) = {
    val phase = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL) {
       def generateGraalIR() = {
         val removeLocals = new java.util.BitSet()
         removeLocals.set(1)
         frameState.clearNonLiveLocals(removeLocals);

         lastInstr = graph.start()
         // finish the start block
         lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0))

         frameState.cleanupDeletedPhis()
         frameState.setRethrowException(false);
         frameState.push(Kind.Int, frameState.loadLocal(1)); // ILOAD_1
         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))

         ifNode(frameState.pop(Kind.Int), Condition.LE,frameState.pop(Kind.Int), true);
         val frameStateThen = frameState.copy()
         val frameStateElse = frameState.copy()
         // then
         // here we should have a new lastInstr, and the new frameState
         lastInstr = thn
         frameState = frameStateThen

         frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
         // appendGoto(createTarget(probability, currentBlock.successors.get(0), frameState));
         var exitState = frameState.copy()
         val target = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
         appendGoto({ // inlined create target
          val result = new LancetGraphBuilder.Target(target, frameState);
          result.fixed
         })

         // else
         lastInstr = els
         frameState = frameStateElse
         frameState.ipush(ConstantNode.forConstant(Constant.INT_MINUS_1, runtime, graph))
         // appendGoto(createTarget(block.successors.get(0), frameState));

         // The EndNode for the already existing edge.
         val end = currentGraph.add(new EndNode());
         // The MergeNode that replaces the placeholder.
         val mergeNode = currentGraph.add(new MergeNode());
         appendGoto({ // inlined create target
            val next = target.next();

            target.setNext(end);
            mergeNode.addForwardEnd(end);
            mergeNode.setNext(next);

            // The EndNode for the newly merged edge.
            val newEnd = currentGraph.add(new EndNode())
            val target2 = new LancetGraphBuilder.Target(newEnd, frameState);
            val result = target2.fixed;
            exitState.merge(mergeNode, target2.state);
            mergeNode.addForwardEnd(newEnd);
            result
         })
         frameState = exitState
         lastInstr = mergeNode
         mergeNode.setStateAfter(frameState.create(10))// darn what do we put here?

         // return
         frameState.cleanupDeletedPhis();
         frameState.setRethrowException(false);

         val node = frameState.pop(Kind.Int)
         frameState.clearStack();
         val retNode = new ReturnNode(node)
         graph.add(retNode)
         lastInstr.setNext(retNode)
       }
    }

    phase.apply(graph)
    graph
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

    val node = frameState.pop(Kind.Int)
    frameState.clearStack();
    val retNode = new ReturnNode(node)
    graph.add(retNode)
    lastInstr.setNext(retNode)

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

    frameState.cleanupDeletedPhis();
    val bs = new java.util.BitSet()
    bs.set(1)
    frameState.clearNonLiveLocals(bs)// TODO
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

    val node = frameState.pop(Kind.Int)
    frameState.clearStack();
    val retNode = new ReturnNode(node)
    graph.add(retNode)
    lastInstr.setNext(retNode)

    graph
  }

  // run default graal compiler on argument 7closure
  // f is here because I do not know how to create a new class :)
  def compile[A:Manifest,B:Manifest](f: A => B)(build: StructuredGraph => StructuredGraph): A => B = {
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
      val graphBuilderPhase = new GraphBuilderPhase(runtime, config, OptimisticOptimizations.ALL)
      graphBuilderPhase.debugApply(sampleGraph)
      // new DeadCodeEliminationPhase().apply(sampleGraph);
      Util.printGraph("AFTER_PARSING (required)", Node.Verbosity.Debugger)(sampleGraph)
      val graph = build(new StructuredGraph(method))
      Util.printGraph("AFTER_PARSING ", Node.Verbosity.Debugger)(graph)
      // new DeadCodeEliminationPhase().apply(graph)
      Util.printGraph("AFTER_PARSING (dead-code)", Node.Verbosity.Debugger)(graph)

      Debug.dump(sampleGraph, "Parsed")
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