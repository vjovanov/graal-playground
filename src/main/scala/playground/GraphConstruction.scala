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
import com.oracle.graal.nodes.java.MethodCallTargetNode.InvokeKind;

object GraphBuilder {

  val compiler = HotSpotGraalRuntime.getInstance().getCompiler();
  val backend = HotSpotGraalRuntime.getInstance().getBackend();
  val target = HotSpotGraalRuntime.getInstance().getTarget();
  val cache = HotSpotGraalRuntime.getInstance().getCache();
  val runtime = HotSpotGraalRuntime.getInstance().getRuntime();


  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise

  def arrays(arg: Int) = {
    val f = compile{ arg: Int =>
      val a = new Array[Int](arg)
      a(0) = 0
      val x = a(0)
      val y = a.length
      x
    }(buildArrays)

    f(arg)
  }

  def methodCalls(arg: Int) = {
    val f = compile{ arg: Int =>
      println("blomp")
      1
    }(buildMethod)

    f(arg)
  }

  def nestedLoop(arg: Int) = {
    compile{ arg: Int =>
      var sum = 0
      var i = 0
      var j = 0
      while(i < arg) {
        while(i < j) {
          sum = sum + j
          j = j + 1
        }
        i = i + 1
      }
      sum
    }(buildLoop)
  }

  def loop(arg: Int): Int = {
    val f = compile{ arg: Int =>
      var i = 0
      var sum = 0
      // while ({val c = i < arg; c}) {
      while (i < arg) {
        sum = sum + i
        i = i + 1
      }
      sum
    }(buildLoop)

    f(arg)
  }

  def loopCond(arg: Int): Int = {
    val f = compile{ arg: Int =>
      var i = 0
      var sum = 0
      while ({val c = i < arg; c}) {
        sum = sum + i
        i = i + 1
      }
      sum
    }(buildLoopCond)

    f(arg)
  }

  def cond(arg: Int): Int = {
    val f = compile{x: Int =>
      if (x < 01) (if(x < -11) -12 else 13) else (if (x > 21) 22 else x - 32) }(buildIf)
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

  def buildArrays(graph: StructuredGraph) = {
    val phase = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL) {
      def generateGraalIR() = {
        // Construction
        lastInstr = graph.start()
        clearLocals(frameState)()
        // finish the start block
        lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));

        frameState.cleanupDeletedPhis();
        frameState.setRethrowException(false);

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

 def buildMethod(graph: StructuredGraph) = {
  val phase = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL) {
      def generateGraalIR() = {
        // Construction
        lastInstr = graph.start()
        clearLocals(frameState)()
        // finish the start block
        lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));

        frameState.cleanupDeletedPhis();
        frameState.setRethrowException(false);

        frameState.push(Kind.Object, LancetGraphBuilder.append(ConstantNode.forObject(Predef, runtime, currentGraph)));
        frameState.push(Kind.Object, LancetGraphBuilder.append(ConstantNode.forObject("blomp", runtime, currentGraph)));

        // how to get the target without the consts
        val cls = Predef.getClass
        val reflectMeth = cls.getDeclaredMethod("println", classOf[Any])
        val resolvedMethod = runtime.lookupJavaMethod(reflectMeth)
        val args = frameState.popArguments(resolvedMethod.getSignature().getParameterSlots(true), resolvedMethod.getSignature().getParameterCount(true));
        genInvokeIndirect(InvokeKind.Virtual, resolvedMethod, args)

        clearLocals(frameState)()
        lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(8))

        // block stuff
        val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
        val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState);
        val result = target.fixed;
        val tmpState = frameState.copy()
        clearLocals(tmpState)()
        appendGoto(result)

        frameState = tmpState
        frameState.cleanupDeletedPhis();
        frameState.setRethrowException(false);

        lastInstr = nextFirstInstruction

        frameState.ipush(appendConstant(Constant.INT_1));

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

  def buildLoop(graph: StructuredGraph) = {
    val phase = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL) {
       def generateGraalIR() = {
         lastInstr = graph.start()
         // finish the start block
         clearLocals(frameState)(1)
         lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0))
         frameState.cleanupDeletedPhis()
         frameState.setRethrowException(false);

         // [begin] initialize the local variables
         // var sum = 0
         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
         storeLocal(Kind.Int, 2)

         // var i = 0
         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
         storeLocal(Kind.Int, 3)
         // [end]

         clearLocals(frameState)(1, 2, 3)

         {// initialize the next block
           val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode());
           val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState);
           val result = target.fixed;
           val tmpState = frameState.copy()
           clearLocals(tmpState)(1, 2, 3)
           appendGoto(result)

           frameState = tmpState
           lastInstr = nextFirstInstruction
         }

         frameState.cleanupDeletedPhis();
         frameState.setRethrowException(false);

         // Loop
         // starting the loop block
         val preLoopEnd = currentGraph.add(new EndNode())
         val loopBegin = currentGraph.add(new LoopBeginNode())
         lastInstr.setNext(preLoopEnd)
         // Add the single non-loop predecessor of the loop header.
         loopBegin.addForwardEnd(preLoopEnd)
         lastInstr = loopBegin

         // Create phi functions for all local variables and operand stack slots.
         frameState.insertLoopPhis(loopBegin)
         loopBegin.setStateAfter(frameState.create(4))

         val loopFristInstr = loopBegin
         val loopBlockState = frameState.copy()

         frameState = loopBlockState
         lastInstr = loopBegin
         clearLocals(frameState)(1, 2, 3)
         frameState.cleanupDeletedPhis();

         // [begin] load the condition variables
         loadLocal(3, Kind.Int)
         loadLocal(1, Kind.Int)
         // [end]

         val ((thn, frameStateThen), (els, frameStateElse)) = ifNode(frameState.pop(Kind.Int), Condition.LT, frameState.pop(Kind.Int), true, (loopBegin, loopBlockState));

         // starting the body (else block)
         frameState = frameStateElse // should the loop block state go here?
         lastInstr = els
         clearLocals(frameState)(1, 2, 3)
         frameState.cleanupDeletedPhis();

         // [begin] body
         // sum = sum + i
         loadLocal(2, Kind.Int)
         loadLocal(3, Kind.Int)
         frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
         storeLocal(Kind.Int, 2)

         // i = i + 1
         loadLocal(3, Kind.Int)
         frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
         frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
         storeLocal(Kind.Int, 3)
         // [end] body

         appendGoto({
           val target = new LancetGraphBuilder.Target(currentGraph.add(new LoopEndNode(loopBegin)), frameState)
           val result = target.fixed
           loopBlockState.merge(loopBegin, target.state)
           result
         })

         // after loop (then block)
         frameState = frameStateThen
         lastInstr = thn

         loadLocal(2, Kind.Int)


         // return
         {
          val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode());
          val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState);
          val result = target.fixed;
          val tmpState = frameState.copy()
          clearLocals(tmpState)(1, 2, 3)
          appendGoto(result)

          frameState = tmpState
          lastInstr = nextFirstInstruction
         }

         clearLocals(frameState)(2)
         frameState.cleanupDeletedPhis();
         frameState.setRethrowException(false);

         val node = frameState.pop(Kind.Int)
         frameState.clearStack()
         val retNode = new ReturnNode(node)
         graph.add(retNode)
         lastInstr.setNext(retNode)
       }
    }

    phase.apply(graph)
    graph
  }

  def buildLoopCond(graph: StructuredGraph) = {
        val phase = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL) {
       def generateGraalIR() = {
         lastInstr = graph.start()
         // finish the start block
         clearLocals(frameState)(1)
         lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0))
         frameState.cleanupDeletedPhis()
         frameState.setRethrowException(false);

         // [begin] initialize the local variables
         // var sum = 0
         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
         storeLocal(Kind.Int, 2)

         // var i = 0
         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
         storeLocal(Kind.Int, 3)
         // [end]

         clearLocals(frameState)(1, 2, 3)

         {// initialize the next block
           val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode());
           val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState);
           val result = target.fixed;
           val tmpState = frameState.copy()
           clearLocals(tmpState)(1, 2, 3)
           appendGoto(result)

           frameState = tmpState
           lastInstr = nextFirstInstruction
         }

         frameState.cleanupDeletedPhis();
         frameState.setRethrowException(false);

         // Loop
         // starting the loop block
         val preLoopEnd = currentGraph.add(new EndNode())
         val loopBegin = currentGraph.add(new LoopBeginNode())
         lastInstr.setNext(preLoopEnd)
         // Add the single non-loop predecessor of the loop header.
         loopBegin.addForwardEnd(preLoopEnd)
         lastInstr = loopBegin

         // Create phi functions for all local variables and operand stack slots.
         frameState.insertLoopPhis(loopBegin)
         loopBegin.setStateAfter(frameState.create(4))

         val loopFristInstr = loopBegin
         val loopBlockState = frameState.copy()

         frameState = loopBlockState
         lastInstr = loopBegin
         clearLocals(frameState)(1, 2, 3)
         frameState.cleanupDeletedPhis();

         // [begin] load the condition variables
        {
         loadLocal(3, Kind.Int)
         loadLocal(1, Kind.Int)
         val ((thn, frameStateThen), (els, frameStateElse)) = ifNode(frameState.pop(Kind.Int), Condition.LT,frameState.pop(Kind.Int), true, null);
         // then
         // here we should have a new lastInstr, and the new frameState
         lastInstr = thn
         frameState = frameStateThen
         // clearLocals(frameState)()

         frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
         storeLocal(Kind.Int, 4)
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
         // clearLocals(frameState)()

         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
         storeLocal(Kind.Int, 4)
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
         // clearLocals(exitState, 2)
         lastInstr = mergeNode
         mergeNode.setStateAfter(frameState.create(10))// darn what do we put here?
        }

        loadLocal(4, Kind.Int)
         // [end]

         val ((thn, frameStateThen), (els, frameStateElse)) = ifNode(frameState.pop(Kind.Int), Condition.EQ, appendConstant(Constant.INT_0), true, (loopBegin, loopBlockState));

         // starting the body (else block)
         frameState = frameStateElse // should the loop block state go here?
         lastInstr = els
         clearLocals(frameState)(1, 2, 3)
         frameState.cleanupDeletedPhis();

         // [begin] body
         // sum = sum + i
         loadLocal(2, Kind.Int)
         loadLocal(3, Kind.Int)
         frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
         storeLocal(Kind.Int, 2)

         // i = i + 1
         loadLocal(3, Kind.Int)
         frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
         frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
         storeLocal(Kind.Int, 3)
         // [end] body

         appendGoto({
           val target = new LancetGraphBuilder.Target(currentGraph.add(new LoopEndNode(loopBegin)), frameState)
           val result = target.fixed
           loopBlockState.merge(loopBegin, target.state)
           result
         })

         // after loop (then block)
         frameState = frameStateThen
         lastInstr = thn

         loadLocal(2, Kind.Int)


         // return
         {
          val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode());
          val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState);
          val result = target.fixed;
          val tmpState = frameState.copy()
          clearLocals(tmpState)(1, 2, 3)
          appendGoto(result)

          frameState = tmpState
          lastInstr = nextFirstInstruction
         }

         clearLocals(frameState)(2)
         frameState.cleanupDeletedPhis();
         frameState.setRethrowException(false);

         val node = frameState.pop(Kind.Int)
         frameState.clearStack()
         val retNode = new ReturnNode(node)
         graph.add(retNode)
         lastInstr.setNext(retNode)
       }
    }

    phase.apply(graph)
    graph
  }

  def buildIf(graph: StructuredGraph) = {
    val phase = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL) {
       def generateGraalIR() = {
         clearLocals(frameState)(1)

         lastInstr = graph.start()
         // finish the start block
         lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0))

         frameState.cleanupDeletedPhis()
         frameState.setRethrowException(false)
         frameState.push(Kind.Int, frameState.loadLocal(1)) // ILOAD_1
         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))

         val ((thn, frameStateThen), (els, frameStateElse)) = ifNode(frameState.pop(Kind.Int), Condition.GT,frameState.pop(Kind.Int), true, null);
         // then
         // here we should have a new lastInstr, and the new frameState
         lastInstr = thn
         frameState = frameStateThen
         clearLocals(frameState)()

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
         clearLocals(frameState)()

         frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))

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
         // clearLocals(exitState, 2)
         lastInstr = mergeNode
         mergeNode.setStateAfter(frameState.create(10))// darn what do we put here?

         storeLocal(Kind.Int, 2) // var sum
         frameState.push(Kind.Int, frameState.loadLocal(2)); // ILOAD_1

         val ((thn1, frameStateThen1), (els1, frameStateElse1)) = ifNode(frameState.pop(Kind.Int), Condition.EQ, appendConstant(Constant.INT_0), true, null)
         // then
         // here we should have a new lastInstr, and the new frameState
         lastInstr = thn1
         frameState = frameStateThen1
         clearLocals(frameState)()

         frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))

         // appendGoto(createTarget(probability, currentBlock.successors.get(0), frameState));
         var exitState1 = frameState.copy()
         val target1 = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
         appendGoto({ // inlined create target1
          val result = new LancetGraphBuilder.Target(target1, frameState);
          result.fixed
         })

         // else
         lastInstr = els1
         frameState = frameStateElse1
         clearLocals(frameState)()

         frameState.ipush(ConstantNode.forConstant(Constant.INT_MINUS_1, runtime, graph))

         // The EndNode for the already existing edge.
         val end1 = currentGraph.add(new EndNode());
         // The MergeNode that replaces the placeholder.
         val mergeNode1 = currentGraph.add(new MergeNode());
         appendGoto({ // inlined create target1
            val next = target1.next();

            target1.setNext(end1);
            mergeNode1.addForwardEnd(end1);
            mergeNode1.setNext(next);

            // The EndNode for the newly merged edge.
            val newEnd = currentGraph.add(new EndNode())
            val target2 = new LancetGraphBuilder.Target(newEnd, frameState);
            val result = target2.fixed;
            exitState1.merge(mergeNode1, target2.state);
            mergeNode1.addForwardEnd(newEnd);
            result
         })
         frameState = exitState1
         lastInstr = mergeNode1
         mergeNode1.setStateAfter(frameState.create(10))// darn what do we put here?

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

  def clearLocals(fs: FrameStateBuilder)(locals: Int*) = {
    val removeLocals = new java.util.BitSet()
    locals.foreach(removeLocals.set(_))
    fs.clearNonLiveLocals(removeLocals);
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
    clearLocals(frameState)(1)
    frameState.setRethrowException(false);
    frameState.push(Kind.Int, frameState.loadLocal(1));
    frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
    frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
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

    val result = Util.topScope(method) {

      // Building how the graph should look like
      val sampleGraph = new StructuredGraph(method)
      val graphBuilderPhase = new GraphBuilderPhase(runtime, config, OptimisticOptimizations.ALL)
      val plan = new PhasePlan();
      plan.addPhase(PhasePosition.HIGH_LEVEL, Util.printGraph("HIGH_LEVEL"))
      plan.addPhase(PhasePosition.MID_LEVEL, Util.printGraph("MID_LEVEL"))
      plan.addPhase(PhasePosition.AFTER_PARSING, graphBuilderPhase) // this line is required for method calls
      graphBuilderPhase.debugApply(sampleGraph)
      new DeadCodeEliminationPhase().apply(sampleGraph);
      Util.printGraph("AFTER_PARSING (required)", Node.Verbosity.Debugger)(sampleGraph)
      val graph = build(new StructuredGraph(method))
      // val graph = new StructuredGraph(method);   graphBuilderPhase.apply(graph);
      Util.printGraph("AFTER_PARSING ", Node.Verbosity.Debugger)(graph)
      new DeadCodeEliminationPhase().apply(graph)
      Util.printGraph("AFTER_PARSING (dead-code)", Node.Verbosity.Debugger)(graph)

      Debug.dump(sampleGraph, "Parsed")
      Debug.dump(graph, "Constructed")
      val res = GraalCompiler.compileMethod(runtime, backend, target, method, graph, cache, plan, OptimisticOptimizations.ALL)
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