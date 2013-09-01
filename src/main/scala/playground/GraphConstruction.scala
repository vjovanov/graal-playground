package playground

import java.util.concurrent.Callable
import java.util.BitSet
import org.objectweb.asm._;

import com.oracle.graal.java._
import com.oracle.graal.phases._   // PhasePlan
import com.oracle.graal.phases.common._
import com.oracle.graal.phases.tiers._
import com.oracle.graal.phases.PhasePlan.PhasePosition
import com.oracle.graal.hotspot._
import com.oracle.graal.nodes._
import com.oracle.graal.{java=>J,_}
import com.oracle.graal.debug._         // Debug
import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.api.code._      // Assumptions
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import com.oracle.graal.compiler._      // GraalCompiler
import com.oracle.graal.compiler.phases._      // GraalCompiler
import com.oracle.graal.graph._
import com.oracle.graal.nodes.{java=>J,_}   // StructuredGraph
import com.oracle.graal.nodes.java._        // MethodCallTargetNode
import com.oracle.graal.debug._;
import collection.JavaConversions._
import com.oracle.graal.debug.internal._
import com.oracle.graal.printer._
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes.calc._
import com.oracle.graal.api.runtime._
import com.oracle.graal.nodes.spi._
import com.oracle.graal.nodes.java.MethodCallTargetNode.InvokeKind
import playground.LancetGraphBuilder.BlockPlaceholderNode
import playground.LancetGraphBuilder.Target

trait GraalDSL { self: GraphConstructionUtil =>
 def loadFieldStatic(cls: Class[_], name: String) = {
  val reflectField = cls.getDeclaredField(name)
  val resolvedField = runtime.lookupJavaField(reflectField)
  genGetStatic(resolvedField)
 }
}

object GraphBuilder {

  val compiler = HotSpotGraalRuntime.graalRuntime().getCompilerToVM()
  val backend = HotSpotGraalRuntime.graalRuntime().getBackend()
  val target = HotSpotGraalRuntime.graalRuntime().getTarget()
  val cache = HotSpotGraalRuntime.graalRuntime().getCache()
  val runtime = HotSpotGraalRuntime.graalRuntime().getRuntime()


  val config = GraphBuilderConfiguration.getEagerDefault()

  def methodCalls(arg: Int) = {
    val f = generate[Int, Int](
      "f1",
      List(("scala/Predef$", "println", manifest[AnyRef] :: Nil, manifest[Unit])),
      buildMethod
    )

    val res = try f(arg)catch {case e : Throwable => e.printStackTrace; -1}
    res
  }

 def loadField() = ???

 def buildMethod(graph: StructuredGraph) = {
  def clearLocals(fs: FrameStateBuilder)(locals: Int*) = {
    val removeLocals = new BitSet()
    locals.foreach(removeLocals.set(_))
    fs.clearNonLiveLocals(removeLocals)
  }

  val phase = new GraphConstructionUtil(runtime, config, OptimisticOptimizations.ALL) with GraalDSL {
      def generateGraalIR() = {
        // Construction
        lastInstr = graph.start()
        clearLocals(frameState)(1)
        // finish the start block
        lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));

        frameState.cleanupDeletedPhis();
        frameState.setRethrowException(false);
        clearLocals(frameState)(1);

        {
          loadFieldStatic(Predef.getClass, "MODULE$")//frameState.push(Kind.Object, append(ConstantNode.forObject(Predef, runtime, currentGraph)));
          frameState.push(Kind.Object, append(ConstantNode.forObject("blomp", runtime, currentGraph)));

          // how to get the target without the consts
          val cls = Predef.getClass
          val reflectMeth = cls.getDeclaredMethod("println", classOf[Any])
          val resolvedMethod = runtime.lookupJavaMethod(reflectMeth)
          stream.setBCI(2)
          genInvokeSpecial(resolvedMethod)

          clearLocals(frameState)(1)
          lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(8))

          // block stuff
          val nextFirstInstruction = currentGraph.add(new BlockPlaceholderNode())
          val target = new Target(nextFirstInstruction, frameState)
          val result = target.fixed
          val tmpState = frameState.copy()
          clearLocals(tmpState)(1)
          appendGoto(result)

          frameState = tmpState
          frameState.cleanupDeletedPhis()
          frameState.setRethrowException(false)

          lastInstr = nextFirstInstruction
        }
        loadLocal(1, Kind.Int)

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


  def compile0[A: Manifest, B: Manifest](method: ResolvedJavaMethod)(build: StructuredGraph => StructuredGraph): A => B = {
    val result = Util.topScope(method) {

      // Building how the graph should look like
      val sampleGraph = new StructuredGraph(method)
      val graphBuilderPhase = new GraphBuilderPhase(runtime, config, OptimisticOptimizations.ALL)
      val plan = new PhasePlan()
      // This line is necessary for the methods that are inlined in the generated graph. They start from bytecode and requrie
      // the graph builder phase. For the main method this will be ignored.
      plan.addPhase(PhasePosition.AFTER_PARSING, new GraphBuilderPhase(runtime, config, OptimisticOptimizations.ALL))
      graphBuilderPhase.applyDebug(sampleGraph)

      Util.printGraph("AFTER_PARSING (required)", Node.Verbosity.Debugger)(sampleGraph)
      val graph = build(new StructuredGraph(method))
      // val graph = new StructuredGraph(method);  graphBuilderPhase.apply(graph);
      Util.printGraph("AFTER_PARSING ", Node.Verbosity.Debugger)(graph)
      Debug.dump(sampleGraph, "Parsed (Original)")
      Debug.dump(graph, "Constructed (Manaully Constructed)")
      HighTier.Inline.setValue(false)
      var res = GraalCompiler.compileGraph(
        graph,
        CodeUtil.getCallingConvention(runtime, CallingConvention.Type.JavaCallee, method, false),
        method,
        runtime,
        Graal.getRequiredCapability(classOf[Replacements]),
        backend,
        target,
        cache,
        plan,
        OptimisticOptimizations.ALL,
        new SpeculationLog(),
        Suites.createDefaultSuites(),
        new CompilationResult()
      )
      println("Scope " + com.oracle.graal.debug.internal.DebugScope.getInstance.getQualifiedName)
      Util.printGraph("FINAL", Node.Verbosity.Debugger)(graph)
      println("Compilation result:")
      println(s"Infopoints        : ${res.getInfopoints}")
      println(s"Data References   : ${res.getDataReferences}")
      println(s"Excpetion Handlers: ${res.getExceptionHandlers}")
      println(s"Marks             : ${res.getMarks}")
      println(s"Assumptions       : ${res.getAssumptions}")
      println("===== DONE")

      res
    }

    val compiledMethod = runtime.addMethod(method, result)
    println(s"Installed Code    :\n ${runtime.disassemble(compiledMethod)}")

    { (x:A) =>
      val y = compiledMethod.executeVarargs(().asInstanceOf[AnyRef], x.asInstanceOf[AnyRef])
      y.asInstanceOf[B]
    }
  }

  // this method is used for debugging with real bytecode.
  def compile[A:Manifest,B:Manifest](f: A => B)(build: StructuredGraph => StructuredGraph): A => B = {
    assert(manifest[A] == manifest[Int] && manifest[B] == manifest[Int]) // for now ...

    val cls = f.getClass
    val reflectMeth = cls.getDeclaredMethod("apply$mcII$sp", classOf[Int])
    val method = runtime.lookupJavaMethod(reflectMeth)
    compile0[A, B](method)(build)
  }

  def generate[A: Manifest, B:Manifest](className: String, methods: List[(String, String, List[Manifest[_]], Manifest[_])], build: StructuredGraph => StructuredGraph) = {
    val cls = FunctionTemplate.generateAndLoadFunction(className, List(manifest[Int]), manifest[Int], methods)
    val reflectMeth = cls.getDeclaredMethod("apply", classOf[Int])
    val method = runtime.lookupJavaMethod(reflectMeth)
    compile0[A, B](method)(build)
  }

}