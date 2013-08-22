package playground;

import static com.oracle.graal.bytecode.Bytecodes.*;
import static java.lang.reflect.Modifier.*;

import java.lang.reflect.*;
import java.util.*;

import com.oracle.graal.api.code.*;
import com.oracle.graal.api.meta.*;
import com.oracle.graal.api.meta.JavaTypeProfile.ProfiledType;
import com.oracle.graal.api.meta.ResolvedJavaType.Representation;
import com.oracle.graal.bytecode.*;
import com.oracle.graal.debug.*;
import com.oracle.graal.graph.*;
import com.oracle.graal.java.BciBlockMapping.Block;
import com.oracle.graal.java.BciBlockMapping.ExceptionDispatchBlock;
import com.oracle.graal.nodes.util.*;
import com.oracle.graal.phases.*;
import com.oracle.graal.phases.util.*;

import com.oracle.graal.nodes.*;
import com.oracle.graal.nodes.calc.*;
import com.oracle.graal.nodes.extended.*;
import com.oracle.graal.nodes.java.*;
import com.oracle.graal.nodes.java.MethodCallTargetNode.InvokeKind;
import com.oracle.graal.nodes.type.*;
import com.oracle.graal.nodes.util.*;
import com.oracle.graal.phases.*;
import com.oracle.graal.phases.util.*;
import com.oracle.graal.java.*;

public abstract class GraphConstructionUtil extends LancetGraphBuilder {

    protected abstract void generateGraalIR();

    public GraphConstructionUtil(MetaAccessProvider runtime, GraphBuilderConfiguration graphBuilderConfig, OptimisticOptimizations optimisticOpts) {
      super(runtime, graphBuilderConfig, optimisticOpts);
    }

    public void apply(StructuredGraph graph) {
        init(graph);
        generateGraalIR();
        finalize();
    }
}