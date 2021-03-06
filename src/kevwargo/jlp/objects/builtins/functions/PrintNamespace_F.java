package kevwargo.jlp.objects.builtins.functions;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class PrintNamespace_F extends LispFunction {

    public PrintNamespace_F() {
        super(LispType.FUNCTION, "print-namespace", new FormalArguments());
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        HashMap<String, LispObject> components[] = namespace.getComponents();
        PrintStream out = namespace.getOutput();
        out.println("NAMESPACE BEGIN");
        for (HashMap<String, LispObject> component : components) {
            out.println("{");
            for (Map.Entry<String, LispObject> entry : component.entrySet()) {
                out.printf("\t%s: %s\n", entry.getKey(), entry.getValue().toString());
            }
            out.println("}");
        }
        out.println("NAMESPACE END");
        return LispBool.NIL;
    }
}
