package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;


public class PrintNamespace_F extends LispBuiltinFunction {

    public PrintNamespace_F() {
        super("print-namespace", new FormalArguments());
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        HashMap<String, LispObject> components[] = basicNamespace.getComponents();
        System.out.println("NAMESPACE BEGIN");
        for (HashMap<String, LispObject> component : components) {
            System.out.println("{");
            for (Map.Entry<String, LispObject> entry : component.entrySet()) {
                System.out.printf("\t%s: %s\n", entry.getKey(), entry.getValue().toString());
            }
            System.out.println("}");
        }
        System.out.println("NAMESPACE END");
        return Sexp.getInstance();
    }
}
