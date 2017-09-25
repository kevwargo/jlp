package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class If_M extends LispBuiltinMacro {

    public If_M() {
        super("if", new FormalArguments().addPositional("condition").addPositional("true").setRest("false"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(basicNamespace, arguments);
        LispObject result;
        if (namespace.resolve("condition").eval(basicNamespace).getBooleanValue()) {
            result = namespace.resolve("true").eval(basicNamespace);
        } else {
            result = Sexp.getInstance();
            for (LispObject form : (Sexp)namespace.resolve("false")) {
                result = form.eval(basicNamespace);
            }
        }
        return result;
    }
    
}
