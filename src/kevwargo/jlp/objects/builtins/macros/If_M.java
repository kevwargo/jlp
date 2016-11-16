package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class If_M extends LispBuiltinMacro {

    public If_M() {
        super("if", new FormalArguments().addPositional("condition").addPositional("true").setRest("false"));
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        LispObject result;
        if (arguments.get("condition").eval(namespace).getBooleanValue()) {
            result = arguments.get("true").eval(namespace);
        } else {
            result = Sexp.getInstance();
            Iterator<LispObject> iterator = ((Sexp)arguments.get("false")).iterator();
            while (iterator.hasNext()) {
                result = iterator.next().eval(namespace);
            }
        }
        return result;
    }
    
}
