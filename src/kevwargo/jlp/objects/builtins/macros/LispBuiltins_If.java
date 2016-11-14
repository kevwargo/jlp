package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class LispBuiltins_If extends LispBuiltinMacro {

    public LispBuiltins_If() {
        super("if", new FormalArguments().addPositional("condition").addPositional("true").setRest("false"));
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        LispObject result;
        if (arguments.get("condition").eval(namespace).getBooleanValue()) {
            result = arguments.get("true").eval(namespace);
        } else {
            result = LispNil.getInstance();
            Iterator<LispObject> iterator = ((Sexp)arguments.get("false")).iterator();
            while (iterator.hasNext()) {
                result = iterator.next().eval(namespace);
            }
        }
        return result;
    }
    
}
