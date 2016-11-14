package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class LispBuiltins_Progn extends LispBuiltinMacro {

    public LispBuiltins_Progn() {
        super("progn", new FormalArguments().setRest("body"));
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        LispObject result = LispNil.getInstance();
        Iterator<LispObject> iterator = ((Sexp)arguments.get("body")).iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(namespace);
        }
        return result;
    }
    
}
