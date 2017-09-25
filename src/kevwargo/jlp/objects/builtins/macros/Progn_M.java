package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class Progn_M extends LispBuiltinMacro {

    public Progn_M() {
        super("progn", new FormalArguments().setRest("body"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispObject result = Sexp.getInstance();
        for (LispObject form : (Sexp)parseArgs(basicNamespace, arguments).resolve("body")) {
            result = form.eval(basicNamespace);
        }
        return result;
    }
    
}
