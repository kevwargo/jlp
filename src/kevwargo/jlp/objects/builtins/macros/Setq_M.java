package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;


public class Setq_M extends LispBuiltinMacro {

    public Setq_M() {
        super("setq", (new FormalArguments()).setRest("defs"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(basicNamespace, arguments);
        Iterator<LispObject> iterator = ((Sexp)namespace.resolve("defs").assertType("sexp")).iterator();
        LispObject result = Sexp.getInstance();
        while (iterator.hasNext()) {
            LispSymbol var = (LispSymbol)iterator.next().assertType("symbol");
            LispObject val = Sexp.getInstance();
            if (iterator.hasNext()) {
                val = iterator.next().eval(basicNamespace);
            }
            basicNamespace.bind(var.getName(), val);
            result = val;
        }
        return result;
    }
    
}
