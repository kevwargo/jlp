package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Setq_M extends LispFunction {

    public Setq_M() {
        super(LispType.MACRO, "setq", (new FormalArguments()).setRest("defs"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        Iterator<LispObject> iterator = ((LispList)arguments.get("defs").assertType(LispType.LIST)).iterator();
        LispObject result = LispBool.FALSE;
        while (iterator.hasNext()) {
            LispSymbol var = (LispSymbol)iterator.next().assertType(LispType.SYMBOL);
            LispObject val = LispBool.FALSE;
            if (iterator.hasNext()) {
                val = iterator.next().eval(namespace);
            }
            namespace.bind(var.getName(), val);
            result = val;
        }
        return result;
    }
    
}
