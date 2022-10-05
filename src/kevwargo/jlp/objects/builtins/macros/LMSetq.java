package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;

import java.util.Iterator;
import java.util.Map;

public class LMSetq extends LispFunction {

    public static final String NAME = "setq";
    public static final String ARG_DEFS = "defs";

    public LMSetq() {
        super(LispType.MACRO, NAME, (new FormalArguments()).rest(ARG_DEFS));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        Iterator<LispObject> iterator =
                ((LispList) arguments.get(ARG_DEFS).cast(LispType.LIST)).iterator();
        LispObject result = LispBool.NIL;
        while (iterator.hasNext()) {
            LispSymbol var = (LispSymbol) iterator.next().cast(LispType.SYMBOL);

            if (!iterator.hasNext()) {
                throw new LispException(String.format("Odd number of arguments to '%s'", NAME));
            }

            LispObject val = iterator.next().eval(namespace);
            namespace.bind(var.getName(), val);
            result = val;
        }
        return result;
    }
}
