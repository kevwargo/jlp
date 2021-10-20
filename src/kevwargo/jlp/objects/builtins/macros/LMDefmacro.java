package kevwargo.jlp.objects.builtins.macros;

import java.util.Map;
import java.util.Iterator;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LMDefmacro extends LMDefun {

    public LMDefmacro() {
        super("defmacro");
    }

    protected LispFunction createFunction(String name, FormalArguments formalArguments, LispList body, LispNamespace namespace) {
        return new Macro(LispType.LISP_MACRO, name, formalArguments, body, namespace);
    }


    protected static class Macro extends LMDefun.Function {

        Macro(LispType type, String name, FormalArguments formalArguments, LispList body, LispNamespace namespace) {
            super(type, name, formalArguments, body, namespace);
        }

        protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
            return super.callInternal(namespace, arguments).eval(namespace);
        }

    }

}
