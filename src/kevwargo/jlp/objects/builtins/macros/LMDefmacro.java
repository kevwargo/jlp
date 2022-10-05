package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LMDefmacro extends LMDefun {

    public LMDefmacro() {
        super("defmacro");
    }

    protected LispFunction createFunction(
            String name, FormalArguments formalArguments, LispList body, LispNamespace namespace) {
        return new Macro(LispType.LISP_MACRO, name, formalArguments, body, namespace);
    }

    protected static class Macro extends LMDefun.Function {

        public Macro(
                LispType type,
                String name,
                FormalArguments formalArguments,
                LispList body,
                LispNamespace namespace) {
            super(type, name, formalArguments, body, namespace);
        }

        protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
                throws LispException {
            return super.callInternal(runtime, arguments).eval(runtime);
        }
    }
}
