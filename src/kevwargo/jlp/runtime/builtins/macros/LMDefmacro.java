package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.functions.LispFunction;

public class LMDefmacro extends LMDefun {

    public LMDefmacro() {
        super("defmacro", false);
    }

    protected LispType getFunctionType() {
        return LispFunction.LISP_MACRO_TYPE;
    }
}
