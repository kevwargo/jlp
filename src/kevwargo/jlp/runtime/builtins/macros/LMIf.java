package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LMIf extends LispFunction {

    public static final String NAME = "if";
    public static final String ARG_COND = "cond";
    public static final String ARG_THEN = "then";
    public static final String ARG_ELSE = "else";

    public LMIf() {
        super(LispFunction.MACRO_TYPE, NAME, new CallArgs(ARG_COND, ARG_THEN).rest(ARG_ELSE));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        if (args.get(ARG_COND).eval(runtime).bool()) {
            return args.get(ARG_THEN).eval(runtime);
        }

        LispObject result = LispBool.FALSE;
        for (LispObject form : (LispList) args.get(ARG_ELSE)) {
            result = form.eval(runtime);
        }

        return result;
    }
}
