package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.calls.CallArgs;

public class LMIf extends LispFunction {

    public static final String NAME = "if";
    public static final String ARG_COND = "cond";
    public static final String ARG_THEN = "then";
    public static final String ARG_ELSE = "else";

    public LMIf() {
        super(LispType.MACRO, NAME, new CallArgs(ARG_COND, ARG_THEN).rest(ARG_ELSE));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
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
