package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LMProgn extends LispFunction {

    public static final String NAME = "progn";
    public static final String ARG_BODY = "body";

    public LMProgn() {
        super(LispFunction.MACRO_TYPE, NAME, new CallArgs().rest(ARG_BODY));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispList body = (LispList) args.get(ARG_BODY);

        LispObject result = LispNil.NIL;
        for (LispObject form : body) {
            result = form.eval(runtime);
        }

        return result;
    }
}
