package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

public class LMProgn extends LispFunction {

    public static final String NAME = "progn";
    public static final String ARG_BODY = "body";

    public LMProgn() {
        super(LispType.MACRO, NAME, new CallArgs().rest(ARG_BODY));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispList body = (LispList) args.get(ARG_BODY);

        LispObject result = LispNil.NIL;
        for (LispObject form : body) {
            result = form.eval(runtime);
        }

        return result;
    }
}
