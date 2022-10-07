package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LMProgn extends LispFunction {

    public static final String NAME = "progn";
    public static final String ARG_BODY = "body";

    public LMProgn() {
        super(LispType.MACRO, NAME, new FormalArguments().rest(ARG_BODY));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispList body = (LispList) arguments.get(ARG_BODY);

        LispObject result = LispNil.NIL;
        for (LispObject form : body) {
            result = form.eval(runtime);
        }

        return result;
    }
}
