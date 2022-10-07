package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFAppend extends LispFunction {

    public static final String NAME = "append";
    public static final String ARG_ARGS = "args";

    public LFAppend() {
        super(LispType.FUNCTION, NAME, new FormalArguments().rest(ARG_ARGS));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispList args = (LispList) arguments.get(ARG_ARGS).cast(LispType.LIST);

        LispList result = new LispList();
        for (LispObject arg : args) {
            LispList list = (LispList) arg.cast(LispType.LIST);
            for (LispObject obj : list) {
                result.add(obj);
            }
        }

        return result;
    }
}
