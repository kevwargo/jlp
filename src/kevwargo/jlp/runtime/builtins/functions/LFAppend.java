package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFAppend extends LispFunction {

    public static final String NAME = "append";
    public static final String ARG_LISTS = "lists";

    public LFAppend() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs().rest(ARG_LISTS));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispList lists = (LispList) args.get(ARG_LISTS).cast(LispList.TYPE);

        LispList result = new LispList();
        for (LispObject list : lists) {
            result.addAll((LispList) list.cast(LispList.TYPE));
        }

        return result;
    }
}
