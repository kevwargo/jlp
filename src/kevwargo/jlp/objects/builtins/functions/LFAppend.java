package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.calls.CallArgs;

public class LFAppend extends LispFunction {

    public static final String NAME = "append";
    public static final String ARG_LISTS = "lists";

    public LFAppend() {
        super(LispType.FUNCTION, NAME, new CallArgs().rest(ARG_LISTS));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispList lists = (LispList) args.get(ARG_LISTS).cast(LispType.LIST);

        LispList result = new LispList();
        for (LispObject list : lists) {
            result.addAll((LispList) list.cast(LispType.LIST));
        }

        return result;
    }
}
