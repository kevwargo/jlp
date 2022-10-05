package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFNth extends LispFunction {

    public LFNth() {
        super(LispType.FUNCTION, "nth", new FormalArguments("n", "list"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        long idx = ((LispInt) arguments.get("n").cast(LispType.INT)).getValue();
        LispList list = (LispList) arguments.get("list").cast(LispType.LIST);

        return list.get((int) idx);
    }
}
