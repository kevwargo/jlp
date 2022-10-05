package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFApply extends LispFunction {

    public LFApply() {
        super(LispType.FUNCTION, "apply", new FormalArguments("func", "args"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispObject func = arguments.get("func");
        LispList args = (LispList) arguments.get("args").cast(LispType.LIST);
        LispRuntime argsRuntime = func.isInstance(LispType.MACRO) ? null : runtime;
        return func.call(runtime, new ArgumentsIterator(args.iterator(), argsRuntime, args.size()));
    }
}
