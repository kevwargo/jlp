package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFApply extends LispFunction {

    public LFApply() {
        super(LispType.FUNCTION, "apply", new FormalArguments("func", "args"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject func = arguments.get("func");
        LispList args = (LispList)arguments.get("args").cast(LispType.LIST);
        LispNamespace argsNamespace = func.isInstance(LispType.MACRO) ? null : namespace;
        return func.call(namespace, new ArgumentsIterator(args.iterator(), argsNamespace, args.size()));
    }

}
