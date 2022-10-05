package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LMLambda extends LMDefun {

    public LMLambda() {
        super("lambda", new FormalArguments("arglist").rest("body"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispList arglist = (LispList) arguments.get("arglist").cast(LispType.LIST);
        LispList body = (LispList) arguments.get("body").cast(LispType.LIST);
        LispFunction function =
                createFunction("<lambda>", buildArgs(arglist), body, runtime.getNS());
        return function;
    }
}
