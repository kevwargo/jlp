package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFFormat extends LispFunction {

    public LFFormat() {
        super(LispType.FUNCTION, "format", new FormalArguments("fmt").rest("args"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispList argsList = (LispList) arguments.get("args");
        Object fmtArgs[] = new Object[argsList.size()];
        int pos = 0;
        for (LispObject arg : argsList) {
            fmtArgs[pos++] = arg.format();
        }
        String fmt = ((LispString) arguments.get("fmt").cast(LispType.STRING)).getValue();
        return new LispString(String.format(fmt, fmtArgs));
    }
}
