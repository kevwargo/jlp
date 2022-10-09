package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

public class LFFormat extends LispFunction {

    public LFFormat() {
        super(LispType.FUNCTION, "format", new CallArgs("fmt").rest("args"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispList argsList = (LispList) args.get("args");
        Object fmtArgs[] = new Object[argsList.size()];
        int pos = 0;
        for (LispObject arg : argsList) {
            fmtArgs[pos++] = arg.format();
        }
        String fmt = ((LispString) args.get("fmt").cast(LispType.STRING)).getValue();
        return new LispString(String.format(fmt, fmtArgs));
    }
}
