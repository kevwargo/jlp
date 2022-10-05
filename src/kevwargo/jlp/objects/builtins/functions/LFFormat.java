package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFFormat extends LispFunction {

    public LFFormat() {
        super(LispType.FUNCTION, "format", new FormalArguments("fmt").rest("args"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispList argsList = (LispList)arguments.get("args");
        Object args[] = new Object[argsList.size()];
        int pos = 0;
        for (LispObject arg : argsList) {
            args[pos++] = arg.format();
        }
        String fmt = ((LispString)arguments.get("fmt").cast(LispType.STRING)).getValue();
        return new LispString(String.format(fmt, args));
    }

}
