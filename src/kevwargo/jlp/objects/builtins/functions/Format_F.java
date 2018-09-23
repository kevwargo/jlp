package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispString;


public class Format_F extends LispFunction {

    public Format_F() {
        super(LispType.FUNCTION, "format", new FormalArguments("args").pos("fmt"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
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
