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

import java.util.Iterator;
import java.util.Map;

public class LFConcat extends LispFunction {

    public LFConcat() {
        super(LispType.FUNCTION, "concat", new FormalArguments().rest("args"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        StringBuffer sb = new StringBuffer();
        Iterator<LispObject> iterator = ((LispList) arguments.get("args")).iterator();
        while (iterator.hasNext()) {
            LispObject object = iterator.next().cast(LispType.STRING);
            sb.append(((LispString) object).getValue());
        }
        return new LispString(sb.toString());
    }
}
