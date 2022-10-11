package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public class LFConcat extends LispFunction {

    public LFConcat() {
        super(LispType.FUNCTION, "concat", new CallArgs().rest("args"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        StringBuffer sb = new StringBuffer();
        Iterator<LispObject> iterator = ((LispList) args.get("args")).iterator();
        while (iterator.hasNext()) {
            LispObject object = iterator.next().cast(LispType.STRING);
            sb.append(((LispString) object).getValue());
        }
        return new LispString(sb.toString());
    }
}
