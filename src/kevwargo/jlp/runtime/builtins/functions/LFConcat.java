package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispString;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public class LFConcat extends LispFunction {

    public LFConcat() {
        super(LispFunction.FUNCTION_TYPE, "concat", new CallArgs().rest("args"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        StringBuffer sb = new StringBuffer();
        Iterator<LispObject> iterator = ((LispList) args.get("args")).iterator();
        while (iterator.hasNext()) {
            LispObject object = iterator.next().cast(LispString.TYPE);
            sb.append(((LispString) object).getValue());
        }
        return new LispString(sb.toString());
    }
}
