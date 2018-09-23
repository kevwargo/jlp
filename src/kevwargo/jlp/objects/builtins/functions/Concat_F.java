package kevwargo.jlp.objects.builtins.functions;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;

public class Concat_F extends LispFunction {

    public Concat_F() {
        super(LispType.FUNCTION, "concat", new FormalArguments(new ArrayList<String>(), "args"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        StringBuffer sb = new StringBuffer();
        Iterator<LispObject> iterator = ((LispList)arguments.get("args")).iterator();
        while (iterator.hasNext()) {
            LispObject object = iterator.next().cast(LispType.STRING);
            sb.append(((LispString)object).getValue());
        }
        return new LispString(sb.toString());
    }
    
}
