package kevwargo.jlp.objects.builtins.functions;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class List_F extends LispFunction {

    public List_F() {
        super(LispType.FUNCTION, "list", new FormalArguments(new ArrayList<String>(), "args"));
    }

    public LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        Iterator<LispObject> iterator = ((LispList)arguments.get("args")).iterator();
        if (! iterator.hasNext()) {
            return LispBool.FALSE;
        }
        LispList result = new LispList();
        while (iterator.hasNext()) {
            result = result.add(iterator.next());
        }
        return result;
    }
    
}
