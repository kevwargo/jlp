package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Progn_M extends LispFunction {

    public Progn_M() {
        super(LispType.MACRO, "progn", new FormalArguments().rest("body"));
    }

    public LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject result = LispBool.FALSE;
        Iterator<LispObject> iterator = ((LispList)arguments.get("body")).iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(namespace);
        }
        return result;
    }
    
}
