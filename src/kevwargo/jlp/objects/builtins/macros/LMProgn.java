package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import java.util.Map;

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


public class LMProgn extends LispFunction {

    public LMProgn() {
        super(LispType.MACRO, "progn", new FormalArguments().rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject result = LispBool.NIL;
        Iterator<LispObject> iterator = ((LispList)arguments.get("body")).iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(namespace);
        }
        return result;
    }

}
