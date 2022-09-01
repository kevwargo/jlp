package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LMIf extends LispFunction {

    public LMIf() {
        super(LispType.MACRO, "if", new FormalArguments().pos("condition").pos("true").rest("false"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject result;
        if (arguments.get("condition").eval(namespace) != LispBool.NIL) {
            result = arguments.get("true").eval(namespace);
        } else {
            result = LispBool.NIL;
            Iterator<LispObject> iterator = ((LispList)arguments.get("false").cast(LispType.LIST)).iterator();
            while (iterator.hasNext()) {
                result = iterator.next().eval(namespace);
            }
        }
        return result;
    }

}