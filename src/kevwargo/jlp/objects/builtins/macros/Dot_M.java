package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Dot_M extends LispFunction {

    public Dot_M() {
        super(LispType.MACRO, ".", new FormalArguments().pos("obj").pos("attr"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject obj = arguments.get("obj").eval(namespace);
        LispObject attrObj = arguments.get("attr");
        String attrName;
        if (attrObj.isInstance(LispType.SYMBOL)) {
            attrName = ((LispSymbol)attrObj).getName();
        } else {
            attrName = attrObj.eval(namespace).toString();
        }
        LispObject attr = obj.getAttr(attrName, true);
        if (attr == null) {
            throw new LispException(String.format("'%s' object has no attribute '%s'", obj.getType().getName(), attrName));
        }
        return attr;
    }
    
}
