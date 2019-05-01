package kevwargo.jlp.objects.builtins.macros.loop;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class While_M extends LoopBase {

    public While_M() {
        super("while", new FormalArguments().pos("cond").rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject cond = arguments.get("cond");
        LispList body = (LispList)arguments.get("body").cast(LispType.LIST);
        while (cond.eval(namespace) != LispBool.NIL) {
            if (executeBody(namespace, body)) {
                break;
            }
        }
        return LispBool.NIL;
    }

}
