package kevwargo.jlp.objects.builtins.macros;

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

public class While_M extends LispFunction {

    public While_M() {
        super(LispType.MACRO, "while", new FormalArguments().pos("condition").rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject condition = arguments.get("condition");
        while (condition.eval(namespace) != LispBool.FALSE) {
            for (LispObject form : (LispList)arguments.get("body")) {
                form.eval(namespace);
            }
        }
        return LispBool.FALSE;
    }

}
