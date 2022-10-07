package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Iterator;
import java.util.Map;

public class LMIf extends LispFunction {

    public LMIf() {
        super(LispType.MACRO, "if", new FormalArguments("condition", "true").rest("false"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispObject result = LispBool.FALSE;

        if (arguments.get("condition").eval(runtime).bool()) {
            result = arguments.get("true").eval(runtime);
        } else {
            Iterator<LispObject> iterator =
                    ((LispList) arguments.get("false").cast(LispType.LIST)).iterator();
            while (iterator.hasNext()) {
                result = iterator.next().eval(runtime);
            }
        }

        return result;
    }
}
