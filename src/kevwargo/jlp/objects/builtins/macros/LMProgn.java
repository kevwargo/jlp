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

public class LMProgn extends LispFunction {

    public LMProgn() {
        super(LispType.MACRO, "progn", new FormalArguments().rest("body"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispObject result = LispBool.NIL;
        Iterator<LispObject> iterator = ((LispList) arguments.get("body")).iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(runtime);
        }
        return result;
    }
}
