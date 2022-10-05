package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFPrintNamespace extends LispFunction {

    public LFPrintNamespace() {
        super(LispType.FUNCTION, "print-namespace", new FormalArguments());
    }

    protected LispObject callInternal(LispNamespace ns, Map<String, LispObject> arguments)
            throws LispException {
        ns.dump();
        return LispBool.NIL;
    }
}
