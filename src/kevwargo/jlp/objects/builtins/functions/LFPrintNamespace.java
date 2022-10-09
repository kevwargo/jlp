package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.calls.CallArgs;

public class LFPrintNamespace extends LispFunction {

    public LFPrintNamespace() {
        super(LispType.FUNCTION, "print-namespace", new CallArgs());
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        runtime.getNS().dump(runtime.getOut());
        return LispNil.NIL;
    }
}
