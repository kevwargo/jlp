package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFPrintNamespace extends LispFunction {

    public LFPrintNamespace() {
        super(LispFunction.FUNCTION_TYPE, "print-namespace", new CallArgs());
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        runtime.getNS().dump(runtime.getOut());
        return LispNil.NIL;
    }
}
