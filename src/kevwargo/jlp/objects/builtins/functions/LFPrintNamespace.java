package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.io.PrintStream;
import java.util.Map;

public class LFPrintNamespace extends LispFunction {

    public LFPrintNamespace() {
        super(LispType.FUNCTION, "print-namespace", new FormalArguments());
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        runtime.getNS().dump(new PrintStream(runtime.getOut()));
        return LispBool.NIL;
    }
}
