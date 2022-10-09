package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

import java.io.PrintStream;

public class LFPrint extends LispFunction {

    public LFPrint() {
        super(LispType.FUNCTION, "print", new CallArgs("obj"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispObject object = args.get("obj");
        PrintStream out = new PrintStream(runtime.getOut());
        out.println(object.toString());
        return object;
    }
}
