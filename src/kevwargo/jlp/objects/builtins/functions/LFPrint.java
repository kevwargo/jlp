package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFPrint extends LispFunction {

    public LFPrint() {
        super(LispType.FUNCTION, "print", new FormalArguments().pos("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject object = arguments.get("obj");
        namespace.getOutput().println(object.toString());
        return object;
    }

}
