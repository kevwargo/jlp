package kevwargo.jlp.objects.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispNamedObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LispMethod extends LispBaseObject implements LispCallable, LispNamedObject {

    public static final LispType TYPE = new FunctionType("method", LispFunction.FUNCTION_TYPE);

    private LispObject instance;
    private LispFunction function;
    private String instanceArgName;
    private CallArgs callArgs;

    public LispMethod(LispObject instance, LispFunction function) {
        super(LispMethod.TYPE);
        this.instance = instance;
        this.function = function;

        callArgs = function.getCallArgs().clone();
        instanceArgName = callArgs.pop();
    }

    public CallArgs getCallArgs() {
        return callArgs;
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        args.put(instanceArgName, instance);
        return function.call(runtime, args);
    }

    public String getName() {
        return function.getName();
    }

    public String repr() {
        return String.format("<bound method %s of %s>", function.getName(), instance.repr());
    }

    public boolean bool() {
        return true;
    }
}
