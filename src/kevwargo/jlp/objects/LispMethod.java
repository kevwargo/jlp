package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.calls.CallArgs;

public class LispMethod extends LispBaseObject implements LispCallable, LispNamedObject {

    private LispObject instance;
    private LispFunction function;
    private String instanceArgName;
    private CallArgs callArgs;

    public LispMethod(LispObject instance, LispFunction function) {
        super(LispType.METHOD);
        this.instance = instance;
        this.function = function;

        callArgs = function.getCallArgs().clone();
        instanceArgName = callArgs.pop();
    }

    public CallArgs getCallArgs() {
        return callArgs;
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
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
