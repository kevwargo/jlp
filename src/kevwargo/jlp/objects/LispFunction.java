package kevwargo.jlp.objects;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

public abstract class LispFunction extends LispBaseObject implements LispCallable, LispNamedObject {

    protected String name;
    protected CallArgs callArgs;

    public LispFunction(String name, CallArgs callArgs) {
        this(LispType.FUNCTION, name, callArgs);
    }

    public LispFunction(LispType type, String name, CallArgs callArgs) {
        super(type);
        this.name = name;
        this.callArgs = callArgs;
    }

    public String getName() {
        return name;
    }

    public CallArgs getCallArgs() {
        return callArgs;
    }

    public String repr() {
        return String.format("%s '%s' at 0x%x", getType().getName(), name, System.identityHashCode(this));
    }

    public boolean bool() {
        return true;
    }
}

class FunctionType extends LispType {

    FunctionType(String name) {
        this(name, OBJECT);
    }

    FunctionType(String name, LispType base) {
        super(name, new LispType[] {base}, CallArgs.ignored());
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        throw new LispException("Cannot instantiate '%s'", getName());
    }
}
