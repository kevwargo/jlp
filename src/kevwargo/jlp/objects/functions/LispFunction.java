package kevwargo.jlp.objects.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispNamedObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public abstract class LispFunction extends LispBaseObject implements LispCallable, LispNamedObject {

    public static final LispType FUNCTION_TYPE = new FunctionType("builtin-function");
    public static final LispType MACRO_TYPE = new FunctionType("builtin-macro", FUNCTION_TYPE);
    public static final LispType LISP_FUNCTION_TYPE = new FunctionType("function", FUNCTION_TYPE);
    public static final LispType LISP_MACRO_TYPE = new FunctionType("macro", MACRO_TYPE);

    protected String name;
    protected CallArgs callArgs;

    public LispFunction(String name, CallArgs callArgs) {
        this(LispFunction.FUNCTION_TYPE, name, callArgs);
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
        return String.format(
                "%s '%s' at 0x%x", getType().getName(), name, System.identityHashCode(this));
    }

    public boolean bool() {
        return true;
    }
}

class FunctionType extends LispType {

    FunctionType(String name) {
        this(name, LispBaseObject.TYPE);
    }

    FunctionType(String name, LispType base) {
        super(name, new LispType[] {base}, CallArgs.ignored());
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        throw new LispException("Cannot instantiate '%s'", getName());
    }
}
