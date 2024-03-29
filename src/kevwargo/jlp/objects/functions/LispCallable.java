package kevwargo.jlp.objects.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public interface LispCallable extends LispObject {

    public CallArgs getCallArgs();

    public LispObject call(LispRuntime runtime, Layer args) throws LispException;
}
