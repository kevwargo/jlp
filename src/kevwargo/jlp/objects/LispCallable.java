package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

public interface LispCallable extends LispObject {

    public CallArgs getCallArgs();

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException;
}
