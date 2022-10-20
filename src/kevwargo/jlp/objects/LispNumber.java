package kevwargo.jlp.objects;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public interface LispNumber extends LispObject {

    public double getDoubleValue();
}

class NumberType extends LispType {

    NumberType() {
        super("number", new LispType[] {OBJECT}, new CallArgs());
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        throw new LispException("Cannot instantiate number");
    }
}
