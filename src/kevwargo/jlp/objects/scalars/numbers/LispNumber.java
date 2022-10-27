package kevwargo.jlp.objects.scalars.numbers;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public interface LispNumber extends LispObject {

    public static final LispType TYPE = new NumberType();

    public double getDoubleValue();
}

class NumberType extends LispType {

    NumberType() {
        super("number", new LispType[] {LispBaseObject.TYPE}, new CallArgs());
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        throw new LispException("Cannot instantiate number");
    }
}
