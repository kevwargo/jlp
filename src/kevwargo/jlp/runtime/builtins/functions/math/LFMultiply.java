package kevwargo.jlp.objects.builtins.functions.math;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;

import java.util.Iterator;

public class LFMultiply extends ArithmeticFunction {

    public LFMultiply() {
        super("*", new CallArgs());
    }

    protected Params parseParams(Layer args) throws LispCastException {
        Iterator<LispObject> it = ((LispList) args.get(ARG_NUMBERS).cast(LispType.LIST)).iterator();
        return new Params(1, 1.0, it);
    }

    protected long addLong(long result, long value) {
        return result * value;
    }

    protected double addDouble(double result, double value) {
        return result * value;
    }
}