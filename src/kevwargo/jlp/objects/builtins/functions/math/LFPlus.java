package kevwargo.jlp.objects.builtins.functions.math;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;

import java.util.Iterator;

public class LFPlus extends ArithmeticFunction {

    public LFPlus() {
        super("+", new CallArgs());
    }

    protected Params parseParams(LispNamespace.Layer args) throws LispCastException {
        Iterator<LispObject> it = ((LispList) args.get(ARG_NUMBERS).cast(LispType.LIST)).iterator();
        return new Params(0, 0.0, it);
    }

    protected long addLong(long result, long value) {
        return result + value;
    }

    protected double addDouble(double result, double value) {
        return result + value;
    }
}
