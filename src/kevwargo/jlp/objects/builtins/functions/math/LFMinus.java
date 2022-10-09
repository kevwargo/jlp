package kevwargo.jlp.objects.builtins.functions.math;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.calls.CallArgs;

import java.util.Iterator;

public class LFMinus extends ArithmeticFunction {

    public LFMinus() {
        super("-", new CallArgs());
    }

    protected Params parseParams(LispNamespace.Layer args) throws LispCastException {
        LispList numbers = (LispList) args.get(ARG_NUMBERS).cast(LispType.LIST);
        Iterator<LispObject> it = numbers.iterator();
        if (it.hasNext()) {
            LispObject first = it.next();
            long lv = ((LispInt) first.cast(LispType.INT)).getValue();
            double dv = ((LispFloat) first.cast(LispType.FLOAT)).getValue();
            return new Params(lv, dv, it);
        }
        return new Params(0, 0.0, it);
    }

    protected long addLong(long result, long value) {
        return result - value;
    }

    protected double addDouble(double result, double value) {
        return result - value;
    }
}
