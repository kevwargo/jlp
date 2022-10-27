package kevwargo.jlp.runtime.builtins.functions.math;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.scalars.numbers.LispFloat;
import kevwargo.jlp.objects.scalars.numbers.LispInt;
import kevwargo.jlp.runtime.LispNamespace.Layer;

import java.util.Iterator;

public class LFMinus extends ArithmeticFunction {

    public LFMinus() {
        super("-", new CallArgs());
    }

    protected Params parseParams(Layer args) throws LispCastException {
        LispList numbers = (LispList) args.get(ARG_NUMBERS).cast(LispList.TYPE);
        Iterator<LispObject> it = numbers.iterator();
        if (it.hasNext()) {
            LispObject first = it.next();
            long lv = ((LispInt) first.cast(LispInt.TYPE)).getValue();
            double dv = ((LispFloat) first.cast(LispFloat.TYPE)).getValue();
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
