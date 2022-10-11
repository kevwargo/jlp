package kevwargo.jlp.objects.builtins.functions.math;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;

import java.util.Iterator;

public class LFDivide extends ArithmeticFunction {

    public static final String ARG_FIRST = "first";

    public LFDivide() {
        super("/", new CallArgs(ARG_FIRST));
    }

    protected Params parseParams(LispNamespace.Layer args) throws LispCastException {
        LispObject first = args.get(ARG_FIRST);
        long lv = ((LispInt) first.cast(LispType.INT)).getValue();
        double dv = ((LispFloat) first.cast(LispType.FLOAT)).getValue();
        Iterator<LispObject> it = ((LispList) args.get(ARG_NUMBERS).cast(LispType.LIST)).iterator();
        return new Params(lv, dv, it);
    }

    protected boolean isDouble(LispNamespace.Layer args) throws LispCastException {
        LispObject first = args.get(ARG_FIRST);
        if (first.isInstance(LispType.FLOAT)) {
            return true;
        } else if (first.isInstance(LispType.INT)) {
            return false;
        }
        throw new LispCastException("'%s' is not a number", first.toString());
    }

    protected long addLong(long result, long value) {
        return result / value;
    }

    protected double addDouble(double result, double value) {
        return result / value;
    }
}
