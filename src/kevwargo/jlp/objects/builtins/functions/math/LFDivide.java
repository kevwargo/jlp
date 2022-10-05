package kevwargo.jlp.objects.builtins.functions.math;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Iterator;
import java.util.Map;

public class LFDivide extends ArithmeticFunction {

    public static final String ARG_FIRST = "first";

    public LFDivide() {
        super("/", new FormalArguments(ARG_FIRST));
    }

    protected Params parseParams(Map<String, LispObject> arguments) throws LispCastException {
        LispObject first = arguments.get(ARG_FIRST);
        long lv = ((LispInt) first.cast(LispType.INT)).getValue();
        double dv = ((LispFloat) first.cast(LispType.FLOAT)).getValue();
        Iterator<LispObject> it =
                ((LispList) arguments.get(ARG_NUMBERS).cast(LispType.LIST)).iterator();
        return new Params(lv, dv, it);
    }

    protected boolean isDouble(Map<String, LispObject> arguments) throws LispCastException {
        LispObject first = arguments.get(ARG_FIRST);
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
