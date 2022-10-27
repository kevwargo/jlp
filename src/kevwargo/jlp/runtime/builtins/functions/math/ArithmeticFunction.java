package kevwargo.jlp.runtime.builtins.functions.math;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.numbers.LispFloat;
import kevwargo.jlp.objects.scalars.numbers.LispInt;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public abstract class ArithmeticFunction extends LispFunction {

    public static final String ARG_NUMBERS = "numbers";

    protected abstract long addLong(long result, long value);

    protected abstract double addDouble(double result, double value);

    protected abstract Params parseParams(Layer args) throws LispCastException;

    protected ArithmeticFunction(String name, CallArgs args) {
        super(LispFunction.FUNCTION_TYPE, name, args.rest(ARG_NUMBERS));
    }

    protected boolean isDouble(Layer args) throws LispCastException {
        LispList numbers = (LispList) args.get(ARG_NUMBERS);
        if (numbers.size() > 0) {
            LispObject first = numbers.get(0);
            if (first.isInstance(LispFloat.TYPE)) {
                return true;
            }
        }
        return false;
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        Params params = parseParams(args);
        boolean isDouble = isDouble(args);
        long longResult = params.longInitial;
        double doubleResult = params.doubleInitial;
        while (params.numbers.hasNext()) {
            LispObject number = params.numbers.next();
            if (!number.isInstance(LispInt.TYPE) && !number.isInstance(LispFloat.TYPE)) {
                throw new LispException("'%s' is not a number", number);
            }
            if (isDouble) {
                double value;
                if (number.isInstance(LispFloat.TYPE)) {
                    value = ((LispFloat) number.cast(LispFloat.TYPE)).getValue();
                } else {
                    value = (double) ((LispInt) number.cast(LispInt.TYPE)).getValue();
                }
                doubleResult = addDouble(doubleResult, value);
            } else if (number.isInstance(LispFloat.TYPE)) {
                double value = ((LispFloat) number.cast(LispFloat.TYPE)).getValue();
                isDouble = true;
                doubleResult = addDouble((double) longResult, value);
            } else {
                longResult = addLong(longResult, ((LispInt) number.cast(LispInt.TYPE)).getValue());
            }
        }
        if (isDouble) {
            return new LispFloat(doubleResult);
        } else {
            return new LispInt(longResult);
        }
    }

    protected static class Params {

        long longInitial;
        double doubleInitial;
        Iterator<LispObject> numbers;

        Params(long li, double di, Iterator<LispObject> n) {
            longInitial = li;
            doubleInitial = di;
            numbers = n;
        }
    }
}
