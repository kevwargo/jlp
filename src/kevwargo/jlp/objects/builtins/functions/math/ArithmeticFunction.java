package kevwargo.jlp.objects.builtins.functions.math;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Iterator;
import java.util.Map;

public abstract class ArithmeticFunction extends LispFunction {

    public static final String ARG_NUMBERS = "numbers";

    protected abstract long addLong(long result, long value);

    protected abstract double addDouble(double result, double value);

    protected abstract Params parseParams(Map<String, LispObject> arguments)
            throws LispCastException;

    protected ArithmeticFunction(String name, FormalArguments args) {
        super(LispType.FUNCTION, name, args.rest(ARG_NUMBERS));
    }

    protected boolean isDouble(Map<String, LispObject> arguments) throws LispCastException {
        LispList numbers = (LispList) arguments.get(ARG_NUMBERS);
        if (numbers.size() > 0) {
            LispObject first = numbers.get(0);
            if (first.isInstance(LispType.FLOAT)) {
                return true;
            }
        }
        return false;
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        Params params = parseParams(arguments);
        boolean isDouble = isDouble(arguments);
        long longResult = params.longInitial;
        double doubleResult = params.doubleInitial;
        while (params.numbers.hasNext()) {
            LispObject number = params.numbers.next();
            if (!number.isInstance(LispType.INT) && !number.isInstance(LispType.FLOAT)) {
                throw new LispException("'%s' is not a number", number);
            }
            if (isDouble) {
                double value;
                if (number.isInstance(LispType.FLOAT)) {
                    value = ((LispFloat) number.cast(LispType.FLOAT)).getValue();
                } else {
                    value = (double) ((LispInt) number.cast(LispType.INT)).getValue();
                }
                doubleResult = addDouble(doubleResult, value);
            } else if (number.isInstance(LispType.FLOAT)) {
                double value = ((LispFloat) number.cast(LispType.FLOAT)).getValue();
                isDouble = true;
                doubleResult = addDouble((double) longResult, value);
            } else {
                longResult = addLong(longResult, ((LispInt) number.cast(LispType.INT)).getValue());
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
