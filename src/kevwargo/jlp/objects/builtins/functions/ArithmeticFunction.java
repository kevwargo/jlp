package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispCastException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


abstract public class ArithmeticFunction extends LispFunction {

    abstract protected long addLong(long result, long value);
    abstract protected double addDouble(double result, double value);
    abstract protected Params parseParams(Map<String, LispObject> arguments) throws LispCastException;

    protected ArithmeticFunction(String name, FormalArguments args) {
        super(LispType.FUNCTION, name, args.rest("numbers"));
    }

    protected boolean isDouble(Map<String, LispObject> arguments) throws LispCastException {
        LispList numbers = (LispList)arguments.get("numbers");
        if (numbers.size() > 0) {
            LispObject first = numbers.get(0);
            if (first.isInstance(LispType.FLOAT)) {
                return true;
            }
        }
        return false;
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
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
                    value = ((LispFloat)number.cast(LispType.FLOAT)).getValue();
                } else {
                    value = (double)((LispInt)number.cast(LispType.INT)).getValue();
                }
                doubleResult = addDouble(doubleResult, value);
            } else if (number.isInstance(LispType.FLOAT)) {
                double value = ((LispFloat)number.cast(LispType.FLOAT)).getValue();
                isDouble = true;
                doubleResult = addDouble((double)longResult, value);
            } else {
                longResult = addLong(longResult, ((LispInt)number.cast(LispType.INT)).getValue());
            }
        }
        if (isDouble) {
            return new LispFloat(doubleResult);
        } else {
            return new LispInt(longResult);
        }
    }


    protected static class Params {

        public long longInitial;
        public double doubleInitial;
        public Iterator<LispObject> numbers;

        public Params(long li, double di, Iterator<LispObject> n) {
            longInitial = li;
            doubleInitial = di;
            numbers = n;
        }

    }

}
