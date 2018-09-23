package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;
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

    protected ArithmeticFunction(String name, FormalArguments args) {
        super(LispType.FUNCTION, name, args.rest("numbers"));
    }

    protected long getLongInitial(HashMap<String, LispObject> arguments) throws LispCastException {
        return 0;
    }

    protected double getDoubleInitial(HashMap<String, LispObject> arguments) throws LispCastException {
        return 0.0;
    }

    protected boolean isDouble(HashMap<String, LispObject> arguments) throws LispCastException {
        return false;
    }
    
    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        long longResult = getLongInitial(arguments);
        double doubleResult = getDoubleInitial(arguments);
        boolean isDouble = isDouble(arguments);
        LispList numbers = (LispList)arguments.get("numbers").cast(LispType.LIST);
        for (LispObject number : numbers) {
            if (!number.isInstance(LispType.INT) && !number.isInstance(LispType.FLOAT)) {
                throw new LispException(String.format("'%s' is not a number", number));
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

}
