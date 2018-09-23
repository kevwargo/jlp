package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;

public class Plus_F extends LispFunction {

    public Plus_F() {
        super(LispType.FUNCTION, "+", new FormalArguments().rest("numbers"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        long longResult = 0;
        double doubleResult = 0.0;
        boolean isDouble = false;
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
                doubleResult += value;
            } else if (number.isInstance(LispType.FLOAT)) {
                isDouble = true;
                doubleResult = (double)longResult;
                doubleResult += ((LispFloat)number.cast(LispType.FLOAT)).getValue();
            } else {
                longResult += ((LispInt)number.cast(LispType.INT)).getValue();
            }
        }
        if (isDouble) {
            return new LispFloat(doubleResult);
        } else {
            return new LispInt(longResult);
        }
    }

}
