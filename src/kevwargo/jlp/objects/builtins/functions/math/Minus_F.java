package kevwargo.jlp.objects.builtins.functions.math;

import java.util.Iterator;
import java.util.Map;

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


public class Minus_F extends ArithmeticFunction {

    public Minus_F() {
        super("-", new FormalArguments());
    }

    protected Params parseParams(Map<String, LispObject> arguments) throws LispCastException {
        LispList numbers = (LispList)arguments.get("numbers").cast(LispType.LIST);
        Iterator<LispObject> it = numbers.iterator();
        if (it.hasNext()) {
            LispObject first = it.next();
            long lv = ((LispInt)first.cast(LispType.INT)).getValue();
            double dv = ((LispFloat)first.cast(LispType.FLOAT)).getValue();
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
