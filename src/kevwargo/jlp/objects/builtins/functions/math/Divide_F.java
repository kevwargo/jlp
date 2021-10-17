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

public class Divide_F extends ArithmeticFunction {

    public Divide_F() {
        super("/", new FormalArguments().pos("first"));
    }

    protected Params parseParams(Map<String, LispObject> arguments) throws LispCastException {
        LispObject first = arguments.get("first");
        long lv = ((LispInt)first.cast(LispType.INT)).getValue();
        double dv = ((LispFloat)first.cast(LispType.FLOAT)).getValue();
        Iterator<LispObject> it = ((LispList)arguments.get("numbers").cast(LispType.LIST)).iterator();
        return new Params(lv, dv, it);
    }

    protected boolean isDouble(Map<String, LispObject> arguments) throws LispCastException {
        LispObject first = arguments.get("first");
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
