package kevwargo.jlp.objects.builtins.functions.math;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Iterator;
import java.util.Map;

public class LFPlus extends ArithmeticFunction {

    public LFPlus() {
        super("+", new FormalArguments());
    }

    protected Params parseParams(Map<String, LispObject> arguments) throws LispCastException {
        Iterator<LispObject> it =
                ((LispList) arguments.get(ARG_NUMBERS).cast(LispType.LIST)).iterator();
        return new Params(0, 0.0, it);
    }

    protected long addLong(long result, long value) {
        return result + value;
    }

    protected double addDouble(double result, double value) {
        return result + value;
    }
}
