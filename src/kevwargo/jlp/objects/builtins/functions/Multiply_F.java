package kevwargo.jlp.objects.builtins.functions;

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


public class Multiply_F extends ArithmeticFunction {

    public Multiply_F() {
        super("*", new FormalArguments());
    }

    protected Params parseParams(Map<String, LispObject> arguments) throws LispCastException {
        Iterator<LispObject> it = ((LispList)arguments.get("numbers").cast(LispType.LIST)).iterator();
        return new Params(1, 1.0, it);
    }

    protected long addLong(long result, long value) {
        return result * value;
    }

    protected double addDouble(double result, double value) {
        return result * value;
    }

}
