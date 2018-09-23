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

public class Divide_F extends ArithmeticFunction {

    public Divide_F() {
        super("/", new FormalArguments().pos("first"));
    }

    protected long getLongInitial(HashMap<String, LispObject> arguments) throws LispCastException {
        return ((LispInt)arguments.get("first").cast(LispType.INT)).getValue();
    }

    protected double getDoubleInitial(HashMap<String, LispObject> arguments) throws LispCastException {
        return ((LispFloat)arguments.get("first").cast(LispType.FLOAT)).getValue();
    }

    protected boolean isDouble(HashMap<String, LispObject> arguments) throws LispCastException {
        LispObject first = arguments.get("first");
        if (first.isInstance(LispType.INT)) {
            return false;
        } else if (first.isInstance(LispType.FLOAT)) {
            return true;
        }
        throw new LispCastException(String.format("'%s' is not a number", first.toString()));
    }

    protected long addLong(long result, long value) {
        return result / value;
    }

    protected double addDouble(double result, double value) {
        return result / value;
    }

}
