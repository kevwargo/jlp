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

public class Plus_F extends ArithmeticFunction {

    public Plus_F() {
        super("+", new FormalArguments());
    }

    protected long getLongInitial(HashMap<String, LispObject> arguments) throws LispCastException {
        return 0;
    }

    protected double getDoubleInitial(HashMap<String, LispObject> arguments) throws LispCastException {
        return 0.0;
    }

    protected long addLong(long result, long value) {
        return result + value;
    }

    protected double addDouble(double result, double value) {
        return result + value;
    }

}
