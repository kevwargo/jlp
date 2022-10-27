package kevwargo.jlp.objects.builtins.functions.compare;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispNumber;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;

import java.util.Iterator;

public abstract class CompareFunction extends LispFunction {

    public static final String ARG_OBJ1 = "obj1";
    public static final String ARG_OBJ2 = "obj2";

    public CompareFunction(String name) {
        super(LispType.FUNCTION, name, new CallArgs(ARG_OBJ1, ARG_OBJ2));
    }

    protected int compare(LispObject obj1, LispObject obj2) throws LispException {
        Integer result;

        if ((result = compareNumbers(obj1, obj2)) != null) {
            return result.intValue();
        }
        if ((result = compareStrings(obj1, obj2)) != null) {
            return result.intValue();
        }
        if ((result = compareLists(obj1, obj2)) != null) {
            return result.intValue();
        }

        throw new LispException(
                "Comparison between '%s' and '%s' is not supported",
                obj1.getType().getName(), obj2.getType().getName());
    }

    private Integer compareNumbers(LispObject obj1, LispObject obj2) throws LispException {
        try {
            double number1 = ((LispNumber) obj1.cast(LispType.NUMBER)).getDoubleValue();
            checkType(obj2, LispType.NUMBER);
            double number2 = ((LispNumber) obj2.cast(LispType.NUMBER)).getDoubleValue();

            if (number1 == number2) {
                return 0;
            }
            return number1 < number2 ? -1 : 1;
        } catch (LispCastException e) {
            return null;
        }
    }

    private Integer compareStrings(LispObject obj1, LispObject obj2) throws LispException {
        try {
            String str1 = ((LispString) obj1.cast(LispType.STRING)).getValue();
            checkType(obj2, LispType.STRING);
            return str1.compareTo(((LispString) obj2.cast(LispType.STRING)).getValue());
        } catch (LispCastException e) {
            return null;
        }
    }

    private Integer compareLists(LispObject obj1, LispObject obj2) throws LispException {
        try {
            Iterator<LispObject> it1 = ((LispList) obj1.cast(LispType.LIST)).iterator();
            checkType(obj2, LispType.LIST);
            Iterator<LispObject> it2 = ((LispList) obj2.cast(LispType.LIST)).iterator();

            int result = 0;
            while (it1.hasNext() && it2.hasNext()) {
                if ((result = compare(it1.next(), it2.next())) != 0) {
                    return result;
                }
            }

            return it2.hasNext() ? -1 : 1;
        } catch (LispCastException e) {
            return null;
        }
    }

    private void checkType(LispObject obj, LispType type) throws LispException {
        if (obj.isInstance(type)) {
            return;
        }

        throw new LispException(
                "Comparison between '%s' and '%s' is not supported",
                type.getName(), obj.getType().getName());
    }
}
