package kevwargo.jlp.runtime.builtins.functions.compare;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.collections.LispString;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.numbers.LispNumber;

import java.util.Iterator;

public abstract class CompareFunction extends LispFunction {

    public static final String ARG_OBJ1 = "obj1";
    public static final String ARG_OBJ2 = "obj2";

    public CompareFunction(String name) {
        super(LispFunction.FUNCTION_TYPE, name, new CallArgs(ARG_OBJ1, ARG_OBJ2));
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
            double number1 = ((LispNumber) obj1.cast(LispNumber.TYPE)).getDoubleValue();
            checkType(obj2, LispNumber.TYPE);
            double number2 = ((LispNumber) obj2.cast(LispNumber.TYPE)).getDoubleValue();

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
            String str1 = ((LispString) obj1.cast(LispString.TYPE)).getValue();
            checkType(obj2, LispString.TYPE);
            return str1.compareTo(((LispString) obj2.cast(LispString.TYPE)).getValue());
        } catch (LispCastException e) {
            return null;
        }
    }

    private Integer compareLists(LispObject obj1, LispObject obj2) throws LispException {
        try {
            Iterator<LispObject> it1 = ((LispList) obj1.cast(LispList.TYPE)).iterator();
            checkType(obj2, LispList.TYPE);
            Iterator<LispObject> it2 = ((LispList) obj2.cast(LispList.TYPE)).iterator();

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
