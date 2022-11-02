package kevwargo.jlp.runtime.builtins.functions.compare;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.collections.LispString;
import kevwargo.jlp.objects.scalars.numbers.LispNumber;
import kevwargo.jlp.objects.wrappers.LispJavaObject;

import java.util.Iterator;

public abstract class CheckEqualityFunction extends CompareFunction {

    CheckEqualityFunction(String name) {
        super(name);
    }

    protected boolean equalp(LispObject obj1, LispObject obj2, boolean strict) {
        if (obj1 == obj2) {
            return true;
        }

        if (strict) {
            return equalObjects(obj1, obj2, true);
        }

        if (equalNumbers(obj1, obj2)) {
            return true;
        }
        if (equalStrings(obj1, obj2)) {
            return true;
        }
        if (equalLists(obj1, obj2)) {
            return true;
        }

        return equalObjects(obj1, obj2, false);
    }

    private boolean equalLists(LispObject obj1, LispObject obj2) {
        try {
            Iterator<LispObject> it1 = ((LispList) obj1.cast(LispList.TYPE)).iterator();
            Iterator<LispObject> it2 = ((LispList) obj2.cast(LispList.TYPE)).iterator();
            while (it1.hasNext() && it2.hasNext()) {
                if (!equalp(it1.next(), it2.next(), false)) {
                    return false;
                }
            }
            if (!it1.hasNext() && !it2.hasNext()) {
                return true;
            }
        } catch (LispCastException e) {
        }

        return false;
    }

    private boolean equalStrings(LispObject obj1, LispObject obj2) {
        try {
            return ((LispString) obj1.cast(LispString.TYPE))
                    .getValue()
                    .equals(((LispString) obj2.cast(LispString.TYPE)).getValue());
        } catch (LispCastException e) {
            return false;
        }
    }

    private boolean equalNumbers(LispObject obj1, LispObject obj2) {
        try {
            return ((LispNumber) obj1.cast(LispNumber.TYPE)).getDoubleValue()
                    == ((LispNumber) obj2.cast(LispNumber.TYPE)).getDoubleValue();
        } catch (LispCastException e) {
            return false;
        }
    }

    private boolean equalObjects(LispObject obj1, LispObject obj2, boolean strict) {
        try {
            Object o1 = ((LispJavaObject) obj1.cast(LispJavaObject.TYPE)).getObject();
            Object o2 = ((LispJavaObject) obj2.cast(LispJavaObject.TYPE)).getObject();

            if (strict) {
                return o1 == o2;
            }

            return o1.equals(o2);
        } catch (LispCastException e) {
            return false;
        }
    }
}
