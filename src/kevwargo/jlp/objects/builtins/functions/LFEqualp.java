package kevwargo.jlp.objects.builtins.functions;

import java.util.Iterator;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispCastException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFEqualp extends LFEq {

    public LFEqualp() {
        super("equalp");
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        if (super.callInternal(namespace, arguments) == LispBool.T) {
            return LispBool.T;
        }
        if (equalp(arguments.get("arg1"), arguments.get("arg2"))) {
            return LispBool.T;
        }
        return LispBool.NIL;
    }

    private boolean equalp(LispObject obj1, LispObject obj2) throws LispException {
        if (obj1.isInstance(LispType.FLOAT) && obj2.isInstance(LispType.FLOAT)) {
            if (((LispFloat)obj1.cast(LispType.FLOAT)).getValue() == ((LispFloat)obj2.cast(LispType.FLOAT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.FLOAT) && obj2.isInstance(LispType.INT)) {
            if (((LispFloat)obj1.cast(LispType.FLOAT)).getValue() == ((LispInt)obj2.cast(LispType.INT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.INT) && obj2.isInstance(LispType.FLOAT)) {
            if (((LispInt)obj1.cast(LispType.INT)).getValue() == ((LispFloat)obj2.cast(LispType.FLOAT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.INT) && obj2.isInstance(LispType.INT)) {
            if (((LispInt)obj1.cast(LispType.INT)).getValue() == ((LispInt)obj2.cast(LispType.INT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.STRING) && obj2.isInstance(LispType.STRING)) {
            if (((LispString)obj1.cast(LispType.STRING)).getValue().equals(((LispString)obj2.cast(LispType.STRING)).getValue())) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.LIST) && obj2.isInstance(LispType.LIST)) {
            Iterator<LispObject> it1 = ((LispList)obj1.cast(LispType.LIST)).iterator();
            Iterator<LispObject> it2 = ((LispList)obj2.cast(LispType.LIST)).iterator();
            while (it1.hasNext() && it2.hasNext()) {
                if (!equalp(it1.next(), it2.next())) {
                    return false;
                }
            }
            if (!it1.hasNext() && !it2.hasNext()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.SYMBOL) && obj2.isInstance(LispType.SYMBOL)) {
            if (((LispSymbol)obj1.cast(LispType.SYMBOL)).getName().equals(((LispSymbol)obj2.cast(LispType.SYMBOL)).getName())) {
                return true;
            }
        }
        return false;
    }

}
