package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LMDot extends LispFunction {

    public static final String NAME = ".";
    public static final String ARG_OBJ = "obj";
    public static final String ARG_ATTR = "attr";
    public static final String ARG_VALUE = "value";

    public LMDot() {
        super(LispType.MACRO, ".", new FormalArguments(ARG_OBJ, ARG_ATTR).opt(ARG_VALUE));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispObject obj = arguments.get(ARG_OBJ).eval(runtime);
        String attrName = getAttrName(runtime, arguments);
        LispObject value = arguments.get(ARG_VALUE);

        if (value != null) {
            obj.setAttr(attrName, value);
            return value;
        }

        LispObject attr = obj.getAttr(attrName);
        if (attr == null) {
            throw new LispException(
                    "'%s' object has no attribute '%s'", obj.getType().getName(), attrName);
        }
        return attr;
    }

    private static String getAttrName(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispObject attr = arguments.get(ARG_ATTR);
        if (attr.isInstance(LispType.SYMBOL)) {
            return ((LispSymbol) attr.cast(LispType.SYMBOL)).getName();
        }

        return attr.eval(runtime).toString();
    }
}
