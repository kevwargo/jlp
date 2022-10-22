package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LMDot extends LispFunction {

    public static final String NAME = ".";
    public static final String ARG_OBJ = "obj";
    public static final String ARG_ATTR = "attr";
    public static final String ARG_VALUE = "value";

    public LMDot() {
        super(LispType.MACRO, ".", new CallArgs(ARG_OBJ, ARG_ATTR).opt(ARG_VALUE));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get(ARG_OBJ).eval(runtime);
        String attrName = getAttrName(runtime, args);

        LispObject value = args.get(ARG_VALUE);
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

    private static String getAttrName(LispRuntime runtime, Layer args) throws LispException {
        LispObject attr = args.get(ARG_ATTR);
        if (attr.isInstance(LispType.SYMBOL)) {
            return ((LispSymbol) attr.cast(LispType.SYMBOL)).getName();
        }

        return attr.eval(runtime).toString();
    }
}
