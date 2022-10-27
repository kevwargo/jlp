package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispCallable;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LMChain extends LispFunction {

    public static final String NAME = "->";
    public static final String ARG_OBJ = "obj";
    public static final String ARG_ATTR = "attr";
    public static final String ARG_CHAIN = "chain";

    public LMChain() {
        super(LispType.MACRO, NAME, new CallArgs(ARG_OBJ, ARG_ATTR).rest(ARG_CHAIN));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get(ARG_OBJ).eval(runtime);
        LispObject attr = getAttr(obj, args.get(ARG_ATTR), runtime);

        for (LispObject form : (LispList) args.get(ARG_CHAIN)) {
            if (form.isInstance(LispType.LIST)) {
                if (!(attr instanceof LispCallable)) {
                    throw new LispException(
                            "Object '%s' is not callable", attr.getType().getName());
                }
                LispCallable callable = (LispCallable) attr;
                attr = ((LispList) form.cast(LispType.LIST)).applyCallable(callable, runtime);
            } else {
                attr = getAttr(attr, form, runtime);
            }
        }

        return attr;
    }

    private static LispObject getAttr(LispObject obj, LispObject attrDesc, LispRuntime runtime)
            throws LispException {
        String attrName;
        if (attrDesc.isInstance(LispType.SYMBOL)) {
            attrName = ((LispSymbol) attrDesc.cast(LispType.SYMBOL)).getName();
        } else {
            attrName = attrDesc.eval(runtime).toString();
        }

        LispObject attr = obj.getAttr(attrName);
        if (attr == null) {
            throw new LispException(
                    "'%s' object has no attribute '%s'", obj.getType().getName(), attrName);
        }

        return attr;
    }
}
