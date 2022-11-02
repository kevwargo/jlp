package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispCollection;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispCallable;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LMChain extends LispFunction {

    public static final String NAME = "->";
    public static final String ARG_OBJ = "obj";
    public static final String ARG_ATTR = "attr";
    public static final String ARG_CHAIN = "chain";

    public LMChain() {
        super(LispFunction.MACRO_TYPE, NAME, new CallArgs(ARG_OBJ, ARG_ATTR).rest(ARG_CHAIN));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get(ARG_OBJ).eval(runtime);
        LispObject item = getItem(obj, args.get(ARG_ATTR), runtime);

        for (LispObject form : (LispList) args.get(ARG_CHAIN)) {
            if (form.isInstance(LispList.TYPE)) {
                if (!(item instanceof LispCallable)) {
                    throw new LispException(
                            "Object '%s' is not callable", item.getType().getName());
                }
                LispCallable callable = (LispCallable) item;
                item = ((LispList) form.cast(LispList.TYPE)).applyCallable(callable, runtime);
            } else {
                item = getItem(item, form, runtime);
            }
        }

        return item;
    }

    private LispObject getItem(LispObject obj, LispObject item, LispRuntime runtime)
            throws LispException {
        if (item.isInstance(LispList.TYPE)) {
            item = item.eval(runtime);
        }

        if (item.isInstance(LispSymbol.TYPE)) {
            String attrName = ((LispSymbol) item.cast(LispSymbol.TYPE)).getName();
            LispObject attr = obj.getAttr(attrName);
            if (attr == null) {
                throw new LispException(
                        "'%s' object has no attribute '%s'", obj.getType().getName(), attrName);
            }
            return attr;
        }

        if (!obj.isInstance(LispCollection.TYPE)) {
            throw new LispException("%s is not a collection", obj.repr());
        }

        LispCollection collection = (LispCollection) obj.cast(LispCollection.TYPE);
        return collection.getItem(item);
    }
}
