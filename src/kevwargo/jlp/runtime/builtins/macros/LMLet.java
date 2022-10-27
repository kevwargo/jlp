package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public class LMLet extends LispFunction {

    public static final String ARG_MAPPINGS = "mappings";
    public static final String ARG_BODY = "body";

    protected boolean usePrevMappings;

    public LMLet() {
        this("let", false);
    }

    protected LMLet(String name, boolean usePrevMappings) {
        super(LispFunction.MACRO_TYPE, name, new CallArgs(ARG_MAPPINGS).rest(ARG_BODY));
        this.usePrevMappings = usePrevMappings;
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispList mappings = (LispList) args.get(ARG_MAPPINGS).cast(LispList.TYPE);
        Layer bindings = new Layer();

        for (LispObject mapping : mappings) {
            if (mapping.isInstance(LispSymbol.TYPE)) {
                bindings.put(((LispSymbol) mapping).getName(), LispNil.NIL);
            } else if (mapping.isInstance(LispList.TYPE)) {
                Iterator<LispObject> it = ((LispList) mapping).iterator();
                LispSymbol variable = (LispSymbol) it.next().cast(LispSymbol.TYPE);
                LispObject value = LispNil.NIL;
                if (it.hasNext()) {
                    value = it.next().eval(usePrevMappings ? runtime.with(bindings) : runtime);
                }
                if (it.hasNext()) {
                    throw new LispException(
                            "Mapping must be of structure (var val), not '%s'", mapping.toString());
                }
                bindings.put(variable.getName(), value);
            } else {
                throw new LispException(
                        "Mapping must be a symbol or a list, not '%s'",
                        mapping.getType().toString());
            }
        }

        LispRuntime localRuntime = runtime.with(bindings);
        LispObject result = LispNil.NIL;
        Iterator<LispObject> bodyIterator = ((LispList) args.get(ARG_BODY)).iterator();
        while (bodyIterator.hasNext()) {
            result = bodyIterator.next().eval(localRuntime);
        }
        return result;
    }
}
