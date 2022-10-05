package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class LMLet extends LispFunction {

    public static final String ARG_MAPPINGS = "mappings";
    public static final String ARG_BODY = "body";

    protected boolean usePrevMappings;

    public LMLet() {
        this("let", false);
    }

    protected LMLet(String name, boolean usePrevMappings) {
        super(LispType.MACRO, name, new FormalArguments(ARG_MAPPINGS).rest(ARG_BODY));
        this.usePrevMappings = usePrevMappings;
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        LispList mappings = (LispList) arguments.get(ARG_MAPPINGS).cast(LispType.LIST);
        Map<String, LispObject> bindings = new HashMap<String, LispObject>();

        for (LispObject mapping : mappings) {
            if (mapping.isInstance(LispType.SYMBOL)) {
                bindings.put(((LispSymbol) mapping).getName(), LispBool.NIL);
            } else if (mapping.isInstance(LispType.LIST)) {
                Iterator<LispObject> mappingIterator = ((LispList) mapping).iterator();
                LispSymbol variable = (LispSymbol) mappingIterator.next().cast(LispType.SYMBOL);
                LispObject value = LispBool.NIL;
                if (mappingIterator.hasNext()) {
                    value =
                            mappingIterator
                                    .next()
                                    .eval(
                                            usePrevMappings
                                                    ? namespace.prepend(bindings)
                                                    : namespace);
                }
                if (mappingIterator.hasNext()) {
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

        LispNamespace localNamespace = namespace.prepend(bindings);
        LispObject result = LispBool.NIL;
        Iterator<LispObject> bodyIterator = ((LispList) arguments.get(ARG_BODY)).iterator();
        while (bodyIterator.hasNext()) {
            result = bodyIterator.next().eval(localNamespace);
        }
        return result;
    }
}
