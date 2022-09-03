package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


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

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject mappingsObject = arguments.get(ARG_MAPPINGS).cast(LispType.LIST);
        Map<String, LispObject> mappings = new HashMap<String, LispObject>();
        Iterator<LispObject> iterator = ((LispList)mappingsObject).iterator();
        while (iterator.hasNext()) {
            LispObject mapping = iterator.next();
            if (mapping.isInstance(LispType.SYMBOL)) {
                mappings.put(((LispSymbol)mapping).getName(), LispBool.NIL);
            } else if (mapping.isInstance(LispType.LIST)) {
                Iterator<LispObject> mappingIterator = ((LispList)mapping).iterator();
                LispObject varObject = mappingIterator.next().cast(LispType.SYMBOL);
                LispObject valObject = LispBool.NIL;
                if (mappingIterator.hasNext()) {
                    valObject = mappingIterator.next().eval(usePrevMappings ? namespace.prepend(mappings) : namespace);
                }
                if (mappingIterator.hasNext()) {
                    throw new LispException("Mapping must be of structure (var val), not '%s'", mapping.toString());
                }
                mappings.put(((LispSymbol)varObject).getName(), valObject);
            } else {
                throw new LispException("Mapping must be a symbol or a list, not '%s'", mapping.toString());
            }
        }
        LispNamespace localNamespace = namespace.prepend(mappings);
        LispObject result = LispBool.NIL;
        Iterator<LispObject> bodyIterator = ((LispList)arguments.get(ARG_BODY)).iterator();
        while (bodyIterator.hasNext()) {
            result = bodyIterator.next().eval(localNamespace);
        }
        return result;
    }

}
