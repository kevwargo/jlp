package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Let_M extends LispFunction {

    public Let_M() {
        this("let");
    }

    public Let_M(String name) {
        super(LispType.MACRO, name, new FormalArguments().pos("mappings").rest("body"));
    }

    protected LispNamespace getVarValNamespace(LispNamespace namespace, HashMap<String, LispObject> prevDefs) {
        return namespace;
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject mappingsObject = arguments.get("mappings").assertType(LispType.LIST);
        HashMap<String, LispObject> mappings = new HashMap<String, LispObject>();
        Iterator<LispObject> iterator = ((LispList)mappingsObject).iterator();
        while (iterator.hasNext()) {
            LispObject mapping = iterator.next();
            if (mapping.isInstance(LispType.SYMBOL)) {
                mappings.put(((LispSymbol)mapping).getName(), LispBool.FALSE);
            } else if (mapping.isInstance(LispType.LIST)) {
                Iterator<LispObject> mappingIterator = ((LispList)mapping).iterator();
                LispObject varObject = mappingIterator.next().assertType(LispType.SYMBOL);
                LispObject valObject = LispBool.FALSE;
                if (mappingIterator.hasNext()) {
                    valObject = mappingIterator.next().eval(getVarValNamespace(namespace, mappings));
                }
                if (mappingIterator.hasNext()) {
                    throw new LispException("Mapping must be of structure (var val): " + mapping.toString());
                }
                mappings.put(((LispSymbol)varObject).getName(), valObject);
            } else {
                throw new LispException("Mapping must be a symbol or a list: " + mapping.toString());
            }
        }
        LispNamespace localNamespace = namespace.prepend(mappings);
        LispObject result = LispBool.FALSE;
        Iterator<LispObject> bodyIterator = ((LispList)arguments.get("body")).iterator();
        while (bodyIterator.hasNext()) {
            result = bodyIterator.next().eval(localNamespace);
        }
        return result;
    }
    
}
