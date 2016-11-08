package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.ListIterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.*;


public class LispBuiltins_Let extends LispBuiltinMacro {

    public LispBuiltins_Let() {
        this("let");
    }

    public LispBuiltins_Let(String name) {
        super(name, new String[] {"mappings"}, true);
    }

    protected LispNamespace getVarValNamespace(LispNamespace namespace, HashMap<String, LispObject> prevDefs) {
        return namespace;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        LispObject mappingsObject = arguments.get("mappings");
        if (!(mappingsObject instanceof Sexp)) {
            throw new LispException("Wrong argument type: mappings must be a sexp: " + mappingsObject.toString());
        }
        HashMap<String, LispObject> mappings = new HashMap<String, LispObject>();
        ListIterator<LispObject> iterator = ((Sexp)mappingsObject).listIterator();
        while (iterator.hasNext()) {
            LispObject mapping = iterator.next();
            if (mapping instanceof LispSymbol) {
                mappings.put(((LispSymbol)mapping).getName(), LispNil.getInstance());
            } else if (mapping instanceof Sexp) {
                ListIterator<LispObject> mappingIterator = ((Sexp)mapping).listIterator();
                LispObject varObject = mappingIterator.next();
                if (!(varObject instanceof LispSymbol)) {
                    throw new LispException("Wrong argument type: variable must be a symbol: " + varObject.toString());
                }
                LispObject valObject;
                if (mappingIterator.hasNext()) {
                    valObject = mappingIterator.next().eval(getVarValNamespace(namespace, mappings));
                } else {
                    valObject = LispNil.getInstance();
                }
                mappings.put(((LispSymbol)varObject).getName(), valObject);
                if (mappingIterator.hasNext()) {
                    throw new LispException("Mapping must be of structure (var val): " + mapping.toString());
                }
            } else {
                throw new LispException("Mapping must be a symbol or a sexp: " + mapping.toString());
            }
        }
        LispObject result = LispNil.getInstance();
        LispNamespace localNamespace = namespace.prepend(mappings);
        for (LispObject expr : rest) {
            result = expr.eval(localNamespace);
        }
        return result;
    }
    
}
