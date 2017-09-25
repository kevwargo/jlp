package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.*;
import kevwargo.jlp.utils.FormalArguments;


public class Let_M extends LispBuiltinMacro {

    public Let_M() {
        this("let");
    }

    public Let_M(String name) {
        super(name, new FormalArguments().addPositional("mappings").setRest("body"));
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
        Iterator<LispObject> iterator = ((Sexp)mappingsObject).iterator();
        while (iterator.hasNext()) {
            LispObject mapping = iterator.next();
            if (mapping instanceof LispSymbol) {
                mappings.put(((LispSymbol)mapping).getName(), Sexp.getInstance());
            } else if (mapping instanceof Sexp) {
                Iterator<LispObject> mappingIterator = ((Sexp)mapping).iterator();
                LispObject varObject = mappingIterator.next();
                if (!(varObject instanceof LispSymbol)) {
                    throw new LispException("Wrong argument type: variable must be a symbol: " + varObject.toString());
                }
                LispObject valObject;
                if (mappingIterator.hasNext()) {
                    valObject = mappingIterator.next().eval(getVarValNamespace(namespace, mappings));
                } else {
                    valObject = Sexp.getInstance();
                }
                mappings.put(((LispSymbol)varObject).getName(), valObject);
                if (mappingIterator.hasNext()) {
                    throw new LispException("Mapping must be of structure (var val): " + mapping.toString());
                }
            } else {
                throw new LispException("Mapping must be a symbol or a sexp: " + mapping.toString());
            }
        }
        LispObject result = Sexp.getInstance();
        LispNamespace localNamespace = namespace.prepend(mappings);
        iterator = ((Sexp)arguments.get("body")).iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(localNamespace);
        }
        return result;
    }
    
}
