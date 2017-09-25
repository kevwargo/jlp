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

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(basicNamespace, arguments);
        LispObject mappingsObject = namespace.resolve("mappings").assertType("sexp");
        HashMap<String, LispObject> mappings = new HashMap<String, LispObject>();
        for (LispObject mapping : (Sexp)mappingsObject) {
            if (mapping instanceof LispSymbol) {
                mappings.put(((LispSymbol)mapping).getName(), Sexp.getInstance());
            } else if (mapping instanceof Sexp) {
                Iterator<LispObject> mappingIterator = ((Sexp)mapping).iterator();
                LispObject varObject = mappingIterator.next().assertType("symbol");
                LispObject valObject = Sexp.getInstance();
                if (mappingIterator.hasNext()) {
                    valObject = mappingIterator.next().eval(getVarValNamespace(basicNamespace, mappings));
                }
                if (mappingIterator.hasNext()) {
                    throw new LispException("Mapping must be of structure (var val): " + mapping.toString());
                }
                mappings.put(((LispSymbol)varObject).getName(), valObject);
            } else {
                throw new LispException("Mapping must be a symbol or a sexp: " + mapping.toString());
            }
        }
        LispNamespace localNamespace = basicNamespace.prepend(mappings);
        LispObject result = Sexp.getInstance();
        for (LispObject form : (Sexp)namespace.resolve("body")) {
            result = form.eval(localNamespace);
        }
        return result;
    }
    
}
