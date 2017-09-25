package kevwargo.jlp.objects;

import java.util.HashMap;
import java.util.List;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;


public abstract class LispBuiltinFunction extends LispDataObject {

    private FormalArguments formalArguments;
    protected String name;

    public LispBuiltinFunction(String name, FormalArguments formalArguments) {
        this.name = name;
        this.formalArguments = formalArguments;
    }

    protected LispObject evalArg(LispObject arg, LispNamespace namespace) throws LispException {
        return arg.eval(namespace);
    }

    protected LispNamespace parseArgs(LispNamespace namespace, Iterator<LispObject> actual) throws LispException {
        HashMap<String, LispObject> arguments = new HashMap<String, LispObject>();
        Iterator<String> formal = formalArguments.positional().iterator();
        int argCount = 0;
        while (formal.hasNext() && actual.hasNext()) {
            arguments.put(formal.next(), evalArg(actual.next(), namespace));
            argCount++;
        }
        if (formal.hasNext()) {
            throw new LispException(String.format("Too few arguments for %s: %d", name, argCount));
        }
        if (formalArguments.rest() != null) {
            Sexp rest = Sexp.getInstance();
            while (actual.hasNext()) {
                rest = rest.add(evalArg(actual.next(), namespace));
            }
            arguments.put(formalArguments.rest(), rest);
        } else if (actual.hasNext()) {
            while (actual.hasNext()) {
                actual.next();
                argCount++;
            }
            throw new LispException(String.format("Too many arguments for %s: %d", name, argCount));
        }
        return namespace.prepend(arguments);
    }

    public String type() {
        return "function";
    }

    public String toString() {
        return String.format("built-in function `%s'", name);
    }

}
