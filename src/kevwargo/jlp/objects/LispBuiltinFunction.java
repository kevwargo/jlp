package kevwargo.jlp.objects;

import java.util.HashMap;
import java.util.List;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;


public abstract class LispBuiltinFunction extends LispObject {

    private FormalArguments formalArguments;
    protected String name;
    protected HashMap<String, LispObject> arguments;

    public LispBuiltinFunction(String name, FormalArguments formalArguments) {
        this.name = name;
        this.formalArguments = formalArguments;
    }

    protected LispObject evalArg(LispObject arg, LispNamespace namespace) throws LispException {
        return arg.eval(namespace);
    }

    public void setArguments(LispNamespace namespace, Iterator<LispObject> actual) throws LispException {
        arguments = new HashMap<String, LispObject>();
        Iterator<String> formal = formalArguments.positional().iterator();
        int argCount = 0;
        while (formal.hasNext() && actual.hasNext()) {
            arguments.put(formal.next(), evalArg(actual.next(), namespace));
            argCount++;
        }
        if (formal.hasNext()) {
            throw new LispException(String.format("Too few arguments to %s: %d", name, argCount));
        }
        if (formalArguments.rest() != null) {
            Sexp rest = new Sexp();
            while (actual.hasNext()) {
                rest.add(evalArg(actual.next(), namespace));
            }
            arguments.put(formalArguments.rest(), rest);
        } else if (actual.hasNext()) {
            while (actual.hasNext()) {
                actual.next();
                argCount++;
            }
            throw new LispException(String.format("Too many arguments to %s: %d", name, argCount));
        }
    }

}
