package kevwargo.jlp.objects;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;


public class Sexp extends LispObject {

    private static Sexp instance;

    private List<LispObject> contents;

    private Sexp() {
        contents = new ArrayList<LispObject>();
    }

    public static Sexp getInstance() {
        if (instance == null) {
            instance = new Sexp();
        }
        return instance;
    }

    public Sexp add(LispObject object) {
        Sexp result;
        if (this == instance) {
            result = new Sexp();
        } else {
            result = this;
        }
        result.contents.add(object);
        return result;
    }

    public Iterator<LispObject> iterator() {
        return contents.iterator();
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        Iterator<LispObject> iterator = contents.iterator();
        if (!iterator.hasNext()) {
            return this;
        }
        LispObject first = iterator.next();
        if (!(first instanceof LispSymbol)) {
            throw new LispException("First object in sexp must be a symbol");
        }
        String name = ((LispSymbol)first).getName();
        LispObject function = namespace.resolve(name);
        if (!(function instanceof LispBuiltinFunction)) {
            throw new LispException(String.format("Symbol's value is not a function: %s", name));
        }
        ((LispBuiltinFunction)function).setArguments(namespace, iterator);
        return function.eval(namespace);
    }

    public String toString() {
        Iterator<LispObject> iterator = contents.iterator();
        if (!iterator.hasNext()) {
            return "nil";
        }
        StringBuffer sb = new StringBuffer();
        sb.append('(').append(iterator.next().toString());
        while (iterator.hasNext()) {
            sb.append(" ").append(iterator.next());
        }
        return sb.append(')').toString();
    }

    public boolean getBooleanValue() {
        return !contents.isEmpty();
    }
}
