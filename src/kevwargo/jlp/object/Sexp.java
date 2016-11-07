package kevwargo.jlp.object;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import kevwargo.jlp.LispException;


public class Sexp extends LispObject {

    private List<LispObject> contents;

    public Sexp() {
        contents = new ArrayList<LispObject>();
    }

    public void add(LispObject object) {
        contents.add(object);
    }

    public LispObject eval(HashMap<String, LispObject> namespace) throws LispException {
        ListIterator<LispObject> iterator = contents.listIterator();
        if (!iterator.hasNext()) {
            return new LispNil();
        }
        LispObject first = iterator.next();
        if (!(first instanceof LispSymbol)) {
            throw new LispException("First object in sexp must be a symbol");
        }
        String name = ((LispSymbol)first).getName();
        LispObject function = namespace.get(name);
        if (function == null) {
            throw new LispException(String.format("Global function '%s' not found", name));
        }
        if (!(function instanceof LispFunction)) {
            throw new LispException(String.format("Global symbol's '%s' value is not a function", name));
        }
        List<LispObject> args = new ArrayList<LispObject>();
        while (iterator.hasNext()) {
            args.add(iterator.next());
        }
        ((LispFunction)function).setArguments(namespace, args);
        return function.eval(namespace);
    }

    public String toString() {
        ListIterator<LispObject> iterator = contents.listIterator();
        if (!iterator.hasNext()) {
            return "()";
        }
        StringBuffer sb = new StringBuffer();
        sb.append('(').append(iterator.next().toString());
        while (iterator.hasNext()) {
            sb.append(" ").append(iterator.next());
        }
        return sb.append(')').toString();
    }

}
