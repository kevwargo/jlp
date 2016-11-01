package kevwargo.jlp.object;

import java.util.LinkedList;
import java.util.HashMap;
import java.util.List;
import kevwargo.jlp.LispException;


public class Sexp extends LispObject {

    private LinkedList<LispObject> contents;
    private LispObject first;

    public Sexp() {
        contents = new LinkedList<LispObject>();
    }

    public void add(LispObject object) {
        contents.add(object);
    }

    public LispObject eval(HashMap<String, LispObject> namespace) throws LispException {
        if (contents.isEmpty()) {
            return new LispNil();
        }
        if (first == null) {
            first = contents.remove(0);
        }
        if (!(first instanceof Symbol)) {
            throw new LispException("First object in sexp must be a symbol");
        }
        String name = ((Symbol)first).getName();
        LispObject function = namespace.get(name);
        if (function == null) {
            throw new LispException(String.format("Global function '%s' not found", name));
        }
        if (!(function instanceof LispFunction)) {
            throw new LispException(String.format("Global symbol's '%s' value is not a function", name));
        }
        List<LispObject> args = new LinkedList<LispObject>();
        for (LispObject object : contents) {
            args.add(object);
        }
        return ((LispFunction)function).eval(namespace, args);
    }

    public String toString() {
        if (first == null) {
            first = contents.remove(0);
        }
        StringBuffer sb = new StringBuffer();
        sb.append('(').append(first.toString());
        for (LispObject object : contents) {
            sb.append("\n ");
            sb.append(object.toString());
        }
        return sb.append(')').toString();
    }

}
