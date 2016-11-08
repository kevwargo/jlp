package kevwargo.jlp.objects;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;


public class Sexp extends LispObject {

    private List<LispObject> contents;

    public Sexp() {
        contents = new ArrayList<LispObject>();
    }

    public void add(LispObject object) {
        contents.add(object);
    }

    public LispObject set(int index, LispObject object) {
        return contents.set(index, object);
    }

    public int size() {
        return contents.size();
    }

    public ListIterator<LispObject> listIterator() {
        return contents.listIterator();
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        ListIterator<LispObject> iterator = contents.listIterator();
        if (!iterator.hasNext()) {
            return LispNil.getInstance();
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
        List<LispObject> args = new ArrayList<LispObject>();
        while (iterator.hasNext()) {
            args.add(iterator.next());
        }
        ((LispBuiltinFunction)function).setArguments(namespace, args);
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
