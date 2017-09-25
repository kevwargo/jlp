package kevwargo.jlp.objects;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;


public final class Sexp extends LispObject implements Iterable<LispObject> {

    private static Sexp instance;
    private static Sexp specialInstance;

    private List<LispObject> contents;
    private boolean special;

    private Sexp(boolean special) {
        contents = new ArrayList<LispObject>();
        this.special = special;
    }

    public static Sexp getInstance() {
        if (instance == null) {
            instance = new Sexp(false);
        }
        return instance;
    }

    public static Sexp getSpecialInstance() {
        if (specialInstance == null) {
            specialInstance = new Sexp(true);
        }
        return specialInstance;
    }

    public Sexp add(LispObject object) throws LispException {
        Sexp result;
        if (this == instance) {
            result = new Sexp(false);
        } else if (this == specialInstance) {
            result = new Sexp(true);
        } else if (special && contents.size() >= 2) {
            throw new LispException("Special sexp can hold at most two elements");
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
        LispBuiltinFunction function = (LispBuiltinFunction)iterator.next().eval(namespace).assertType("function");
        return function.call(namespace, iterator);
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

    public int size() {
        return contents.size();
    }

    public boolean isSpecial() {
        return special;
    }

    public String type() {
        return "sexp";
    }
}
