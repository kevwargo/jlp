package kevwargo.jlp.utils;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispObject;


public class ArgumentsIterator {

    private Iterator<LispObject> iterator;
    private LispNamespace evalNamespace;
    private LispObject first;

    public ArgumentsIterator(Iterator<LispObject> iterator, LispNamespace evalNamespace) {
        this.iterator = iterator;
        this.evalNamespace = evalNamespace;
    }

    public boolean hasNext() {
        return first != null || iterator.hasNext();
    }

    public void setFirst(LispObject first) {
        this.first = first;
    }

    public LispObject next() throws LispException {
        LispObject object;
        if (first != null) {
            object = first;
            first = null;
        } else {
            object = iterator.next();
        }
        if (evalNamespace != null) {
            return object.eval(evalNamespace);
        } else {
            return object;
        }
    }
}
