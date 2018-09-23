package kevwargo.jlp.utils;

import java.util.Iterator;
import java.util.NoSuchElementException;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispObject;


public class ArgumentsIterator {

    private Iterator<LispObject> iterator;
    private LispNamespace evalNamespace;
    private LispObject first;
    private int length;

    public ArgumentsIterator(Iterator<LispObject> iterator, LispNamespace evalNamespace, int length) {
        this.iterator = iterator;
        this.evalNamespace = evalNamespace;
        this.length = length;
    }

    public ArgumentsIterator() {
        this(
                new Iterator<LispObject>() {
                    public boolean hasNext() {
                        return false;
                    }

                    public LispObject next() throws NoSuchElementException {
                        throw new NoSuchElementException();
                    }
                },
                null,
                0
            );
    }

    public boolean hasNext() {
        return first != null || iterator.hasNext();
    }

    public LispObject next() throws LispException {
        LispObject object;
        if (first != null) {
            object = first;
            first = null;
        } else {
            object = iterator.next();
        }
        length--;
        if (evalNamespace != null) {
            return object.eval(evalNamespace);
        } else {
            return object;
        }
    }

    public int getLength() {
        return length;
    }
    
    public void setFirst(LispObject first) {
        if (this.first == null) {
            length++;
        }
        this.first = first;
    }

}
