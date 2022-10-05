package kevwargo.jlp.utils;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class ArgumentsIterator {

    private Iterator<LispObject> iterator;
    private LispNamespace evalNamespace;
    private LispObject first;
    private int length;

    public ArgumentsIterator() {
        this(new EmptyIterator(), null, 0);
    }

    public ArgumentsIterator(LispList list) {
        this(list.iterator(), null, list.size());
    }

    public ArgumentsIterator(
            Iterator<LispObject> iterator, LispNamespace evalNamespace, int length) {
        this.iterator = iterator;
        this.evalNamespace = evalNamespace;
        this.length = length;
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

    private static class EmptyIterator implements Iterator<LispObject> {

        public boolean hasNext() {
            return false;
        }

        public LispObject next() throws NoSuchElementException {
            throw new NoSuchElementException();
        }
    }
}
