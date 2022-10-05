package kevwargo.jlp.objects;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LispIterator extends LispObject implements Iterator<LispObject> {

    public LispIterator() {
        super(LispType.ITERATOR);
    }

}

class IteratorType extends LispType {

    IteratorType() {
        super("iterator", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (!arguments.hasNext()) {
            return new LispListIterator(new LispList());
        }

        LispObject obj = arguments.next();
        if (obj.isInstance(ITERATOR)) {
            return obj.cast(ITERATOR);
        }
        if (obj.isInstance(JAVA_OBJECT)) {
            return handleJavaObject(((LispJavaObject)obj.cast(JAVA_OBJECT)).getObject());
        }
        if (obj.isInstance(LIST)) {
            return new LispListIterator((LispList)obj.cast(LIST));
        }
        if (obj.isInstance(STRING)) {
            return new LispStringIterator((LispString)obj.cast(STRING));
        }
        throw new LispException("object '%s' is not an iterator", obj);
    }

    @SuppressWarnings("unchecked")
    private LispIterator handleJavaObject(Object obj) throws LispException {
        if (obj.getClass().isArray()){
            return new JavaArrayIterator((Object[])obj);
        }
        if (obj instanceof Iterable) {
            return new JavaIterator(((Iterable<Object>)obj).iterator());
        }
        if (obj instanceof Iterator) {
            return new JavaIterator((Iterator<Object>)obj);
        }
        throw new LispException("java-object '%s' is not iterable", obj);
    }

}

class LispListIterator extends LispIterator {

    private Iterator<LispObject> it;

    public LispListIterator(LispList list) {
        super();
        it = list.iterator();
    }

    public boolean hasNext() {
        return it.hasNext();
    }

    public LispObject next() throws NoSuchElementException {
        return it.next();
    }

}

class LispStringIterator extends LispIterator {

    private String string;
    private int pos;
    private int length;

    public LispStringIterator(LispString string) {
        super();
        this.string = string.getValue();
        length = this.string.length();
        pos = 0;
    }

    public boolean hasNext() {
        return pos < length;
    }

    public LispObject next() throws NoSuchElementException {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        char chr = string.charAt(pos++);
        return new LispInt(chr);
    }

}

class JavaIterator extends LispIterator {

    private Iterator<Object> it;

    public JavaIterator(Iterator<Object> it) {
        super();
        this.it = it;
    }

    public boolean hasNext() {
        return it.hasNext();
    }

    public LispObject next() throws NoSuchElementException {
        return new LispJavaObject(it.next());
    }

}

class JavaArrayIterator extends LispIterator {

    private Object array[];
    private int pos;
    private int length;

    public JavaArrayIterator(Object array[]) {
        super();
        this.array = array;
        length = array.length;
        pos = 0;
    }

    public boolean hasNext() {
        return pos < length;
    }

    public LispObject next() throws NoSuchElementException {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        return new LispJavaObject(array[pos++]);
    }

}
