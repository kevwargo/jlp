package kevwargo.jlp.objects.iter;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.wrappers.LispJavaObject;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public abstract class LispIterator extends LispBaseObject
        implements LispIterable, Iterator<LispObject> {

    public static final LispType TYPE = new IteratorType();

    public LispIterator() {
        super(LispIterator.TYPE);
    }

    public Iterator<LispObject> iterator() {
        return this;
    }

    public static boolean isIterable(LispObject obj) {
        if (obj instanceof LispIterable) {
            return true;
        }

        try {
            Object object = ((LispJavaObject) obj.cast(LispJavaObject.TYPE)).getObject();
            return (object instanceof Iterator)
                    || (object instanceof Iterable)
                    || object.getClass().isArray();
        } catch (LispCastException exc) {
            return false;
        }
    }

    public static LispIterator create(LispObject obj) throws LispException {
        if (obj == null) {
            return new LispIteratorImpl(new LispList().iterator());
        }

        if (obj instanceof LispIterable) {
            return new LispIteratorImpl(((LispIterable) obj).iterator());
        }

        if (obj.isInstance(LispJavaObject.TYPE)) {
            Object object = ((LispJavaObject) obj.cast(LispJavaObject.TYPE)).getObject();

            if (object instanceof Iterator) {
                return new JavaIterator((Iterator<?>) object);
            }
            if (object instanceof Iterable) {
                return new JavaIterable((Iterable<?>) object);
            }
            if (object.getClass().isArray()) {
                return new JavaArray((Object[]) object);
            }
        }

        throw new LispCastException("object '%s' is not iterable", obj.getType().getName());
    }
}

class IteratorType extends LispType {

    IteratorType() {
        super("iterator", new LispType[] {LispBaseObject.TYPE});
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return LispIterator.create(args.get(ARG_OBJ));
    }
}

class LispIteratorImpl extends LispIterator {

    private Iterator<LispObject> it;

    LispIteratorImpl(Iterator<LispObject> it) {
        this.it = it;
    }

    public boolean hasNext() {
        return it.hasNext();
    }

    public LispObject next() {
        return it.next();
    }
}

class JavaIterator extends LispIterator {

    private Iterator<?> it;

    JavaIterator(Iterator<?> it) {
        this.it = it;
    }

    public boolean hasNext() {
        return it.hasNext();
    }

    public LispObject next() {
        return LispBaseObject.wrap(it.next());
    }
}

class JavaIterable extends JavaIterator {

    JavaIterable(Iterable<?> it) {
        super(it.iterator());
    }
}

class JavaArray extends LispIterator {

    private Object array[];
    private int pos;

    JavaArray(Object array[]) {
        this.array = array;
    }

    public boolean hasNext() {
        return pos < array.length;
    }

    public LispObject next() {
        return LispBaseObject.wrap(array[pos++]);
    }
}
