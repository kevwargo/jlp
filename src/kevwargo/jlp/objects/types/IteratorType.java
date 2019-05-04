package kevwargo.jlp.objects.types;

import java.util.Iterator;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.iterator.JavaArrayIterator;
import kevwargo.jlp.objects.iterator.JavaIterator;
import kevwargo.jlp.objects.iterator.LispIterator;
import kevwargo.jlp.objects.iterator.LispListIterator;
import kevwargo.jlp.objects.iterator.LispStringIterator;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class IteratorType extends LispType {

    IteratorType() {
        super("iterator");
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (!arguments.hasNext()) {
            return new LispListIterator(new LispList());
        }

        LispObject obj = arguments.next();
        if (obj.isInstance(LispType.ITERATOR)) {
            return obj.cast(LispType.ITERATOR);
        }
        if (obj.isInstance(LispType.JAVA_OBJECT)) {
            return handleJavaObject(((LispJavaObject)obj.cast(LispType.JAVA_OBJECT)).getObject());
        }
        if (obj.isInstance(LispType.LIST)) {
            return new LispListIterator((LispList)obj.cast(LispType.LIST));
        }
        if (obj.isInstance(LispType.STRING)) {
            return new LispStringIterator((LispString)obj.cast(LispType.STRING));
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
