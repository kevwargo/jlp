package kevwargo.jlp.objects.iterator;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;


public class JavaIterator extends LispIterator {

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
