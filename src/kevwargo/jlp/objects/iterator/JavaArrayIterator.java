package kevwargo.jlp.objects.iterator;

import java.util.NoSuchElementException;

import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;


public class JavaArrayIterator extends LispIterator {

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
