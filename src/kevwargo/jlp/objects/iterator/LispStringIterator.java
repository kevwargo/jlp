package kevwargo.jlp.objects.iterator;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;


public class LispStringIterator extends LispIterator {

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
        int chr = string.codePointAt(pos++);
        return new LispInt(chr);
    }

}
