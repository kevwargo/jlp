package kevwargo.jlp.objects.iterator;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;


public class LispListIterator extends LispIterator {

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
