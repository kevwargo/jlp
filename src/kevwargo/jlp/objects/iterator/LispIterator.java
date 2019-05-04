package kevwargo.jlp.objects.iterator;

import java.util.Iterator;

import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.TypeInitializer;


public abstract class LispIterator extends LispObject implements Iterator<LispObject> {

    public LispIterator() {
        super();
        TypeInitializer.instance().deferTypeSet(this, "iterator");
    }

}
