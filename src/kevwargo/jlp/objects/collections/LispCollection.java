package kevwargo.jlp.objects.collections;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public interface LispCollection extends LispObject {

    public static final LispType TYPE = new CollectionType();

    public LispObject getItem(LispObject key) throws LispException;
}

class CollectionType extends LispType {

    CollectionType() {
        super("collection", new LispType[] {LispBaseObject.TYPE}, CallArgs.ignored());
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        throw new LispException("Cannot instantiate %s", getName());
    }
}
