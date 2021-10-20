package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class LispJavaObject extends LispObject {

    private Object object;

    public LispJavaObject(Object object) {
        super(LispType.JAVA_OBJECT);
        this.object = object;
    }

    public Object getObject() {
        return object;
    }

    public String repr() {
        return object.toString();
    }

    public String toString() {
        return object.toString();
    }

    public Object format() {
        return object;
    }

    public Object getJavaObject() {
        return object;
    }

    public Class<?> getJavaClass() {
        return object.getClass();
    }

}

class JavaObjectType extends LispType {

    JavaObjectType() {
        super("java-object", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.hasNext()) {
            throw new LispException("java-object's constructor does not accept any arguments");
        }

        return new LispJavaObject(new Object());
    }

}
