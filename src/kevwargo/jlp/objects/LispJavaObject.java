package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.objects.types.TypeInitializer;


public class LispJavaObject extends LispObject {

    private Object object;

    public LispJavaObject(Object object) {
        super();
        TypeInitializer.instance().deferTypeSet(this, "java-object");
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

}
