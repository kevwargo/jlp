package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;


public class LispBool extends LispObject {

    public static final LispBool TRUE = new LispBool(true);
    public static final LispBool FALSE = new LispBool(false);


    private boolean value;


    private LispBool(boolean value) {
        super(LispType.BOOL);
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    public String repr() {
        return value ? "t" : "nil";
    }
    
}
