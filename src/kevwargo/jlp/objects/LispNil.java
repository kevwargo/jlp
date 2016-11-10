package kevwargo.jlp.objects;


public class LispNil extends LispDataObject {

    private static LispNil instance;

    public static LispNil getInstance() {
        if (instance == null) {
            instance = new LispNil();
        }
        return instance;
    }

    private LispNil() {}

    public String toString() {
        return "nil";
    }

    public boolean getBooleanValue() {
        return false;
    }
    
}
