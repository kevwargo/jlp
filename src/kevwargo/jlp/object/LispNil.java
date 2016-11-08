package kevwargo.jlp.object;


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
    
}
