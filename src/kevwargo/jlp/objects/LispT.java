package kevwargo.jlp.objects;


public class LispT extends LispDataObject {

    private static LispT instance;

    public static LispT getInstance() {
        if (instance == null) {
            instance = new LispT();
        }
        return instance;
    }

    private LispT() {}

    public String toString() {
        return "t";
    }

}
