package kevwargo.jlp.objects.types;

public class IntType extends LispType {

    IntType() {
        super(LispType.TYPE, "int", new LispType[] { LispType.OBJECT });
    }

}
