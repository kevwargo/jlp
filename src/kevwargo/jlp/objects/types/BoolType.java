package kevwargo.jlp.objects.types;

public class BoolType extends LispType {

    BoolType() {
        super(LispType.TYPE, "bool", new LispType[] { LispType.OBJECT });
    }

}
