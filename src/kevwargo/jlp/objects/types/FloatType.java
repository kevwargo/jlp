package kevwargo.jlp.objects.types;

public class FloatType extends LispType {

    FloatType() {
        super(LispType.TYPE, "float", new LispType[] { LispType.OBJECT });
    }

}
