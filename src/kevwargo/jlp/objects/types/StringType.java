package kevwargo.jlp.objects.types;

public class StringType extends LispType {

    StringType() {
        super(LispType.TYPE, "str", new LispType[] { LispType.OBJECT });
    }

}
