package kevwargo.jlp.objects.types;

public class ListType extends LispType {

    ListType() {
        super(LispType.TYPE, "list", new LispType[] { LispType.OBJECT });
    }

}
