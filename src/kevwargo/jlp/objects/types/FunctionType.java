package kevwargo.jlp.objects.types;

public class FunctionType extends LispType {

    FunctionType() {
        super(LispType.TYPE, "builtin-function", new LispType[] { LispType.OBJECT });
    }

    FunctionType(String name, LispType baseTypes[]) {
        super(LispType.TYPE, name, baseTypes);
    }

}
