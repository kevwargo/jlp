package kevwargo.jlp.objects.types;

public class SymbolType extends LispType {

    SymbolType() {
        super(LispType.TYPE, "symbol", new LispType[] { LispType.OBJECT });
    }
    
}
