package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.LispNamespace;


public class LispSymbol extends LispObject {

    private String name;

    public LispSymbol(String name) {
        super(LispType.SYMBOL);
        this.name = name;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        if (name.startsWith(":")) {
            return this;
        }
        return namespace.resolve(name);
    }

    public String getName() {
        return name;
    }

    public String toString() {
        return name;
    }
    
}
