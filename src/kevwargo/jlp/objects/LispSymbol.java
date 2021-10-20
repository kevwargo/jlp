package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispCastException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.utils.ArgumentsIterator;


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

    public String repr() {
        return name;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof LispObject)) {
            return false;
        }

        try {
            LispSymbol symbol = (LispSymbol)((LispObject)obj).cast(LispType.SYMBOL);
            return symbol.getName().equals(getName());
        } catch (LispCastException e) {
            return false;
        }
    }

}

class SymbolType extends LispType {

    SymbolType() {
        super("symbol", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        LispObject obj = arguments.next();
        if (obj.isInstance(this)) {
            return obj;
        }
        if (obj.isInstance(LispType.STRING)) {
            return new LispSymbol(((LispString)obj.cast(LispType.STRING)).getValue());
        }

        throw new LispCastException("The argument to symbol's constructor should be a symbol or a string");
    }

}
