package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;

public class LispSymbol extends LispBaseObject {

    private String name;

    public LispSymbol(String name) {
        super(LispType.SYMBOL);
        this.name = name;
    }

    public LispObject eval(LispRuntime runtime) throws LispException {
        if (name.startsWith(":")) {
            return this;
        }

        LispNamespace namespace = runtime.getNS();
        LispObject obj = namespace.get(name);
        if (obj != null) {
            return obj;
        }

        LispObject cls = LispJavaClass.forName(name);
        if (cls == null) {
            throw new LispException("Symbol '%s' is not bound", name);
        }
        namespace.bind(name, cls);
        return cls;
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
            LispSymbol symbol = (LispSymbol) ((LispObject) obj).cast(LispType.SYMBOL);
            return symbol.getName().equals(getName());
        } catch (LispCastException e) {
            return false;
        }
    }
}

class SymbolType extends LispType {

    SymbolType() {
        super("symbol", new LispType[] {OBJECT});
    }

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
        LispObject obj = arguments.next();
        if (obj.isInstance(this)) {
            return obj;
        }
        if (obj.isInstance(LispType.STRING)) {
            return new LispSymbol(((LispString) obj.cast(LispType.STRING)).getValue());
        }

        throw new LispCastException(
                "The argument to symbol's constructor should be a symbol or a string");
    }
}
