package kevwargo.jlp.objects.scalars;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.wrappers.LispJavaClass;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.HashMap;
import java.util.Map;

public class LispSymbol extends LispBaseObject {

    public static final LispType TYPE = new SymbolType();

    private static Map<String, LispSymbol> cache = new HashMap<String, LispSymbol>();
    private String name;

    private LispSymbol(String name) {
        super(LispSymbol.TYPE);
        this.name = name;
    }

    public static LispSymbol make(String name) {
        if (!cache.containsKey(name)) {
            cache.put(name, new LispSymbol(name));
        }

        return cache.get(name);
    }

    public LispObject eval(LispRuntime runtime) throws LispException {
        if (isKeyword()) {
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

    public boolean isKeyword() {
        return name.startsWith(":");
    }

    public String repr() {
        return name;
    }

    public boolean bool() {
        return true;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof LispObject)) {
            return false;
        }

        try {
            LispSymbol symbol = (LispSymbol) ((LispObject) obj).cast(LispSymbol.TYPE);
            return symbol.getName().equals(getName());
        } catch (LispCastException e) {
            return false;
        }
    }
}

class SymbolType extends LispType {

    SymbolType() {
        super("symbol", new LispType[] {LispBaseObject.TYPE}, new CallArgs(ARG_OBJ));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get(ARG_OBJ);
        if (obj.isInstance(this)) {
            return obj;
        }
        if (obj.isInstance(LispString.TYPE)) {
            return LispSymbol.make(((LispString) obj.cast(LispString.TYPE)).getValue());
        }

        throw new LispCastException(
                "The argument to symbol's constructor should be a symbol or a string");
    }
}
