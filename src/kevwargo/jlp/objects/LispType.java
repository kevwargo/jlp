package kevwargo.jlp.objects;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LispType extends LispObject {

    public static final LispType OBJECT = new ObjectType();
    public static final LispType TYPE = new Type();
    public static final LispType SYMBOL = new SymbolType();
    public static final LispType STRING = new StringType();
    public static final LispType LIST = new ListType();
    public static final LispType BOOL = new BoolType();
    public static final LispType INT = new IntType();
    public static final LispType FLOAT = new FloatType();
    public static final LispType FUNCTION = new FunctionType("builtin-function");
    public static final LispType MACRO = new FunctionType("builtin-macro", FUNCTION);
    public static final LispType LISP_FUNCTION = new FunctionType("function", FUNCTION);
    public static final LispType LISP_MACRO = new FunctionType("macro", MACRO);
    public static final LispType METHOD = new FunctionType("method", FUNCTION);
    public static final LispType JAVA_OBJECT = new JavaObjectType();
    public static final LispType ITERATOR = new IteratorType();

    static {
        OBJECT.setType(TYPE);
        OBJECT.setBases(new LispType[0]);
        TYPE.setType(TYPE);
        TYPE.setBases(new LispType[] { OBJECT });
    }

    private LispType bases[];
    private String name;

    public LispType(String name, LispType[] bases) {
        super(TYPE);
        this.name = name;
        this.bases = bases;
    }

    LispType(String name) {
        super();
        this.name = name;
    }

    void setBases(LispType[] bases) {
        this.bases = bases;
    }

    public String getName() {
        return name;
    }

    public LispType[] getBases() {
        return bases;
    }

    public boolean isSubtype(LispType type) {
        if (type.equals(this)) {
            return true;
        }
        for (LispType base : bases) {
            if (base.isSubtype(type)) {
                return true;
            }
        }
        return false;
    }

    public String repr() {
        return String.format("<type '%s'>", name);
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        return makeInstance(namespace, arguments);
    }

    public abstract LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException;
}

class Type extends LispType {

    Type() {
        super("type");
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.getLength() == 1) {
            return arguments.next().getType();
        }
        return super.call(namespace, arguments);
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.getLength() != 2) {
            throw new LispException("(%s) takes 1 or 2 arguments", getName());
        }
        String name = ((LispString)arguments.next().cast(STRING)).getValue();
        LispList basesList = (LispList)arguments.next().cast(LIST);
        LispType bases[] = new LispType[basesList.size()];
        int pos = 0;
        for (LispObject base : basesList) {
            bases[pos++] = (LispType)base.cast(TYPE);
        }
        return new LispType(name, bases) {
            public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
                // TODO: maybe bring back default constructor, idk
                return new LispObject(Type.this);
            }
        };
    }

}