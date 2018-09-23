package kevwargo.jlp.objects.types;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LispType extends LispObject {

    public static final LispType TYPE;
    public static final LispType OBJECT;
    public static final LispType BOOL;
    public static final LispType FLOAT;
    public static final LispType FUNCTION;
    public static final LispType METHOD;
    public static final LispType MACRO;
    public static final LispType LISP_FUNCTION;
    public static final LispType LISP_MACRO;
    public static final LispType INT;
    public static final LispType LIST;
    public static final LispType STRING;
    public static final LispType SYMBOL;
    

    protected LispType baseTypes[];
    protected String name;


    protected LispType(LispType typeType, String name, LispType baseTypes[]) {
        super(typeType);
        this.name = name;
        if (baseTypes != null && baseTypes.length == 0) {
            this.baseTypes = new LispType[] { OBJECT };
        } else {
            this.baseTypes = baseTypes;
        }
        init();
    }

    protected LispType(String name) {
        this(null, name, null);
    }

    void init() {
        
    }

    public String getName() {
        return name;
    }

    public LispType[] getBaseTypes() {
        return baseTypes;
    }

    public boolean isSubtype(LispType baseType) {
        if (baseType.equals(this)) {
            return true;
        }
        for (LispType base : baseTypes) {
            if (base.isSubtype(baseType)) {
                return true;
            }
        }
        return false;
    }

    public String repr() {
        return String.format("<type '%s'>", name);
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (this == TYPE && arguments.getLength() == 1) {
            return arguments.next().getType();
        }
        return makeInstance(namespace, arguments);
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.getLength() != 2) {
            throw new LispException(String.format("(%s) takes 1 or 2 arguments", name));
        }
        String name = ((LispString)arguments.next().cast(STRING)).getValue();
        LispList basesList = (LispList)arguments.next().cast(LIST);
        LispType bases[] = new LispType[basesList.size()];
        int pos = 0;
        for (LispObject base : basesList) {
            bases[pos++] = (LispType)base.cast(TYPE);
        }
        return new LispType(this, name, bases);
    }


    static {
        TYPE = new LispType("type");
        OBJECT = new ObjectType();
        OBJECT.type = TYPE;
        OBJECT.baseTypes = new LispType[0];
        TYPE.type = TYPE;
        TYPE.baseTypes = new LispType[] { OBJECT };

        BOOL = new BoolType();
        FLOAT = new FloatType();
        FUNCTION = new FunctionType();
        METHOD = new FunctionType("method", new LispType[] { FUNCTION });
        MACRO = new FunctionType("builtin-macro", new LispType[] { FUNCTION });
        LISP_FUNCTION = new FunctionType("function", new LispType[] { FUNCTION });
        LISP_MACRO = new FunctionType("macro", new LispType[] { MACRO, LISP_FUNCTION });
        INT = new IntType();
        LIST = new ListType();
        STRING = new StringType();
        SYMBOL = new SymbolType();

        OBJECT.init();
        TYPE.init();
    }
}
