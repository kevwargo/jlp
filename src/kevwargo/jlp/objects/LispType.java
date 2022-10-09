package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.calls.CallArgs;

public abstract class LispType extends LispBaseObject implements LispCallable, LispNamedObject {

    public static final LispType OBJECT = new ObjectType();
    public static final LispType TYPE = new Type();
    public static final LispType SYMBOL = new SymbolType();
    public static final LispType STRING = new StringType();
    public static final LispType LIST = new ListType();
    public static final LispType BOOL = new BoolType();
    public static final LispType NIL = new NilType();
    public static final LispType INT = new IntType();
    public static final LispType FLOAT = new FloatType();
    public static final LispType FUNCTION = new FunctionType("builtin-function");
    public static final LispType MACRO = new FunctionType("builtin-macro", FUNCTION);
    public static final LispType LISP_FUNCTION = new FunctionType("function", FUNCTION);
    public static final LispType LISP_MACRO = new FunctionType("macro", MACRO);
    public static final LispType METHOD = new FunctionType("method", FUNCTION);
    public static final LispType JAVA_OBJECT = new JavaObjectType();
    public static final LispType ITERATOR = new IteratorType();

    protected static final String ARG_OBJ = "obj";
    protected CallArgs callArgs;

    static {
        OBJECT.setType(TYPE);
        OBJECT.setBases(new LispType[0]);
        TYPE.setType(TYPE);
        TYPE.setBases(new LispType[] {OBJECT});
    }

    private LispType bases[];
    private String name;

    public LispType(String name, LispType[] bases) {
        this(name, bases, new CallArgs().opt(ARG_OBJ));
    }

    public LispType(String name, LispType[] bases, CallArgs callArgs) {
        super(TYPE);
        this.name = name;
        this.bases = bases;
        this.callArgs = callArgs;
    }

    LispType(String name, CallArgs callArgs) {
        super();
        this.name = name;
        this.callArgs = callArgs;
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

    public boolean bool() {
        return true;
    }

    public CallArgs getCallArgs() {
        return callArgs;
    }
}

class Type extends LispType {

    private static final String ARG_OBJ_OR_NAME = "obj-or-name";
    private static final String ARG_BASES = "bases";

    Type() {
        super("type", new CallArgs(ARG_OBJ_OR_NAME).opt(ARG_BASES));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        if (!args.containsKey(ARG_BASES)) {
            return args.get(ARG_OBJ_OR_NAME).getType();
        }

        String name = ((LispString) args.get(ARG_OBJ_OR_NAME).cast(STRING)).getValue();
        LispList bases = (LispList) args.get(ARG_BASES).cast(LIST);

        LispType baseTypes[] = new LispType[bases.size()];
        for (int i = 0; i < baseTypes.length; i++) {
            baseTypes[i] = (LispType) bases.get(i).cast(TYPE);
        }

        return new LispType(name, baseTypes) {
            public LispObject call(LispRuntime runtime, LispNamespace.Layer args)
                    throws LispException {
                return new LispBaseObject(this);
            }
        };
    }
}
