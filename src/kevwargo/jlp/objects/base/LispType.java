package kevwargo.jlp.objects.base;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispCallable;
import kevwargo.jlp.objects.scalars.LispString;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public abstract class LispType extends LispBaseObject implements LispCallable, LispNamedObject {

    public static final LispType TYPE = new Type();

    protected static final String ARG_OBJ = "obj";
    protected CallArgs callArgs;

    private static boolean typesInitialized = false;

    private LispType bases[];
    private String name;

    public LispType(String name, LispType[] bases) {
        this(name, bases, new CallArgs().opt(ARG_OBJ));
    }

    public LispType(String name, LispType[] bases, CallArgs callArgs) {
        super(TYPE);

        if (!typesInitialized) {
            LispType.TYPE.setType(LispType.TYPE);
            LispType.TYPE.setBases(new LispType[] {LispBaseObject.TYPE});
            LispBaseObject.TYPE.setType(LispType.TYPE);
            LispBaseObject.TYPE.setBases(new LispType[0]);
            typesInitialized = true;
        }

        this.name = name;
        this.bases = bases;
        this.callArgs = callArgs;
    }

    LispType(String name, CallArgs callArgs) {
        super();
        this.name = name;
        this.callArgs = callArgs;

        if (name.equals("object")) {
            bases = new LispType[0];
            if (TYPE == null) {}

            setType(TYPE);
        } else if (name.equals("type")) {
            bases = new LispType[] {LispBaseObject.TYPE};
            setType(this);
        }
    }

    private void setBases(LispType[] bases) {
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

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        if (!args.containsKey(ARG_BASES)) {
            return args.get(ARG_OBJ_OR_NAME).getType();
        }

        String name = ((LispString) args.get(ARG_OBJ_OR_NAME).cast(LispString.TYPE)).getValue();
        LispList bases = (LispList) args.get(ARG_BASES).cast(LispList.TYPE);

        LispType baseTypes[] = new LispType[bases.size()];
        for (int i = 0; i < baseTypes.length; i++) {
            baseTypes[i] = (LispType) bases.get(i).cast(TYPE);
        }

        return new LispType(name, baseTypes) {
            public LispObject call(LispRuntime runtime, Layer args) throws LispException {
                return new LispBaseObject(this);
            }
        };
    }
}
