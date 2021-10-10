package kevwargo.jlp.objects.types;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
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
    public static final LispType JAVA_OBJECT;
    public static final LispType ITERATOR;


    protected LispType baseTypes[];
    protected String name;


    public LispType(String name) {
        this.name = name;
        dict.put("@init@", new DefaultConstructor());
    }

    public String getName() {
        return name;
    }

    public LispType[] getBaseTypes() {
        return baseTypes;
    }

    public void setBaseTypes(LispType types[]) {
        baseTypes = types;
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
            throw new LispException("(%s) takes 1 or 2 arguments", name);
        }
        String name = ((LispString)arguments.next().cast(STRING)).getValue();
        LispList basesList = (LispList)arguments.next().cast(LIST);
        LispType bases[] = new LispType[basesList.size()];
        int pos = 0;
        for (LispObject base : basesList) {
            bases[pos++] = (LispType)base.cast(TYPE);
        }
        LispType type = new LispType(name);
        type.setType(TYPE);
        type.setBaseTypes(bases);
        return type;
    }

    private class DefaultConstructor extends LispFunction {

        DefaultConstructor() {
            super("@init@", new FormalArguments().pos("self").rest("args"));
            TypeInitializer.instance().deferTypeSet(this, "builtin-function");
        }

        protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
            LispObject self = arguments.get("self");
            LispList args = (LispList)arguments.get("args").cast(LispType.LIST);
            ArgumentsIterator it = new ArgumentsIterator(args.iterator(), namespace, args.size());
            LispObject cast = LispType.this.makeInstance(namespace, it);
            self.defineCast(LispType.this, cast);
            return LispBool.NIL;
        }

    }

    static {
        try {
            TYPE = TypeInitializer.instance().initType("type");
            OBJECT = TypeInitializer.instance().initType("object");
            BOOL = TypeInitializer.instance().initType("bool");
            FLOAT = TypeInitializer.instance().initType("float");
            FUNCTION = TypeInitializer.instance().initType("builtin-function");
            METHOD = TypeInitializer.instance().initType("method");
            MACRO = TypeInitializer.instance().initType("builtin-macro");
            LISP_FUNCTION = TypeInitializer.instance().initType("function");
            LISP_MACRO = TypeInitializer.instance().initType("macro");
            INT = TypeInitializer.instance().initType("int");
            LIST = TypeInitializer.instance().initType("list");
            STRING = TypeInitializer.instance().initType("string");
            SYMBOL = TypeInitializer.instance().initType("symbol");
            JAVA_OBJECT = TypeInitializer.instance().initType("java-object");
            ITERATOR = TypeInitializer.instance().initType("iterator");

            TypeInitializer.instance().check();
        } catch (LispException e) {
            throw new ExceptionInInitializerError(e);
        }
    }
}
