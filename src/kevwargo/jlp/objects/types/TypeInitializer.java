package kevwargo.jlp.objects.types;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.BoolType;
import kevwargo.jlp.objects.types.FloatType;
import kevwargo.jlp.objects.types.FunctionType;
import kevwargo.jlp.objects.types.IntType;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.objects.types.ListType;
import kevwargo.jlp.objects.types.ObjectType;
import kevwargo.jlp.objects.types.StringType;
import kevwargo.jlp.objects.types.SymbolType;


public class TypeInitializer {

    private static TypeInitializer instance;

    private Map<String, LispType> typeMap;
    private DeferredObjects objectTypeDeferMap;
    private DeferredBaseTypes baseTypesDeferMap;


    private TypeInitializer() {
        typeMap = new HashMap<String, LispType>();
        objectTypeDeferMap = new DeferredObjects(typeMap);
        baseTypesDeferMap = new DeferredBaseTypes(typeMap);
    }

    public static TypeInitializer instance() {
        if (instance == null) {
            instance = new TypeInitializer();
        }
        return instance;
    }

    LispType initType(String name) throws LispException {
        LispType type = typeMap.get(name);
        if (type != null) {
            return type;
        }
        type = createType(name);
        typeMap.put(name, type);
        switch (name) {
        case "type":
            type.setType(type);
            baseTypesDeferMap.defer(type, new String[] { "object" });
            break;
        case "object":
            objectTypeDeferMap.defer(type, "type");
            type.setBaseTypes(new LispType[0]);
            break;
        case "method":
        case "builtin-macro":
        case "function":
            objectTypeDeferMap.defer(type, "type");
            baseTypesDeferMap.defer(type, new String[] { "builtin-function" });
            break;
        case "macro":
            objectTypeDeferMap.defer(type, "type");
            baseTypesDeferMap.defer(type, new String[] { "builtin-macro", "builtin-function" });
            break;
        case "list":
        case "string":
        case "symbol":
        case "bool":
        case "float":
        case "int":
        case "builtin-function":
        case "java-object":
        case "iterator":
            objectTypeDeferMap.defer(type, "type");
            baseTypesDeferMap.defer(type, new String[] { "object" });
            break;
        }

        objectTypeDeferMap.resolve(name, type);
        baseTypesDeferMap.resolve(name, type);

        return type;
    }

    private LispType createType(String name) throws LispException {
        switch (name) {
        case "type":    return new LispType("type");
        case "object":  return new ObjectType();
        case "list":    return new ListType();
        case "string":  return new StringType();
        case "symbol":  return new SymbolType();
        case "bool":    return new BoolType();
        case "float":   return new FloatType();
        case "int":     return new IntType();
        case "java-object": return new JavaObjectType();
        case "iterator": return new IteratorType();

        case "builtin-function":
        case "macro":
        case "method":
        case "builtin-macro":
        case "function":
            return new FunctionType(name);
        default:
            throw new LispException("Unknown built-in type: " + name);
        }
    }

    public void deferTypeSet(LispObject object, String typeName) {
        objectTypeDeferMap.defer(object, typeName);
    }

    public void deferCastDefine(LispObject object, String typeName, LispObject cast) {
        objectTypeDeferMap.defer(object, typeName, cast);
    }

    public void check() throws LispException {
        if (! objectTypeDeferMap.isEmpty()) {
            throw new LispException("Type initialization is in inconsistent state: some objects do not have type assigned");
        }
        if (! baseTypesDeferMap.isEmpty()) {
            throw new LispException("Type initialization is in inconsistent state: some types do not have base types assigned");
        }
    }


    private class DeferredBase {

        public LispType type;
        public LispType bases[];
        public int pos;

        public DeferredBase(LispType type, LispType bases[], int pos) {
            this.type = type;
            this.bases = bases;
            this.pos = pos;
        }

        public void set(LispType base) {
            bases[pos] = base;
            boolean done = true;
            for (LispType b : bases) {
                if (b == null) {
                    done = false;
                    break;
                }
            }
            if (done) {
                type.setBaseTypes(bases);
            }
        }

        public boolean isComplete() {
            for (LispType b : bases) {
                if (b == null) {
                    return false;
                }
            }
            return true;
        }

        public String toString() {
            StringBuffer sb = new StringBuffer();
            sb.append("type: " + type.getName());
            sb.append(", bases: [");
            for (LispType b : bases) {
                sb.append(b == null ? "(null)" : b.getName());
                sb.append(" ");
            }
            sb.append("], pos: " + pos);
            return sb.toString();
        }

    }

    private class DeferredBaseTypes {

        private Map<String, LispType> typeMap;
        private Map<String, List<DeferredBase>> deferred;

        public DeferredBaseTypes(Map<String, LispType> typeMap) {
            this.typeMap = typeMap;
            deferred = new HashMap<String, List<DeferredBase>>();
        }

        public void defer(LispType type, String baseNames[]) {
            LispType baseTypes[] = new LispType[baseNames.length];
            for (int i = 0; i < baseNames.length; i++) {
                LispType resolved = typeMap.get(baseNames[i]);
                if (resolved != null) {
                    DeferredBase deferredBase = new DeferredBase(type, baseTypes, i);
                    deferredBase.set(resolved);
                    if (deferredBase.isComplete()) {
                        return;
                    }
                }
            }
            for (int i = 0; i < baseNames.length; i++) {
                List<DeferredBase> list = deferred.get(baseNames[i]);
                if (list == null) {
                    list = new ArrayList<DeferredBase>();
                    deferred.put(baseNames[i], list);
                }
                DeferredBase db = new DeferredBase(type, baseTypes, i);
                list.add(db);
            }
        }

        public void resolve(String typeName, LispType type) {
            List<DeferredBase> list = deferred.get(typeName);
            if (list != null) {
                ListIterator<DeferredBase> it = list.listIterator();
                while (it.hasNext()) {
                    DeferredBase deferredBase = it.next();
                    deferredBase.set(type);
                    if (deferredBase.isComplete()) {
                        it.remove();
                    }
                }
            }
        }

        public boolean isEmpty() {
            if (deferred.isEmpty()) {
                return true;
            }
            for (List<DeferredBase> list : deferred.values()) {
                if (! list.isEmpty()) {
                    return false;
                }
            }
            return true;
        }

    }

    private class DeferredObject {

        public LispObject object;
        public LispObject cast;

        public DeferredObject(LispObject object, LispObject cast) {
            this.object = object;
            this.cast = cast;
        }

    }

    private class DeferredObjects {

        private Map<String, LispType> typeMap;
        private Map<String, List<DeferredObject>> deferred;

        public DeferredObjects(Map<String, LispType> typeMap) {
            this.typeMap = typeMap;
            deferred = new HashMap<String, List<DeferredObject>>();
        }

        public void defer(LispObject object, String typeName) {
            defer(object, typeName, null);
        }

        public void defer(LispObject object, String typeName, LispObject cast) {
            LispType type = typeMap.get(typeName);
            if (type != null) {
                if (cast != null) {
                    object.defineCast(type, cast);
                } else {
                    object.setType(type);
                }
            } else {
                List<DeferredObject> list = deferred.get(typeName);
                if (list == null) {
                    list = new ArrayList<DeferredObject>();
                    deferred.put(typeName, list);
                }
                list.add(new DeferredObject(object, cast));
            }
        }

        public void resolve(String typeName, LispType type) {
            List<DeferredObject> list = deferred.get(typeName);
            if (list != null) {
                for (DeferredObject deferredObject : list) {
                    if (deferredObject.cast != null) {
                        deferredObject.object.defineCast(type, deferredObject.cast);
                    } else {
                        deferredObject.object.setType(type);
                    }
                }
                deferred.remove(typeName);
            }
        }

        public boolean isEmpty() {
            return deferred.isEmpty();
        }

    }

}
