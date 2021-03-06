package kevwargo.jlp.utils;

import java.io.PrintStream;
import java.util.HashMap;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispCastException;
import kevwargo.jlp.objects.types.LispType;

public class LispNamespace {

    private HashMap<String, LispObject>[] components;

    @SuppressWarnings("unchecked")
    public LispNamespace() {
        components = new HashMap[0];
    }

    @SuppressWarnings("unchecked")
    public LispNamespace(HashMap<String, LispObject> map) {
        components = new HashMap[1];
        components[0] = map;
    }

    public LispNamespace(HashMap<String, LispObject>[] components) {
        this.components = components;
    }

    @SuppressWarnings("unchecked")
    public LispNamespace append(HashMap<String, LispObject> map) {
        HashMap<String, LispObject>[] components = new HashMap[this.components.length + 1];
        for (int i = 0; i < this.components.length; i++) {
            components[i] = this.components[i];
        }
        components[this.components.length] = map;
        return new LispNamespace(components);
    }
    
    @SuppressWarnings("unchecked")
    public LispNamespace prepend(HashMap<String, LispObject> map) {
        HashMap<String, LispObject>[] components = new HashMap[this.components.length + 1];
        components[0] = map;
        for (int i = 1; i < components.length; i++) {
            components[i] = this.components[i - 1];
        }
        return new LispNamespace(components);
    }

    public void bind(String name, LispObject definition) {
        int length = components.length;
        boolean defined = false;
        for (int i = 0; i < length - 1; i++) {
            if (components[i].containsKey(name)) {
                components[i].put(name, definition);
                defined = true;
                break;
            }
        }
        if (length > 0 && !defined) {
            components[length - 1].put(name, definition);
        }
    }

    public LispObject get(String name) {
        for (HashMap<String, LispObject> component : components) {
            LispObject object = component.get(name);
            if (object != null) {
                return object;
            }
        }
        return null;
    }
        
    public LispObject resolve(String name) throws LispException {
        LispObject object = get(name);
        if (object != null) {
            return object;
        }
        throw new LispException("Symbol's definition is void: '%s'", name);
    }

    public HashMap<String, LispObject>[] getComponents() {
        return components;
    }

    public PrintStream getOutput() {
        LispObject out = get("*out*");
        if (out != null) {
            try {
                Object outStream = ((LispJavaObject)out.cast(LispType.JAVA_OBJECT)).getObject();
                if (outStream instanceof PrintStream) {
                    return (PrintStream)outStream;
                }
            } catch (LispCastException e) {}
        }
        return System.out;
    }
    
}
