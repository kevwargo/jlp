package kevwargo.jlp.utils;

import java.util.HashMap;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispObject;

public class LispNamespace {

    private HashMap<String, LispObject>[] components;

    public LispNamespace() {
        components = new HashMap[0];
    }

    public LispNamespace(HashMap<String, LispObject> map) {
        components = new HashMap[1];
        components[0] = map;
    }

    public LispNamespace(HashMap<String, LispObject>[] components) {
        this.components = components;
    }

    public LispNamespace append(HashMap<String, LispObject> map) {
        HashMap<String, LispObject>[] components = new HashMap[this.components.length + 1];
        for (int i = 0; i < this.components.length; i++) {
            components[i] = this.components[i];
        }
        components[this.components.length] = map;
        return new LispNamespace(components);
    }
    
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

    public LispObject resolve(String name) throws LispException {
        for (HashMap<String, LispObject> component : components) {
            LispObject object = component.get(name);
            if (object != null) {
                return object;
            }
        }
        throw new LispException("Symbol's definition is void: '%s'", name);
    }

    public HashMap<String, LispObject>[] getComponents() {
        return components;
    }
    
}
