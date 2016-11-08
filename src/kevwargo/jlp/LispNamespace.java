package kevwargo.jlp;

import java.util.HashMap;
import kevwargo.jlp.objects.*;

public class LispNamespace {

    private HashMap[] elements;

    public LispNamespace() {
        elements = new HashMap[0];
    }

    public LispNamespace(HashMap<String, LispObject> basicNamespace) {
        elements = new HashMap[1];
        elements[0] = basicNamespace;
    }

    public LispNamespace(HashMap[] namespace) {
        elements = namespace;
    }

    public LispNamespace append(HashMap<String, LispObject> namespace) {
        HashMap[] elements = new HashMap[this.elements.length + 1];
        for (int i = 0; i < this.elements.length; i++) {
            elements[i] = this.elements[i];
        }
        elements[this.elements.length] = namespace;
        return new LispNamespace(elements);
    }
    
    public LispNamespace prepend(HashMap<String, LispObject> namespace) {
        HashMap[] elements = new HashMap[this.elements.length + 1];
        elements[0] = namespace;
        for (int i = 1; i < elements.length; i++) {
            elements[i] = this.elements[i - 1];
        }
        return new LispNamespace(elements);
    }

    public void bind(String name, LispObject definition) {
        int length = elements.length;
        boolean defined = false;
        for (int i = 0; i < length - 1; i++) {
            if (elements[i].containsKey(name)) {
                elements[i].put(name, definition);
                defined = true;
                break;
            }
        }
        if (length > 0 && !defined) {
            elements[length - 1].put(name, definition);
        }
    }

    public LispObject resolve(String name) throws LispException {
        for (HashMap<String, LispObject> element : elements) {
            LispObject object = element.get(name);
            if (object != null) {
                return object;
            }
        }
        throw new LispException("Symbol's definition is void: " + name);
    }
    
}
