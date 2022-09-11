package kevwargo.jlp.utils;

import java.io.PrintStream;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.LispCastException;
import kevwargo.jlp.objects.LispType;

public class LispNamespace {

    private Map<String, LispObject>[] components;

    @SuppressWarnings("unchecked")
    public LispNamespace() {
        components = new Map[0];
    }

    @SuppressWarnings("unchecked")
    public LispNamespace(Map<String, LispObject> map) {
        components = new Map[1];
        components[0] = map;
    }

    public LispNamespace(Map<String, LispObject>[] components) {
        this.components = components;
    }

    @SuppressWarnings("unchecked")
    public LispNamespace append(Map<String, LispObject> map) {
        Map<String, LispObject>[] components = new Map[this.components.length + 1];
        for (int i = 0; i < this.components.length; i++) {
            components[i] = this.components[i];
        }
        components[this.components.length] = map;
        return new LispNamespace(components);
    }

    @SuppressWarnings("unchecked")
    public LispNamespace prepend(Map<String, LispObject> map) {
        Map<String, LispObject>[] components = new Map[this.components.length + 1];
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
        for (Map<String, LispObject> component : components) {
            LispObject object = component.get(name);
            if (object != null) {
                return object;
            }
        }
        return null;
    }

    public Map<String, LispObject>[] getComponents() {
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

    public void dump() {
        printHeader();
        boolean empty = printComponents();
        printFooter(empty);
    }

    private void printHeader() {
        System.out.printf("Namespace 0x%x: {", System.identityHashCode(this));
    }

    private boolean printComponents() {
        if (components.length > 0) {
            System.out.print("\n  ");
            printComponent(components[0]);
        } else {
            return true;
        }

        for (int i = 1; i < components.length; i++) {
            System.out.print(",\n  ");
            printComponent(components[i]);
        }

        return false;
    }

    private void printComponent(Map<String, LispObject> component) {
        System.out.printf("Map 0x%x: {", System.identityHashCode(component));
        Map<String, LispObject> sorted = new TreeMap<String, LispObject>(component);
        Iterator<Map.Entry<String, LispObject>> it = sorted.entrySet().iterator();

        if (it.hasNext()) {
            Map.Entry<String, LispObject> entry = it.next();
            System.out.printf("\n    %s: %s", entry.getKey(), entry.getValue().toString());
            while (it.hasNext()) {
                entry = it.next();
                System.out.printf(",\n    %s: %s", entry.getKey(), entry.getValue().toString());
            }
            System.out.print("\n  }");
        } else {
            System.out.print("}");
        }
    }

    private void printFooter(boolean empty) {
        if (!empty) {
            System.out.print('\n');
        }
        System.out.println("}");
    }

}
