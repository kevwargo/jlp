package kevwargo.jlp.utils;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;


public class LispNamespace {

    private List<Map<String, LispObject>> components;

    public LispNamespace() {
        components = new ArrayList<Map<String, LispObject>>();
    }

    public LispNamespace(Map<String, LispObject> map) {
        this();
        if (!map.isEmpty()) {
            components.add(map);
        }
    }

    private LispNamespace(List<Map<String, LispObject>> components) {
        this.components = components;
    }

    public LispNamespace prepend(Map<String, LispObject> map) {
        if (map.isEmpty()) {
            return this;
        }
        List<Map<String, LispObject>> components = new ArrayList<Map<String, LispObject>>(this.components);
        components.add(0, map);
        return new LispNamespace(components);
    }

    public void bind(String name, LispObject definition) {
        Map<String, LispObject> component = null;
        Iterator<Map<String, LispObject>> it = components.iterator();
        while (it.hasNext()) {
            component = it.next();
            if (component.containsKey(name)) {
                component.put(name, definition);
                return;
            }
        }
        if (component == null) {
            component = new HashMap<String, LispObject>();
            components.add(component);
        }
        component.put(name, definition);
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
        dumpHeader();
        dumpComponents();
        dumpFooter();
    }

    private void dumpHeader() {
        System.out.printf("Namespace 0x%x: {", System.identityHashCode(this));
    }

    private void dumpComponents() {
        if (components.isEmpty()) {
            return;
        }

        Iterator<Map<String, LispObject>> it = components.iterator();
        Map<String, LispObject> component = it.next();
        System.out.print("\n  ");
        dumpComponent(component);

        while (it.hasNext()) {
            System.out.print(",\n  ");
            dumpComponent(it.next());
        }
    }

    private void dumpComponent(Map<String, LispObject> component) {
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

    private void dumpFooter() {
        if (!components.isEmpty()) {
            System.out.print('\n');
        }
        System.out.println("}");
    }

}
