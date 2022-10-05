package kevwargo.jlp.runtime;

import kevwargo.jlp.objects.LispObject;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

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
        List<Map<String, LispObject>> components =
                new ArrayList<Map<String, LispObject>>(this.components);
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

    public void dump() {
        dump(System.out);
    }

    public void dump(PrintStream out) {
        dumpHeader(out);
        dumpComponents(out);
        dumpFooter(out);
    }

    private void dumpHeader(PrintStream out) {
        out.printf("Namespace 0x%x: {", System.identityHashCode(this));
    }

    private void dumpComponents(PrintStream out) {
        if (components.isEmpty()) {
            return;
        }

        Iterator<Map<String, LispObject>> it = components.iterator();
        Map<String, LispObject> component = it.next();
        out.print("\n  ");
        dumpComponent(out, component);

        while (it.hasNext()) {
            out.print(",\n  ");
            dumpComponent(out, it.next());
        }
    }

    private void dumpComponent(PrintStream out, Map<String, LispObject> component) {
        out.printf("Map 0x%x: {", System.identityHashCode(component));
        Map<String, LispObject> sorted = new TreeMap<String, LispObject>(component);
        Iterator<Map.Entry<String, LispObject>> it = sorted.entrySet().iterator();

        if (it.hasNext()) {
            Map.Entry<String, LispObject> entry = it.next();
            out.printf("\n    %s: %s", entry.getKey(), entry.getValue().toString());
            while (it.hasNext()) {
                entry = it.next();
                out.printf(",\n    %s: %s", entry.getKey(), entry.getValue().toString());
            }
            out.print("\n  }");
        } else {
            out.print("}");
        }
    }

    private void dumpFooter(PrintStream out) {
        if (!components.isEmpty()) {
            out.print('\n');
        }
        out.println("}");
    }
}
