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

    private List<Map<String, LispObject>> layers;

    public LispNamespace() {
        layers = new ArrayList<Map<String, LispObject>>();
    }

    public LispNamespace(Map<String, LispObject> map) {
        this();
        if (!map.isEmpty()) {
            layers.add(map);
        }
    }

    private LispNamespace(List<Map<String, LispObject>> layers) {
        this.layers = layers;
    }

    public LispNamespace with(Map<String, LispObject>... maps) {
        List<Map<String, LispObject>> layers = new ArrayList<Map<String, LispObject>>(this.layers);
        for (Map<String, LispObject> map : maps) {
            if (!map.isEmpty()) {
                layers.add(0, map);
            }
        }
        return new LispNamespace(layers);
    }

    public void bind(String name, LispObject definition) {
        Map<String, LispObject> layer = null;
        Iterator<Map<String, LispObject>> it = layers.iterator();
        while (it.hasNext()) {
            layer = it.next();
            if (layer.containsKey(name)) {
                layer.put(name, definition);
                return;
            }
        }
        if (layer == null) {
            layer = new HashMap<String, LispObject>();
            layers.add(layer);
        }
        layer.put(name, definition);
    }

    public LispObject get(String name) {
        for (Map<String, LispObject> layer : layers) {
            LispObject object = layer.get(name);
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
        dumpLayers(out);
        dumpFooter(out);
    }

    private void dumpHeader(PrintStream out) {
        out.printf("Namespace 0x%x: {", System.identityHashCode(this));
    }

    private void dumpLayers(PrintStream out) {
        if (layers.isEmpty()) {
            return;
        }

        Iterator<Map<String, LispObject>> it = layers.iterator();
        Map<String, LispObject> layer = it.next();
        out.print("\n  ");
        dumpLayer(out, layer);

        while (it.hasNext()) {
            out.print(",\n  ");
            dumpLayer(out, it.next());
        }
    }

    private void dumpLayer(PrintStream out, Map<String, LispObject> layer) {
        out.printf("Map 0x%x: {", System.identityHashCode(layer));
        Map<String, LispObject> sorted = new TreeMap<String, LispObject>(layer);
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
        if (!layers.isEmpty()) {
            out.print('\n');
        }
        out.println("}");
    }
}
