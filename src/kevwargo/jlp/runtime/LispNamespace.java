package kevwargo.jlp.runtime;

import kevwargo.jlp.objects.base.LispNamedObject;
import kevwargo.jlp.objects.base.LispObject;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class LispNamespace {

    private Layer builtins;
    private Layer globals;
    private List<Layer> overlays;

    public LispNamespace(Layer builtins) {
        this.builtins = builtins;
        this.globals = new Layer(false);
        this.overlays = new ArrayList<Layer>();
    }

    public LispNamespace with(Layer layer) {
        LispNamespace clone = new LispNamespace(builtins);
        clone.globals = globals;
        clone.overlays = new ArrayList<Layer>(overlays);
        clone.overlays.add(0, layer);
        return clone;
    }

    public void bind(String name, LispObject definition) {
        bind(name, definition, false);
    }

    public void bind(String name, LispObject definition, boolean global) {
        if (global) {
            globals.put(name, definition);
            return;
        }

        for (Layer overlay : overlays) {
            if (overlay.sticky || overlay.containsKey(name)) {
                overlay.put(name, definition);
                return;
            }
        }

        if (overlays.isEmpty()) {
            globals.put(name, definition);
        } else {
            overlays.get(0).put(name, definition);
        }
    }

    public LispObject get(String name) {
        for (Layer overlay : overlays) {
            if (overlay.containsKey(name)) {
                return overlay.get(name);
            }
        }

        if (globals.containsKey(name)) {
            return globals.get(name);
        }

        return builtins.get(name);
    }

    public void delete(String name) {
        for (Layer overlay : overlays) {
            if (overlay.remove(name) != null) {
                return;
            }
        }

        globals.remove(name);
    }

    public void dump(OutputStream out) {
        dump(new PrintStream(out));
    }

    public void dump(PrintStream out) {
        dumpHeader(out);
        dumpLayer(out, builtins, "Builtins");
        dumpLayer(out, globals, "Globals");
        dumpOverlays(out);
        dumpFooter(out);
    }

    private void dumpHeader(PrintStream out) {
        out.printf("Namespace 0x%x: {", System.identityHashCode(this));
    }

    private void dumpOverlays(PrintStream out) {
        if (overlays.isEmpty()) {
            return;
        }

        Iterator<Layer> it = overlays.iterator();
        Layer layer = it.next();
        out.print("\n  ");
        dumpLayer(out, layer, "Overlay");

        while (it.hasNext()) {
            out.print(",\n  ");
            dumpLayer(out, it.next(), "Overlay");
        }
    }

    private void dumpLayer(PrintStream out, Layer layer, String name) {
        out.printf("\n  %s 0x%x: {", name, System.identityHashCode(layer));
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
        out.println("\n}");
    }

    public static class Layer extends HashMap<String, LispObject> {

        boolean sticky;

        public Layer() {
            super();
        }

        public Layer(Map<String, LispObject> map) {
            super(map);
        }

        public Layer(boolean sticky) {
            this.sticky = sticky;
        }

        public void define(LispNamedObject obj) {
            put(obj.getName(), obj);
        }
    }
}
