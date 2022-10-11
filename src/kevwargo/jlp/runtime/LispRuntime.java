package kevwargo.jlp.runtime;

import java.io.InputStream;
import java.io.OutputStream;

public class LispRuntime {

    private LispNamespace ns;
    private InputStream in;
    private OutputStream out;

    public LispRuntime(LispNamespace ns, InputStream in, OutputStream out) {
        this.ns = ns;
        this.in = in;
        this.out = out;
    }

    public LispNamespace getNS() {
        return ns;
    }

    public InputStream getIn() {
        return in;
    }

    public OutputStream getOut() {
        return out;
    }

    public LispRuntime with(LispNamespace ns) {
        return new LispRuntime(ns, in, out);
    }

    public LispRuntime with(LispNamespace.Layer layer) {
        return new LispRuntime(ns.with(layer), in, out);
    }

    public LispRuntime with(InputStream in) {
        return new LispRuntime(ns, in, out);
    }

    public LispRuntime with(OutputStream out) {
        return new LispRuntime(ns, in, out);
    }
}
