package kevwargo.jlp.runtime.builtins.functions;

public class LFSetGlobal extends LFSet {

    public static final String NAME = "set-global";

    public LFSetGlobal() {
        super(NAME, true);
    }
}
