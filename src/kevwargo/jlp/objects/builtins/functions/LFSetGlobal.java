package kevwargo.jlp.objects.builtins.functions;

public class LFSetGlobal extends LFSet {

    public static final String NAME = "set-global";

    private boolean global;

    public LFSetGlobal() {
        super(NAME, true);
    }
}
