package kevwargo.jlp.utils;

import java.util.ArrayList;
import java.util.List;

public class FormalArguments {

    private List<String> positional;
    private String rest;
    private List<String> optional;

    public FormalArguments(String... args) {
        positional = new ArrayList<String>(args.length);
        for (String arg : args) {
            positional.add(arg);
        }
    }

    public List<String> pos() {
        return positional;
    }

    public FormalArguments pos(String arg) {
        positional.add(arg);
        return this;
    }

    public List<String> opt() {
        return optional;
    }

    public FormalArguments opt(String arg) {
        optional.add(arg);
        return this;
    }

    public String rest() {
        return rest;
    }

    public FormalArguments rest(String rest) {
        this.rest = rest;
        return this;
    }

}
