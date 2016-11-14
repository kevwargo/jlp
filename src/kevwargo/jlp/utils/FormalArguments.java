package kevwargo.jlp.utils;

import java.util.ArrayList;
import java.util.List;

public class FormalArguments {

    private List<String> positional;
    private String rest;

    public FormalArguments() {
        positional = new ArrayList<String>();
    }

    public FormalArguments(List<String> positional, String rest) {
        this.positional = positional;
        this.rest = rest;
    }

    public List<String> positional() {
        return positional;
    }

    public FormalArguments addPositional(String arg) {
        positional.add(arg);
        return this;
    }

    public String rest() {
        return rest;
    }

    public FormalArguments setRest(String rest) {
        this.rest = rest;
        return this;
    }
    
}
