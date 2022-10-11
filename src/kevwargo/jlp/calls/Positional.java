package kevwargo.jlp.calls;

public class Positional {

    String name;
    CallArgs struct;

    public Positional(String name) {
        this.name = name;
    }

    public Positional(CallArgs struct) {
        this.struct = struct;
    }

    public String getName() {
        return name;
    }

    public CallArgs getStruct() {
        return struct;
    }

    public boolean equals(Object other) {
        if (!(other instanceof Positional)) {
            return false;
        }

        return getName().equals(((Positional) other).getName());
    }
}
