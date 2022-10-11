package kevwargo.jlp.exceptions;

public class DuplicatedArgException extends LispException {

    public DuplicatedArgException(String name) {
        super("Argument '%s' declared multiple times", name);
    }
}
