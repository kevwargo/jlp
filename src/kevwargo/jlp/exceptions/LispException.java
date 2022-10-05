package kevwargo.jlp.exceptions;

public class LispException extends Exception {

    public LispException(String fmt, Object... args) {
        super(String.format(fmt, args));
    }

    public LispException(Throwable t) {
        super(t);
    }
}
