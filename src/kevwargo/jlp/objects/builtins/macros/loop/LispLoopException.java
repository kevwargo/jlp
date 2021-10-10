package kevwargo.jlp.objects.builtins.macros.loop;

import kevwargo.jlp.LispException;


public class LispLoopException extends LispException {

    private boolean isContinue;
    private long level;

    public LispLoopException(boolean isContinue, long level) {
        super("'break' or 'continue' used outside of a loop");
        this.isContinue = isContinue;
        this.level = level;
    }

    public LispLoopException(LispLoopException origin) {
        this(origin.isContinue, origin.level - 1);
    }

    public long getLevel() {
        return level;
    }

    public boolean isContinue() {
        return isContinue;
    }

}
