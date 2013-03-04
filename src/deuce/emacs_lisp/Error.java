package deuce.emacs_lisp;

import clojure.lang.RT;
import clojure.lang.Symbol;

public class Error extends RuntimeException {
    public Symbol tag;
    public Object value;

    public Error(Symbol tag, Object value) {
        this.tag = tag;
        this.value = value;
    }

    public String getMessage() {
        return toString();
    }

    public String toString() {
        return RT.printString(this);
    }
}
