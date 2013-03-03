package deuce.emacs_lisp;

import java.util.Objects;
import java.io.Serializable;

import clojure.lang.Sequential;
import clojure.lang.Seqable;
import clojure.lang.ISeq;
import clojure.lang.RT;

public class Cons implements Sequential, Seqable, Serializable {
    public Object fst, rst;

    public Cons(Object fst, Object rst) {
        this.fst = fst;
        this.rst = rst;
    }

    public ISeq seq() {
        if (rst == null) return RT.list(fst);
        if (rst instanceof Seqable) return RT.cons(fst, RT.seq(rst));
        return RT.list(fst, rst);
    }

    public String toString() {
        return RT.printString(this);
    }

    public boolean equals(Object o) {
        if (o instanceof Cons) {
            Cons c = (Cons) o;
            return Objects.equals(fst, c.fst) && Objects.equals(rst, c.rst);
        }
        if (o instanceof Seqable) return seq().equals(RT.seq(o));
        return false;
    }

    public int hashCode() {
        return 31 * Objects.hashCode(fst) + Objects.hashCode(rst);
    }
}
