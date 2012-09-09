package deuce;

public class DottedPair {
    public Object car;
    public Object cdr;

    public DottedPair(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DottedPair that = (DottedPair) o;

        if (car != null ? !car.equals(that.car) : that.car != null) return false;
        if (cdr != null ? !cdr.equals(that.cdr) : that.cdr != null) return false;

        return true;
    }

    public int hashCode() {
        int result = car != null ? car.hashCode() : 0;
        result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
        return result;
    }
}
