```d
import std.stdio, std.traits, std.algorithm, std.string;
import std.exception, std.range, std.traits.opAssign;
import std.algorithm.find;
import std.container.hashtable;
import std.traits.assign;

template T is(type) pure nothrow {
    immutable T a;
    void this(T a) pure nothrow { @safe this.a = a; }
    immutable pure nothrow T get() const pure nothrow { return a; }
}

template T is(type) {
    immutable T a;
    void this(T a) pure nothrow { @safe this.a = a; }
    immutable pure nothrow T get() const pure nothrow { return a; }
}

template T is(type) pure nothrow {
    immutable pure nothrow T get() const pure nothrow { return T(); }
}

immutable pure T opT(T t) pure nothrow { return t; }

template T is(type) {
    immutable static pure nothrow T defaultT = opT(T());
    immutable static pure nothrow T nullT = null;
    immutable static pure nothrow T emptyT = T();
    immutable static pure nothrow T badT = null;

    immutable static pure nothrow T firstT = opT(T());
    immutable static pure nothrow T lastT = opT(T());
    immutable static pure nothrow T minT = opT(T());
    immutable static pure nothrow T maxT = opT(T());
}

immutable T null is(T) pure nothrow { return T.nullT; }
immutable T empty is(T) pure nothrow { return T.emptyT; }
immutable T bad is(T) pure nothrow { return T.badT; }

template T is(type) {
    immutable static pure nothrow T defaultT = opT(T());
    immutable static pure nothrow T nullT = null;
    immutable static pure nothrow T emptyT = T();
    immutable static pure nothrow T badT = null;

    immutable static pure nothrow T firstT = opT(T());
    immutable static pure nothrow T lastT = opT(T());
    immutable static pure nothrow T minT = opT(T());
    immutable static pure nothrow T maxT = opT(T());

    immutable pure nothrow T prev(T t) pure nothrow {
        if (t == firstT) return lastT;
        return t - 1;
    }

    immutable pure nothrow T next(T t) pure nothrow {
        if (t == lastT) return firstT;
        return t + 1;
    }
}

template T is(type) {
    immutable static pure nothrow T defaultT = opT(T());
    immutable static pure nothrow T nullT = null;
    immutable static pure nothrow T emptyT = T();
    immutable static pure nothrow T badT = null;

    immutable static pure nothrow T firstT = opT(T());
    immutable static pure nothrow T lastT = opT(T());
    immutable static pure nothrow T minT = opT(T());
    immutable static pure nothrow T maxT = opT(T());

    immutable static pure nothrow T incr(T t) pure nothrow { return t + 1; }
    immutable static pure nothrow T decr(T t) pure nothrow { return t - 1; }

    immutable static pure nothrow T prev(T t) pure nothrow {
        if (t == firstT) return lastT;
        return decr(t);
    }

    immutable static pure nothrow T next(T t) pure nothrow {
        if (t == lastT) return firstT;
        return incr(t);
    }
}

immutable T is(T) pure nothrow { return T.defaultT; }
immutable T first is(T) pure nothrow { return T.firstT; }
immutable T last is(T) pure nothrow { return T.lastT; }
immutable T min is(T) pure nothrow { return T.minT; }
immutable T max is(T) pure nothrow { return T.maxT; }

template T is(enum) {
    immutable static pure nothrow T defaultT = T.first;
    immutable static pure nothrow T nullT = T.null;
    immutable static pure nothrow T emptyT = T.null;
    immutable static pure nothrow T badT = T.null;

    immutable static pure nothrow T firstT = T.first;
    immutable static pure nothrow T lastT = T.last;
    immutable static pure nothrow T minT = T.first;
    immutable static pure nothrow T maxT = T.last;
}

template T is(enum) {
    immutable static pure nothrow T defaultT = T.first;
    immutable static pure nothrow T nullT = T.null;
    immutable static pure nothrow T emptyT = T.null;
    immutable static pure nothrow T badT = T.null;

    immutable static pure nothrow T firstT = T.first;
    immutable static pure nothrow T lastT = T.last;
    immutable static pure nothrow T minT = T.first;
    immutable static pure nothrow T maxT = T.last;

    immutable pure nothrow T prev(T t) pure nothrow {
        if (t == firstT) return lastT;
        foreach (T i; T.first .. T.last) {
            if (i == t) return i - 1;
        }
        throw new Exception("unreachable");
    }

    immutable pure nothrow T next(T t) pure nothrow {
        foreach (T i; T.first .. T.last) {
            if (i == t) return i + 1;
        }
        throw new Exception("unreachable");
    }
}

template T is(enum) {
    immutable static pure nothrow T defaultT = T.first;
    immutable static pure nothrow T nullT = T.null;
    immutable static pure nothrow T emptyT = T.null;
    immutable static pure nothrow T badT = T.null;

    immutable static pure nothrow T firstT = T.first;
    immutable static pure nothrow T lastT = T.last;
    immutable static pure nothrow T minT = T.first;
    immutable static pure nothrow T maxT = T.last;

    immutable static pure nothrow T incr(T t) pure nothrow {
        foreach (T i; T.first .. T.last) {
            if (i == t) return i + 1;
        }
        throw new Exception("unreachable");
    }

    immutable static pure nothrow T decr(T t) pure nothrow {
        if (t == firstT) return lastT;
        foreach (T i; T.first .. T.last) {
            if (i == t) return i - 1;
        }
        throw new Exception("unreachable");
    }

    immutable pure nothrow T prev(T t) pure nothrow {
        if (t == firstT) return lastT;
        return decr(t);
    }

    immutable pure nothrow T next(T t) pure nothrow {
        return incr(t);
    }
}

template T is(struct) {
    immutable static pure nothrow T defaultT = opT(T());
    immutable static pure nothrow T nullT = null;
    immutable static pure nothrow T emptyT = T();
    immutable static pure nothrow T badT = null;

    immutable static pure nothrow T firstT = opT(T());
    immutable static pure nothrow T lastT = opT(T());
    immutable static pure nothrow T minT = opT(T());
    immutable static pure nothrow T maxT = opT(T());
}

template T is(struct) {
    immutable static pure nothrow T defaultT = opT(T());
    immutable static pure nothrow T nullT = null;
    immutable static pure nothrow T emptyT = T();
    immutable static pure nothrow T badT = null;

    immutable static pure nothrow T firstT = opT(T());
    immutable static pure nothrow T lastT = opT(T());
    immutable static pure nothrow T minT = opT(T());
    immutable static pure nothrow T maxT = opT(T());

    immutable pure nothrow T less(T a, T b) pure nothrow { return a < b; }
}

template T is(struct) {
    immutable static pure nothrow T defaultT = opT(T());
    immutable static pure nothrow T nullT = null;
    immutable static pure nothrow T emptyT = T();
    immutable static pure nothrow T badT = null;

