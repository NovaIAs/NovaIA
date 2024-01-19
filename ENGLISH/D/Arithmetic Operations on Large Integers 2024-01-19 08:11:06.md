```d
import phobos;
import std.bigint;

T addOne = phobos.class(Array!(int)) {
    alias this T;
    public T operator +(T other) pure nothrow {
        T ret = T(this.len);
        int carry = 0;
        foreach (i, a; this) {
            auto b = other[i];
            auto sum = a + b + carry;
            carry = sum / 10;
            ret[i] = sum % 10;
        }
        if (carry > 0) {
            ret ~= carry;
        }
        ret;
    }

    public T operator + (int other) pure nothrow {
        T ret = T(this.len);
        foreach (i, a; this) {
            ret[i] = a;
        }
        int carry = other;
        foreach (i, a; ret) {
            auto sum = a + carry;
            carry = sum / 10;
            ret[i] = sum % 10;
        }
        if (carry > 0) {
            ret ~= carry;
        }
        ret;
    }

    public T operator - (T other) pure nothrow {
        T ret = T(this.len);
        int carry = 0;
        foreach (i, a; this) {
            auto b = other[i];
            auto diff = a - b - carry;
            if (diff < 0) {
                diff += 10;
                carry = 1;
            }
            else {
                carry = 0;
            }
            ret[i] = diff;
        }
        ret;
    }

    public T operator - (int other) pure nothrow {
        T ret = T(this.len);
        foreach (i, a; this) {
            ret[i] = a;
        }
        int carry = other;
        foreach (i, a; ret) {
            auto diff = a - carry;
            if (diff < 0) {
                diff += 10;
                carry = 1;
            }
            else {
                carry = 0;
            }
            ret[i] = diff;
        }
        ret;
    }

    public T operator * (T other) pure nothrow {
        T ret = T(this.len * other.len);
        foreach (i, a; this) {
            foreach (j, b; other) {
                ret[i * other.len + j] += a * b;
            }
        }
        int carry = 0;
        foreach (i, a; ret) {
            auto sum = a + carry;
            carry = sum / 10;
            ret[i] = sum % 10;
        }
        while (carry > 0) {
            ret ~= carry;
            carry = carry / 10;
        }
        ret;
    }

    public T operator / (int other) pure nothrow {
        T ret = T(this.len);
        int carry = 0;
        foreach (i, a; this) {
            auto quotient = (a + carry) / other;
            carry = (a + carry) % other;
            ret[i] = quotient;
        }
        ret;
    }

    public T operator % (int other) pure nothrow {
        T ret = T(this.len);
        int carry = 0;
        foreach (i, a; this) {
            carry = (a + carry) % other;
            ret[i] = carry;
        }
        ret;
    }

    public T operator << (int other) pure nothrow {
        T ret = T(this.len + other);
        ret[other] = 0;
        foreach (i, a; this) {
            ret[i + other] = a;
        }
        ret;
    }

    public T operator >> (int other) pure nothrow {
        T ret = T(this.len - other);
        foreach (i, a; ret) {
            ret[i] = this[i + other];
        }
        ret;
    }

    public bool operator < (T other) pure nothrow {
        foreach (i, a; this) {
            if (a < other[i]) {
                return true;
            }
            else if (a > other[i]) {
                return false;
            }
        }
        return false;
    }

    public bool operator <= (T other) pure nothrow {
        return (*this < other) || (*this == other);
    }

    public bool operator > (T other) pure nothrow {
        return other < *this;
    }

    public bool operator >= (T other) pure nothrow {
        return (*this > other) || (*this == other