```d
import std.algorithm, std.stdio, std.string, std.traits;
import std.typecons;


string toHex(T)(in T value, in int digits = 2) pure nothrow @nogc @safe {
    string chars = "0123456789ABCDEF";
    string result = "";
    if (value != 0) {
        while (value > 0) {
            result = chars[value & 0xF] ~ result;
            value >>= 4;
        }
        result.truncate(min(digits, result.length));
    } else if (digits != 0)
        result = "0" ~ result;
    return result;
}

string toString(T)(in T value, in int radix = 10) pure nothrow @nogc @safe {
    if (radix < 2 || radix > 36)
        throw new Exception("Invalid radix.");
    string chars = "0123456789abcdefghijklmnopqrstuvwxyz";
    string result = "";
    if (value != 0) {
        while (value > 0) {
            result = chars[value % radix] ~ result;
            value /= radix;
        }
    } else
        result = "0";
    return result;
}

void testToHexString() {
    immutable ubyte[] values = [0x00, 0x01, 0x0A, 0x0F, 0x10, 0x20, 0x7F, 0x80,
                               0x81, 0xFF, 0x100, 0x101, 0x102, 0xFFFF, 0x10000,
                               0x10001, 0xFFFFFFFF, 0x100000000, 0x100000001];
    immutable string[] expected = ["00", "01", "0A", "0F", "10", "20", "7F", "80",
                                "81", "FF", "0100", "0101", "0102", "FFFF", "00010000",
                                "00010001", "FFFFFFFF", "0100000000", "0100000001"];

    foreach (i, value; values) {
        string hex = toHex(value);
        if (hex != expected[i]) {
            writefln("toHex(%u): expected %s, got %s", value, expected[i], hex);
            return;
        }
    }
    writefln("toHex tests passed");
}

void testToString() {
    immutable int64[] values = [0, 1, 10, 20, 100, 1000, 10000, 100000,
                             1000000, 10000000, 100000000, 1000000000];
    immutable string[] expected = ["0", "1", "10", "20", "100", "1000", "10000", "100000",
                                "1000000", "10000000", "100000000", "1000000000"];

    foreach (i, value; values) {
        string str = toString(value);
        if (str != expected[i]) {
            writefln("toString(%lld): expected %s, got %s", value, expected[i], str);
            return;
        }
    }
    writefln("toString tests passed");
}

void main() {
    testToHexString();
    testToString();
}
```

This code is a collection of functions that convert integers to strings in different formats. The `toHex()` function converts an integer to a hexadecimal string, while the `toString()` function converts an integer to a string in a specified radix (base).

The `toHex()` function uses the `chars` array to store the hexadecimal digits. It then loops through the integer, converting each digit to a hexadecimal digit and appending it to the result string.

The `toString()` function uses the `chars` array to store the digits in the specified radix. It then loops through the integer, converting each digit to a digit in the specified radix and appending it to the result string.

The `testToHexString()` and `testToString()` functions are used to test the `toHex()` and `toString()` functions, respectively. They print out the results of converting a series of integers to hexadecimal and decimal strings, and compare the results to the expected values.

The `main()` function calls the `testToHexString()` and `testToString()` functions to run the tests.