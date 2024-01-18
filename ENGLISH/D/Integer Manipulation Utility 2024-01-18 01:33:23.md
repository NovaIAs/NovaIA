```d
import std.algorithm, std.bigint, std.stdio, std.string, std.traits, std.typecons, std.variant;
import std.algorithm : swap;
import std.conv : format, reverse;
import std.env;
import std.exception : throw, tryCatch;
import std.typecons : uint32, uint64;
import typedPointer : sizeof, typedPointer;
import std.string.algorithm : toupper;
import std.stdio.printf;

string version = "0.1.0";

typedef enum
{
    kError,
    kHelp,
    kReverse,
    kToUint32,
    kToInt64,
    kToFloat,
} AppMode;

class AppOptions
{
public:

    // Using @init guarantees that options will be initialized
    // before the constructor runs. It can't be omitted, since
    // the constructor referenes the _options field.
    @init (void) { _options = []; }

    // A field with isPublic must an integer or pointer,
    // and must be named _[name] for a [name] switch to work.
    immutable static uint _options;
    static bool get isPublic { @safe return true; }
    static bool get isSwitch { @safe return true; }
    static bool get isSingle { @safe return true; }

    /// Parse command lime arguments.
    void parseArgs(string[] args) pure nothrow @safe
    {
        foreach (i, arg; args)
        {
            switch (arg)
            {
                case "-h":
                case "--help":
                    _options |= kHelp;
                    break;

                case "-r":
                case "--reverse":
                    _options |= kReverse;
                    break;

                case "-uint32":
                    _options |= kToUint32;
                    break;

                case "-int64":
                    _options |= kToInt64;
                    break;

                case "-float":
                    _options |= kToFloat;
                    break;

                default :
                    break;
            }
        }
    }

    /// Return a string from options.
    string toString() pure nothrow @safe
    {
        string s;
        if (_options & kHelp)
            s ~ "-h, --help";
        if (_options & kReverse)
            s ~ "-r, --reverse";
        if (_options & kToUint32)
            s ~ "-uint32";
        if (_options & kToInt64)
            s ~ "-int64";
        if (_options & kToFloat)
            s ~ "-float";
        return s;
    }

    /// Help message.
    void showHelp() pure nothrow @safe
    {
        stdio.writefln("USAGE: %s [OPTIONS] int{32|64|float}\n"
                       "\nOPTIONS:\n"
                       "\t-h, --help\t\t\tDisplay this help message.\n"
                       "\t-r, --reverse\t\t\tReverse the bits of the integer.\n"
                       "\t-uint32\t\t\t\tConvert the integer to uint32.\n"
                       "\t-int64\t\t\t\tConvert the integer to int64.\n"
                       "\t-float\t\t\t\tConvert the integer to a float.",
                       env.get("argv", "").ptr);
        //env.exit(0);
    }

    /// Reverse bits in int.
    int reverseBits(int x) pure nothrow @safe
    {
        int result = 0;
        int i = sizeof!int - 1;
        while (i >= 0)
        {
            int bit = (x >> i) & 1;
            result = (result << 1) + bit;
            i--;
        }
        return result;
    }

    /// Return a variant containing a uint32 or an int64.
    Variant toInteger(string s) pure nothrow @safe
    {
        Variant result;
        immutable Uint64 limit;
        switch (_options & (kToUint32 | kToInt64))
        {
            case kToUint32:
                limit = uint32.max;
                break;

            case kToInt64:
                limit = int64.max;
                break;

            default:
                return null;
        }

        uint64 num = 0ULL;
        immutable Tuple!char digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
        foreach (i, ch; toupper(s))
            num *= 10ULL;
            if (ch in digits)
                num += ch - '0';
            else
                return null;

        if (num > limit)
            return null;

        switch (_options & (kToUint32 | kToInt64))
        {
            case kToUint32:
                result = num;
                break;

            case kToInt64:
                if (num & 0x8000000000000000ULL)
                    return null;
                else
                    result = num - 0x8000000000000000ULL;
                break;
        }

        return result;
    }

    /// Convert a Variant to float.
    float toFloat(Variant v) pure nothrow @safe
    {
        switch (v.type)
        {
            case int64.type:
                return v.cast(int64).to!float;

            case uint32.type:
                return v.cast(uint32).to!float;

            default:
                return v.cast(int).to!float;
        }
    }

    /// Turn arguments into numbers.
    Variant run(string[] args) pure nothrow @safe
    {
        if (args.length == 0)
            return null;
        if (env.get("argv", "").ptr == "-")
            return readln.to!Variant;
        else
            return toInteger(args[0]);
    }
}

void main() pure nothrow @safe
{
    static AppOptions appOptions;
    static string[] args;
    immutable version = version;

    try
    {
        args = env.get("argv");
        appOptions.parseArgs(args);

        if (appOptions._options & kHelp)
            appOptions.showHelp();
        else
        {
            stdio.writefln("Version: %s", version);
            stdio.writefln("\nOptions : %s", appOptions.toString());

            Variant v = appOptions.run(args);
            immutable int localOptions = appOptions._options; // Prevent alias problem

            if (!v.isNull)
            {
                if (localOptions & kReverse)
                    stdio.writefln("Reversed bits: %d", appOptions.reverseBits(v.cast(int)));

                if (localOptions & kToFloat)
                    stdio.writefln("Float: %f", appOptions.toFloat(v));

                stdio.writefln("Int{32,64}: %s", format("%s", v));
            }
            else
            {
                if (localOptions & kHelp)
                    return;
                stdio.writefln("Error: Incorrect input.");
            }
        }
    }
    catch (Exception e)
    {
        stdio.writefln("Error: %s", e.msg);
        //env.exit(1);
    }
}
```

This code is a command-line utility that takes an integer as input and performs various operations on it, such as reversing its bits, converting it to a different integer type, or converting it to a floating-point number. The code also provides help and error messages.

The code is organized into several classes and functions:

* The `AppOptions` class contains the command-line options and methods for parsing them and performing the various operations on the input integer.
* The `main()` function is the entry point for the program. It parses the command-line arguments, creates an instance of the `AppOptions` class, and calls the `run()` method to perform the desired operations on the input integer.
* The `reverseBits()` function reverses the bits in an integer.
* The `toInteger()` function converts a string to an integer of the specified type (uint32 or int64).
* The `toFloat()` function converts a variant containing an integer or a floating-point number to a floating-point number.
* The `run()` function performs the desired operations on the input integer and returns a variant containing the result.

The code demonstrates several advanced features of the D programming language, such as:

* The use of variants to represent values of different types.
* The use of generics to define functions that can work with different types of data.
* The use of switch statements to handle multiple cases.
* The use of tuples to represent collections of related data.
* The use of exceptions to handle errors.