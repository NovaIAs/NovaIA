```
import std.stdio, std.range, std.algorithm;
import std.ascii, std.ctype, std.traits;
import std.string, std.algorithm.ranges, std.container, std.bigint;
import std.conv, std.regex, std.exception, std.conv.bigint;

class StringSplitter
{
    private algo: Algo;
    private regex: Regex;

    enum Algo
    {
        split,
        rsplit,
        splitr,
    }

    this(...) pure nothrow @safe
    {
        // If the first argument is either a string ending in "r" or a regex, then
        // use the regex splitter, otherwise use the normal char splitter.
        if(string.length(arguments[0]) > 0 && arguments[0][string.length(arguments[0]) - 1] == 'r' || arguments[0] is Regex)
        {
            algo = Algo.splitr;
            regex = (Regex)arguments[0];
        }
        else
            regex = ~arguments[0].toRegex();
    }

    immutable(string[]) split(in string s) pure nothrow @safe
    {
        if(algo == Algo.split)
            return s.splitRegex(regex);
        else
            return s.matchRegex(regex);
    }

    immutable(string[]) rsplit(in string s) pure nothrow @safe
    {
        if(algo == Algo.split)
            return s.rsplitRegex(regex);
        else
            return s.rmatchRegex(regex);
    }

    immutable(string[]) splitr(in string s) pure nothrow @safe
    {
        if(algo == Algo.split)
            return s.splitRegex(regex);
        else if(algo == Algo.rsplit)
            return s.rsplitRegex(regex);
        else
            return s.regexMatchAll(regex);
    }

    immutable(string[]) split(...) pure nothrow @safe
    {
        if(arguments.length == 2 && arguments[1] is string)
        {
            if(algo == Algo.split)
                return arguments[0].split(arguments[1]);
            else if(algo == Algo.rsplit)
                return arguments[0].rsplit(arguments[1]);
            else
                return arguments[0].splitRegex(arguments[1]);
        }
        else
        {
            if(arguments.length != 0 && arguments[0] is StringSplitter)
                return arguments[0].split();
            else
                return this.split("");
        }
    }

    immutable(string[]) rsplit(...) pure nothrow @safe
    {
        if(arguments.length == 2 && arguments[1] is string)
        {
            if(algo == Algo.split)
                return arguments[0].rsplit(arguments[1]);
            else if(algo == Algo.rsplit)
                return arguments[0].split(arguments[1]);
            else
                return arguments[0].rsplitRegex(arguments[1]);
        }
        else
        {
            if(arguments.length != 0 && arguments[0] is StringSplitter)
                return arguments[0].rsplit();
            else
                return this.rsplit("");
        }
    }

    immutable(string[]) splitr(...) pure nothrow @safe
    {
        if(arguments.length == 2 && arguments[1] is string)
        {
            if(algo == Algo.split)
                return arguments[0].splitRegex(arguments[1]);
            else if(algo == Algo.rsplit)
                return arguments[0].rsplitRegex(arguments[1]);
            else
                return arguments[0].regexMatchAll(arguments[1]);
        }
        else
        {
            if(arguments.length != 0 && arguments[0] is StringSplitter)
                return arguments[0].splitr();
            else
                return this.splitr("");
        }
    }

    immutable(string[]) split(in Lazy!string s) pure nothrow @safe
    {
        if(algo == Algo.split)
            return s.value.splitRegex(regex);
        else
            return s.value.matchRegex(regex);
    }

    immutable(string[]) rsplit(in Lazy!string s) pure nothrow @safe
    {
        if(algo == Algo.split)
            return s.value.rsplitRegex(regex);
        else
            return s.value.rmatchRegex(regex);
    }

    immutable(string[]) splitr(in Lazy!string s) pure nothrow @safe
    {
        if(algo == Algo.split)
            return s.value.splitRegex(regex);
        else if(algo == Algo.rsplit)
            return s.value.rsplitRegex(regex);
        else
            return s.value.regexMatchAll(regex);
    }
}

enum SplitType
{
    split,
    rsplit,
    splitr,
}

class SplitStream
{
    private Splitter!ss;
    private in source;

    this(in in LazyStream!source, in SplitType type) @safe
    {
        this(source, type, "");
    }

    this(in LazyStream!source, in SplitType type, in string delimiter) @safe
    {
        this(new StringSplitter(delimiter), source, type);
    }

    this(in Splitter!ss, in LazyStream!source, in SplitType type) @safe
    {
        this.ss = ss;
        this.source = source;
        if(type != SplitType.splitr)
            ss.algo = type;
    }

    immutable(string[]) next(in int n) pure nothrow @nogc
    {
        if(n <= 0 || n == null)
            return [];

        immutable(string[]) r = [];
        while(n--)
        {
            immutable(string) s = source.next();
            if(s is null || s.length == 0)
                continue;

            r ~= ss.split(s);
        }
        return r;
    }

    immutable(string)[] all() pure nothrow @nogc
    {
        immutable(string[]) r = [];
        foreach(immutable(string) s; source)
            r ~= ss.split(s);
        return r;
    }
}

final main() @nogc
{
    // Split the string by the newline character.
    immutable(SplitStream!ss) s = new SplitStream("\n");

    // Print the first 10 lines of the input.
    foreach(immutable(string) line; s.next(10))
        writeln(line);

    // Split the string by the comma character.
    s = new SplitStream(",", ",");

    // Print the first 10 lines of the input.
    foreach(immutable(string) line; s.next(10))
        writeln(line);

    // Split the string by the comma character, but this time using the "splitr" mode.
    // This will produce an array of arrays, where each inner array contains all the
    // matches of the regex in the string.
    s = new SplitStream(",", ",", SplitType.splitr);

    // Print the first 10 lines of the input.
    foreach(immutable(string) line; s.next(10))
        writeln(line);

    immutable(StringSplitter!ss) regex = new StringSplitter("\\s+");
    immutable(string[]) arr = "hello world, how are you?".splitRegex(regex);
    foreach(immutable(string) s; arr)
        writeln(s);
}
```

This code defines a class called `StringSplitter` that can be used to split a string into an array of substrings. The class has three methods: `split()`, `rsplit()`, and `splitr()`. The `split()` method splits the string on the first occurrence of the specified delimiter, the `rsplit()` method splits the string on the last occurrence of the specified delimiter, and the `splitr()` method splits the string on all occurrences of the specified regular expression.

The class also defines a class called `SplitStream` that can be used to iterate over a stream of strings and split each string into an array of substrings. The class has two constructors: one that takes a `LazyStream!source` and a `SplitType` value, and one that takes a `Splitter!ss`, a `LazyStream!source`, and a `SplitType` value. The `next()` method returns an array of substrings, and the `all()` method returns an array of all the substrings in the stream.

The final main() function demonstrates how to use the `StringSplitter` and `SplitStream` classes. It first splits a string on the newline character and prints the first 10 lines of the input. It then splits a string on the comma character and prints the first 10 lines of the input. Finally, it splits a string on the comma character using the "splitr" mode and prints the first 10 lines of the input.

The code also defines a variable called `regex` that is an instance of the `StringSplitter` class. The variable is used to split the string "hello world, how are you?" on the whitespace character. The resulting array of substrings is printed to the console.