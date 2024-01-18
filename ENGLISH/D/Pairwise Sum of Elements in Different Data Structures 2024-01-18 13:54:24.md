```d
import std.stdio, std.algorithm, std.range, std.string;

struct Foo {
    int a, b;
}

alias MyPair = tuple(int, int);

auto pairwiseSum(T[](in T) tg) pure @nogc {
    foreach (auto e1, auto e2; pairs(tg)) {
        yield e1 + e2;
    }
}

int main() {
    auto foolist = [Foo(1, 2), Foo(3, 4), Foo(5, 6)];
    auto pairlist = [MyPair(1, 2), MyPair(3, 4), MyPair(5, 6)];
    writeln("Sum of Foo struct a and b fields:", pairwiseSum(foolist.map!(F(Foo) { F.a + F.b; })));
    writeln("Sum of MyPair struct a and b fields:", pairwiseSum(pairlist));
    writeln("Sum of vector a and b fields:", pairwiseSum([1, 2, 3, 4, 5, 6]));

    return 0;
}
```

Explanation:

1. We define a struct `Foo` with two integer fields `a` and `b`.

2. We define an alias `MyPair` as a tuple of two integers.

3. We define a pure function `pairwiseSum` that takes an array of elements of type `T` and returns a generator that yields the sum of each pair of elements in the array.

4. In the `main` function, we create a list of `Foo` structs, a list of `MyPair` structs, and a vector of integers.

5. We use the `pairwiseSum` function to calculate and print the sum of the `a` and `b` fields of the `Foo` structs, the `a` and `b` fields of the `MyPair` structs, and the elements of the vector of integers.

6. We return 0 to indicate successful execution of the program.

The code demonstrates the use of a pure function, a struct, an alias, and a generator in D. It also showcases the ability of D to work with different types of data structures in a concise and readable manner.