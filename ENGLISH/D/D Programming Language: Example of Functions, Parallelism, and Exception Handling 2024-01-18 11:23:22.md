```d
import std.stdio, std.algorithm, std.traits;
import std.range, std.typecons, std.utf;
import std.parallelism, std.thread, std.exception;

void main() {
    auto array = [1, 2, 3, 4, 5];
    auto array1 = [1, 2, 3, 4, 5];

    // Functions and lambdas
    auto sum = [&](int x, int y) { return x + y; };
    auto odd = [&](int x) { return x & 1; };

    // Parallelism
    scope(Parallel) {
        immutable source = 0..100;
        foreach (i; source | parallel) {
            if (i % 10 == 0)
                writeln("Thread", i, "processing", i, "to", i + 9);
            scope(Parallel) {
                foreach (j; [i..i + 9]) {
                    immutable k = j;
                    immutable v = j;
                    thread(lambda!() {
                        auto result = v * v;
                        writeln("Thread", k, "computed", v, "*", v, "=", result);
                    });
                }
            }
        }
    }

    // Exceptions
    scope(Exception) {
        try {
            scope(Exception) {
                try {
                    throw 1;
                }
                catch (int e) {
                    throw new Exception("Wrapping exception", e);
                }
            }
            catch (Exception e) {
                writeln("Caught", e);
            }
        }
        catch (Exception e) {
            writeln("Caught", e);
        }
    }
}
```