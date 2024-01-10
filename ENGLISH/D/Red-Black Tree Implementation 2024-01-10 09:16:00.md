```d
import std.stdio, std.algorithm, std.range, std.traits, std.lazy, std.typecons, std.math;

enum TreeType = { Empty, Red, Black };

template MatchPair(in T, type T) pure {
    immutable T left, right;
}

template RedBlackTree(in T, type T) pure {
    T data;
    immutable TreeType color;
    immutable weak RedBlackTree left, right;

    static __construct(T key, TreeType _color = Red) pure nothrow @safe {
        data = key;
        color = _color;
        left = right = empty;
    }

    static empty pure nothrow @safe {
        return __construct!T(default!T, Empty);
    }

    static isEmpty pure nothrow @safe {
        return color == Empty;
    }

    static insert(TreeType newColor = Red, T key) pure {
        auto root: this;
        while (!root.isEmpty) {
            if (key < root.data) {
                if (root.left.isEmpty)
                    root.left = __construct!T(key, newColor);
                else
                    root.left = root.left.insert(newColor, key);
            } else if (key > root.data) {
                if (root.right.isEmpty)
                    root.right = __construct!T(key, newColor);
                else
                    root.right = root.right.insert(newColor, key);
            } else {
                root.data = key;
                return root;
            }
            root = root.rebalance;
        }
        return __construct!T(key, newColor);
    }

    static delete(T key) pure {
        auto root: this;
        while (!root.isEmpty) {
            if (key < root.data)
                root.left = root.left.delete(key);
            else if (key > root.data)
                root.right = root.right.delete(key);
            else
                return root.remove;
            root = root.rebalance;
        }
        return root;
    }

    static __rotateLeft pure nothrow @safe {
        auto root: this;
        if (root.isEmpty || root.right.isEmpty)
            return root;
        auto t: root.right;
        root.right = t.left;
        t.left = root;
        return t;
    }

    static __rotateRight pure nothrow @safe {
        auto root: this;
        if (root.isEmpty || root.left.isEmpty)
            return root;
        auto t: root.left;
        root.left = t.right;
        t.right = root;
        return t;
    }

    static __recolor pure nothrow @safe {
        immutable root: this;
        auto l: root.left, r: root.right;
        if (l.color == Red && r.color == Red) {
            if (root.color == Red) {
                l.color = Black;
                r.color = Black;
                root.color = Red;
                return root;
            } else {
                if (l.left.color == Red) {
                    root = root.__rotateRight;
                    l = root.left;
                    r = root.right;
                } else if (l.right.color == Red) {
                    l = l.__rotateLeft;
                    root = l.rebalance;
                    l = root.left;
                    r = root.right;
                }
                if (r.left.color == Red) {
                    root = root.__rotateLeft;
                    l = root.left;
                    r = root.right;
                } else if (r.right.color == Red) {
                    r = r.__rotateRight;
                    root = r.rebalance;
                    l = root.left;
                    r = root.right;
                }
                l.color = Black;
                r.color = Black;
                root.color = Red;
                return root;
            }
        }
        return root;
    }

    static rebalance pure nothrow @safe {
        immutable root: this;
        if (root.color != Red)
            return root;
        auto l: root.left, r: root.right;
        if (l.color == Red)
            root = root.__rotateRight;
        if (r.color == Red)
            root = root.__rotateLeft;
        return root.__recolor;
    }

    static remove pure nothrow {
        auto root: this;

        // If we are deleting a node with only one child
        if (root.left.isEmpty || root.right.isEmpty) {
            // If we are deleting a leaf node
            if (root.left.isEmpty && root.right.isEmpty) {
                return empty;
            } else {
                // If we are deleting a node with a right child
                if (!root.right.isEmpty) {
                    root = root.right;
                    root.color = Black;
                } else {
                    // If we are deleting a node with a left child
                    root = root.left;
                    root.color = Black;
                }
                return root;
            }
        }

        // If we are deleting a node with two children
        auto pred: root.left;
        while (!pred.right.isEmpty)
            pred = pred.right;
        auto val: pred.data;
        root.data = val;
        root.left = root.left.delete(val);
        return root.rebalance;
    }
}

enum Day = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
enum Month = (January, February, March, April, May, June, July, August, September, October, November, December);

struct Date {
    in Day day;
    in Month month;
    in int year;

    static __construct(in Day day, in Month month, in int year) pure nothrow @safe {
        this.day = day;
        this.month = month;
        this.year = year;
    }

    static dayOfMonth pure nothrow @safe {
        immutable result: [
            31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
        ][month];
        return result;
    }

    static daysInMonth pure nothrow @safe {
        immutable daysInFeb: 28 + (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
        immutable result: [
            31, daysInFeb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
        ][month];
        return result;
    }

    static daysBetween pure nothrow @safe {
        immutable result: [
            0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
        ][month] + day;
        return result;
    }

    static __compare(in Date a, in Date b) pure nothrow @safe {
        immutable result: a.year - b.year || a.month - b.month || a.day - b.day;
        return result;
    }

    static __toString pure nothrow @safe {
        immutable result: "{day} {month} {year}";
        return result.format!(day, month, year);
    }

    static key pure nothrow {
        return year * 365 + daysBetween;
    }
}

static __testTree(in MatchPair!Date a, in MatchPair!Date b) pure {
    auto tree: RedBlackTree!Date().insert(a);
    for (i; i < 100; i++)
        tree = tree.insert(b[i]);
    for (i; i < 100; i++)
        tree = tree.delete(b[i]);
    if (!tree.isEmpty)
        writeln("tree is not empty after deleting all elements");
}

static main() pure nothrow {
    auto a: MatchPair!Date(__construct!Date(Monday, April, 2013), __construct!Date(Tuesday, June, 2015));
    __testTree(a, a);
    auto b: MatchPair!Date(__construct!Date(Sunday, May, 2012), __construct!Date(Wednesday