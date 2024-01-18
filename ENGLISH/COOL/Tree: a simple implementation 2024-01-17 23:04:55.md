```cool

-- Module: Tree
--

class Tree =
    insert() : Tree;
    remove() : Tree;
    find()   : Tree;
    split()   : Tree*;

class NullTree extends Tree =
    new()
        : NullTree { }.

    insert(x: Int) : Tree
        : new() Node { left: self, right: self, count: 0, value: x };

    remove(_: Int) : Tree : self;
    find(_: Int)   : Tree : self;

    split(_: Int)    : Tree* : (self, self);

class Node extends Tree =
    new(left: Tree, right: Tree, count: Int, value: Int)
        : Node { left, right, count, value }.

    insert(x: Int) : Tree
        : let t := if value <= x then right else left fi
              in t <- t.insert(x)
        fi;
           new() Node { left, right, count + 1, value };

    remove(x: Int) : Tree
        : let left  := left.remove(x),
              right := right.remove(x)
              in if count - 1 = 0 then
                    case (left, right) of
                        (NullTree, NullTree) => NullTree
                        (_, _)               => left.split(x).left
                    fi
               else new() Node { left, right, count - 1, value }
               fi
        fi;

    find(x: Int) : Tree
        : let t := if value <= x then right else left fi
              in if not t.is_null() then t.find(x) else self fi
        fi;

    split(x: Int) : Tree*
        : let left  := left.split(x),
              right := right.split(x)
              in case (left, right) of
                     ((l1, l2), (r1, r2)) => ((l1.insert(value), r1), (l2, r2.insert(value)))
                 fi
        fi;
```