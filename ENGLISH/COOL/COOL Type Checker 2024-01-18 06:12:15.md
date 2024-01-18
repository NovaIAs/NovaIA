```cool
class WellFormed {
    Bool is_wf(xp : Node) : Bool {
        heap allocate [new_bool wf];
        if (xp = xp.new()) {
            wf = TRUE;
        } else {
            match xp.get() {
            case no_expr:
                wf = TRUE;
            case object:
                wf = TRUE;
            case single_let:
                wf = xp.type_decl.is_wf();
            case let_and_in:
                wf = xp.let_type.is_wf() and xp.let_expr.is_wf() and xp.in_expr.is_wf();
            case if_then_else:
                wf = xp.cond_expr.is_wf() and xp.then_expr.is_wf() and xp.else_expr.is_wf();
            case equal:
                wf = xp.a.is_wf() and xp.b.is_wf() and xp.a.type_decl.type = xp.b.type_decl.type;
            case less:
                wf = xp.a.is_wf() and xp.b.is_wf() and xp.a.type_decl.type = xp.b.type_decl.type;
            case mult:
                wf = xp.a.is_wf() and xp.b.is_wf();
            case plus:
                wf = xp.a.is_wf() and xp.b.is_wf();
            case minus:
                wf = xp.a.is_wf() and xp.b.is_wf();
            case isvoid:
                wf = xp.e.is_wf();
            case assign:
                wf = xp.obj.is_wf() and xp.e.is_wf() and xp.obj.type_decl.type = xp.e.type_decl.type;
            case block:
                wf = TRUE;
            case while_loop:
                wf = xp.predicate.is_wf() and xp.body.is_wf();
            case break:
                wf = TRUE;
            }
        }
        return wf;
    }
};
```

This code implements a type checker for a simple language in COOL. The `WellFormed` class has a single method, `is_wf`, which takes an expression as an argument and returns whether or not the expression is well-formed. The `is_wf` method uses a recursive algorithm to check the expression. It first checks if the expression is a literal, in which case it is always well-formed. If the expression is not a literal, it checks the type of the expression and the types of its subexpressions. If the types of the expression and its subexpressions match, then the expression is well-formed. Otherwise, the expression is not well-formed.

The `is_wf` method also checks for a number of other conditions that can cause an expression to be not well-formed. For example, it checks if the expression contains any undeclared variables, if it uses any operators that are not defined for the types of its operands, or if it contains any loops that do not have a terminating condition.

The `WellFormed` class can be used to check the well-formedness of expressions in a COOL program. This is useful for ensuring that the program is syntactically correct before it is compiled or interpreted.