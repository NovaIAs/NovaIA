```modula-2
MODULE Poly;
FROM FormatString IMPORT Format;
FROM FormatString IMPORT Fmt;
TYPE Sym = INTEGER;

TYPE Expr =
    Value OF REAL
  | Plus OF Expr, Expr
  | Minus OF Expr, Expr
  | Times OF Expr, Expr
  | Div OF Expr, Expr

VAR X: Sym := 1;

PROCEDURE ShowExpr(e: Expr);
BEGIN
    CASE e OF
       Value(v):
           Format(Fmt("%r", v))
       Plus(e1, e2):
           Format(Fmt("(%s + %s", [ShowExpr(e1), ShowExpr(e2)]));
       Minus(e1, e2):
           Format(Fmt("(%s - %s", [ShowExpr(e1), ShowExpr(e2)]));
       Times(e1, e2):
           Format(Fmt("(%s * %s", [ShowExpr(e1), ShowExpr(e2)]));
       Div(e1, e2):
           Format(Fmt("(%s / %s", [ShowExpr(e1), ShowExpr(e2)]))
    END;
END ShowExpr;

PROCEDURE Subst(e: Expr; v: Sym; s: REAL): Expr;
BEGIN
    CASE e OF
        Value(_):
            Value(s)
        Plus(e1, e2):
            Plus(Subst(e1, v, s), Subst(e2, v, s))
        Minus(e1, e2):
            Minus(Subst(e1, v, s), Subst(e2, v, s))
        Times(e1, e2):
            Times(Subst(e1, v, s), Subst(e2, v, s))
        Div(e1, e2):
            Div(Subst(e1, v, s), Subst(e2, v, s))
    END
END Subst;

PROCEDURE Derive(e: Expr; v: Sym): Expr;
BEGIN
    CASE e OF
        Value(_):
            Value(0)
        Plus(e1, e2):
            Plus(Derive(e1, v), Derive(e2, v))
        Minus(e1, e2):
            Minus(Derive(e1, v), Derive(e2, v))
        Times(e1, e2):
            Plus(Times(Derive(e1, v), e2), Times(Subst(e2, v, 1), Derive(e1, v)))
        Div(e1, e2):
            Div(Minus(Times(Derive(e1, v), e2), Times(Subst(e1, v, 1), Derive(e2, v))),
                Times(e2, e2))
    END
END Derive;

PROCEDURE Eval(e: Expr): REAL;
BEGIN
    CASE e OF
        Value(v):
            v
        Plus(e1, e2):
            Eval(e1) + Eval(e2)
        Minus(e1, e2):
            Eval(e1) - Eval(e2)
        Times(e1, e2):
            Eval(e1) * Eval(e2)
        Div(e1, e2):
            Eval(e1) / Eval(e2)
    END
END Eval;

PROCEDURE Main;
BEGIN
    ShowExpr(Times(Plus(Value(2), Value(3)), Value(4)));
    NL;
    ShowExpr(Subst(Times(Plus(Value(2), Value(3)), Value(4)), X, 5));
    NL;
    ShowExpr(Derive(Times(Plus(Value(2), Value(3)), Value(4)), X));
    NL;
    Format(Fmt("Value = %r\n", Eval(Times(Plus(Value(2), Value(3)), Value(4)))));
END Main.
```

This program evaluates mathematical expressions with a single variable `X` (and the value `X = 5`).

The `Expr` type defines the structure of the expressions that can be evaluated. It includes values, sums, differences, products, and divisions.

The `ShowExpr` procedure prints the expression in a human-readable format.

The `Subst` procedure substitutes a variable with a given value in an expression.

The `Derive` procedure computes the derivative of an expression with respect to a variable.

The `Eval` procedure evaluates the expression using the current value of `X`.

The `Main` procedure creates an expression, prints it, substitutes `X` with the value 5, prints the substituted expression, computes the derivative of the expression with respect to `X`, prints the derivative, and finally evaluates the expression.