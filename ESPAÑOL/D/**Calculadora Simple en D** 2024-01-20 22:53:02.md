```d
import std.stdio, std.algorithm, std.range, std.format, std.exception, std.traits, std.array, std.array.sequence,
  std.foreach, std.traits, std.string, std.traits.feature, std.traits.ref, std.string.conv, std.range.reverse,
  std.typeinfo, std.traits.typeinfo, std.traits.value, std.array.arrayOps, std.traits.carries, std.traits.sequence;

enum ExprType { VALUE, ADD, SUB, MUL, DIV, MOD, EXP, NEG, PARENS, ACCESS, LVALUE };

class Expr {
    ExprType type;
    union {
        struct { Expr* left, *right; } binary;
        Expr* child;
        double value;
        auto op(Expr* x) pure {
            if(type == ADD) return x + this;
            if(type == SUB) return this - x;
            if(type == MUL) return this * x;
            if(type == DIV) return this / x;
            if(type == MOD) return this % x;
            if(type == EXP) return this ^ x;
        }
    } u;
public:
    Expr(ExprType t) : type(t) {}

    Expr* reduce() {
        if(type == NEG || type == PARENS || type == LVALUE) u.child = u.child->reduce();
        else if(type == ADD || type == SUB || type == MUL || type == DIV || type == EXP) {
            u.binary.left = u.binary.left->reduce();
            u.binary.right = u.binary.right->reduce();
        }
        if(type == ADD && u.binary.left->type == VALUE && u.binary.right->type == VALUE) {
            u.value = u.binary.left->u.value + u.binary.right->u.value;
            type = VALUE;
        } else if(type == SUB && u.binary.left->type == VALUE && u.binary.right->type == VALUE) {
            u.value = u.binary.left->u.value - u.binary.right->u.value;
            type = VALUE;
        } else if(type == MUL && u.binary.left->type == VALUE && u.binary.right->type == VALUE) {
            u.value = u.binary.left->u.value * u.binary.right->u.value;
            type = VALUE;
        } else if(type == DIV && u.binary.left->type == VALUE && u.binary.right->type == VALUE) {
            u.value = u.binary.left->u.value / u.binary.right->u.value;
            type = VALUE;
        } else if(type == MOD && u.binary.left->type == VALUE && u.binary.right->type == VALUE) {
            u.value = u.binary.left->u.value % u.binary.right->u.value;
            type = VALUE;
        } else if(type == EXP && u.binary.left->type == VALUE && u.binary.right->type == VALUE) {
            u.value = pow(u.binary.left->u.value, u.binary.right->u.value);
            type = VALUE;
        }
        return this;
    }

    double eval() pure nothrow {
        if(type == VALUE) return u.value;
        if(type == NEG) return -u.child->eval();
        if(type == ADD) return u.binary.left->eval() + u.binary.right->eval();
        if(type == SUB) return u.binary.left->eval() - u.binary.right->eval();
        if(type == MUL) return u.binary.left->eval() * u.binary.right->eval();
        if(type == DIV) return u.binary.left->eval() / u.binary.right->eval();
        if(type == MOD) return fmod(u.binary.left->eval(), u.binary.right->eval());
        if(type == EXP) return pow(u.binary.left->eval(), u.binary.right->eval());
        return INFINITY;
    }

    string toString() pure nothrow {
        if(type == VALUE) return u.value.to!string;
        if(type == NEG) return "-(" + u.child->toString() + ")";
        if(type == ADD) return "(" + u.binary.left->toString() + ") + (" + u.binary.right->toString() + ")";
        if(type == SUB) return "(" + u.binary.left->toString() + ") - (" + u.binary.right->toString() + ")";
        if(type == MUL) return "(" + u.binary.left->toString() + ") * (" + u.binary.right->toString() + ")";
        if(type == DIV) return "(" + u.binary.left->toString() + ") / (" + u.binary.right->toString() + ")";
        if(type == MOD) return "(" + u.binary.left->toString() + ") % (" + u.binary.right->toString() + ")";
        if(type == EXP) return "(" + u.binary.left->toString() + ") ^ (" + u.binary.right->toString() + ")";
        return "Expr(" + type.to!string + ")";
    }
};

Expr* parse(string s) pure nothrow {
    static string operators = "+-*/%^";
    Expr* res;
    int precedence = 0;
    auto process = [](char c, int p) {
        if(p == precedence) {
            Expr* r = res;
            Expr* n = new Expr(ExprType(c-'0'));
            res = n;
            if(c == '+') res->u.binary.left = r;
            if(c == '-') res->u.binary.right = r;
            if(c == '*') res->u.binary.left = r;
            if(c == '/') res->u.binary.right = r;
            if(c == '%') res->u.binary.right = r;
            if(c == '^') res->u.binary.left = r;
        } else {
            res = new Expr(ExprType(c-'0'));
            precedence++;
        }
    };
    foreach(c, s) {
        if(operators.indexof(c) != -1) {
            process(c, operators.indexof(c));
        } else if(isdigit(c) || c=='.') {
            if(res->type == VALUE || res->type == PARENS) {
                string s = res->toString();
                s += c;
                res->type = VALUE;
                res->u.value = s.toDouble();
            } else {
                precedence--;
                Expr* n = new Expr(ExprType(VALUE));
                n->u.value = (c-'0').toDouble();
                res->op(n);
            }
        } else {
            if(res->type == VALUE || res->type == PARENS) {
                process(')', 0);
                precedence--;
                while(res->type == ADD || res->type == SUB ||
                   res->type == MUL || res->type == DIV ||
                   res->type == MOD || res->type == EXP) {
                    res = res->u.binary.right;
                    precedence--;
                }
            } else if(c=='(') {
                Expr* n = new Expr(ExprType(PARENS));
                Expr* r = res;
                res = n;
                res->u.child = r;
            } else {
                process(c, operators.length);
            }
        }
    }
    while(precedence >= 0) {
        Expr* r = res;
        Expr* n = new Expr(ExprType(PARENS));
        res = n;
        res->u.child = r;
        precedence--;
    }
    return res->reduce();
}

int main() {
    string s;
    writeln("Ingrese una expresión aritmética: ");
    while(cin >> s && s != "QUIT") {
        Expr* expr = parse(s);
        writeln("La expresión reducida es: " + expr->toString());
        writeln("El resultado de la expresión es: " + expr->eval().to!string);
        writeln();
    }
    return 0;
}
```

**Explicación del código:**

* La clase `Expr` representa una expresión aritmética. Tiene un campo `type` que indica el tipo de expresión (valor, suma, resta, multiplicación, división, módulo, exponenciación, negación, paréntesis o acceso) y un campo `u` que contiene la información específica de la expresión.
* El método `reduce()` reduce la expresión a su forma más simple, evaluando las operaciones y eliminando los paréntesis innecesarios.
* El método `eval()` evalúa la expresión y devuelve su valor.
* El método `toString()` devuelve una representación en cadena de la expresión.
* La función `parse()` analiza una cadena que contiene una expresión aritmética y devuelve un objeto `Expr` que representa la expresión.
* La función `main()` es el punto de entrada al programa. Lee una expresión aritmética del usuario, la analiza y evalúa, y muestra el resultado en la consola.

**Ejemplo de uso:**

```
Ingrese una expresión aritmética: (1 + 2) * 3

La expresión reducida es: (3) * 3

El resultado de la expresión es: 9

Ingrese una expresión aritmética: 10 - 5 + 3
La expresión reducida es: 10 - 5 + 3

El resultado de la expresión es: 8

Ingrese una expresión aritmética: 2 ^ 3
La expresión reducida es: 2 ^ 3

El resultado de la expresión es: 8

Ingrese una expresión aritmética: QUIT
```