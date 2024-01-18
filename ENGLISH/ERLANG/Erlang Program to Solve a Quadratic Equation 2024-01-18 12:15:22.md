```erlang
-module(quadratic_solver).
-export([solve/1, solve/2, solve/3]).

solve(A) ->
    solve(A, 0, 0).

solve(A, B) ->
    solve(A, B, 0).

solve(A, B, C) ->
    Discriminant = B * B - 4 * A * C,
    if
        Discriminant < 0 ->
            {error, "no real roots"};
        Discriminant == 0 ->
            {-B / (2 * A)};
        true ->
            SqrtDiscriminant = math:sqrt(Discriminant),
            {(-B + SqrtDiscriminant) / (2 * A),
             (-B - SqrtDiscriminant) / (2 * A)}
    end.
```

This code implements a function `solve/1`, `solve/2`, and `solve/3` that solves a quadratic equation of the form `Ax^2 + Bx + C = 0`.

The function `solve/1` takes the coefficient `A` as input and returns the solutions of the equation `Ax^2 = 0`. Since the equation has only one root, the function returns a single value.

The function `solve/2` takes the coefficients `A` and `B` as input and returns the solutions of the equation `Ax^2 + Bx = 0`. Since the equation has two roots, the function returns a tuple containing the two values.

The function `solve/3` takes the coefficients `A`, `B`, and `C` as input and returns the solutions of the equation `Ax^2 + Bx + C = 0`. Since the equation has two roots, the function returns a tuple containing the two values.

The function `solve/3` uses the discriminant to determine the number of real roots of the equation. If the discriminant is negative, the equation has no real roots. If the discriminant is zero, the equation has one real root. If the discriminant is positive, the equation has two real roots.

The function `solve/3` uses the quadratic formula to compute the roots of the equation. The quadratic formula is:

```
x = (-B ± √(B^2 - 4AC)) / 2A
```

The function `solve/3` uses the `math` module to compute the square root of the discriminant.

The function `solve/3` returns a tuple containing the two roots of the equation. If the equation has no real roots, the function returns the error tuple `{error, "no real roots"}`.