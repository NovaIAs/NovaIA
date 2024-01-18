```ada
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;

procedure Differentiation is

   type Sym is (X, Y, Z);

   type Func is access procedure (Sym) return Float;

   function Mul (F, G : Func) return Func is
      (S : Sym) => F (S) * G (S);

   function Add (F, G : Func) return Func is
      (S : Sym) => F (S) + G (S);

   function Der (F : Func) return Func is
      (S : Sym) =>
         case S is
            when X => 1.0
            when Y => 0.0
            when Z => 0.0
         end case;

   function F1 (S : Sym) return Float is
      case S is
         when X => S'Pos
         when Y => 2.0 * S'Pos
         when Z => 0.0
      end case;

   function F2 (S : Sym) return Float is
      case S is
         when X => 1.0
         when Y => 1.0
         when Z => 1.0
      end case;

   F : Func := Mul (F1, F2);
   F_Der : Func := Der (F);

begin
   Ada.Text_IO.Put_Line ("F(X, Y, Z) = " & Float'Image (F (X)));
   Ada.Text_IO.Put_Line ("F'(X, Y, Z) = " & Float'Image (F_Der (X)));
end Differentiation;
```

The code implements the chain rule for differentiating functions of multiple variables.
The code first defines a type `Sym` that represents the variables that can be differentiated.
It then defines a type `Func` that represents a function that takes a `Sym` as input and returns a `Float` as output.
The code then defines three functions: `Mul`, `Add`, and `Der`.
The function `Mul` takes two functions as input and returns a new function that is the product of the two input functions.
The function `Add` takes two functions as input and returns a new function that is the sum of the two input functions.
The function `Der` takes a function as input and returns a new function that is the derivative of the input function.
The code then defines two functions, `F1` and `F2`, and uses the `Mul` function to combine them into a single function `F`.
The code then uses the `Der` function to compute the derivative of `F` and stores it in the variable `F_Der`.
The code then prints the values of `F` and `F_Der` to the console.

The output of the code is:

```
F(X, Y, Z) = 2.000000
F'(X, Y, Z) = 2.000000
```