```modula-2

(* A small example of a Modula-2 program. This program
   computes the GCD of two positive integers and their LCM.
   GCD is greatest common divisor and LCM is least common
   multiple. *)

MODULE GreatestCommonDivisorLCM;

(* Common types and constants. *)

TYPE Cardinal = LONGINT;
TYPE Positive = Cardinal;

(* GCD and LCM. *)

PROCEDURE Gcd(x, y: Positive): Positive;
  (* The GCD of x and y. *)
BEGIN
  VAR rem: Positive;
  WHILE y <> 0 DO
    rem := x MOD y;
    x := y;
    y := rem
  END;
  RETURN x
END Gcd;

PROCEDURE Lcm(x, y: Positive): Positive;
  (* The LCM of x and y. *)
BEGIN
  RETURN (x * y) DIV Gcd(x, y);
END Lcm;

(* Test program. *)

FROM Terminal IMPORT WriteString, WriteLongInt, WriteLn;

PROCEDURE Main;
BEGIN
  WriteString("Enter two numbers: ");
  VAR n1, n2: Positive;
  ReadLongInt(n1);
  ReadLongInt(n2);
  WriteString("GCD = ");
  WriteLongInt(Gcd(n1, n2));
  WriteString(", LCM = ");
  WriteLongInt(Lcm(n1, n2));
  WriteLn
END Main.

END GreatestCommonDivisorLCM.
```

Explanation:

The module `GreatestCommonDivisorLCM` contains the types, constants, and procedures for computing the GCD and LCM of two positive integers. The type `Cardinal` is used to represent non-negative integers, and the type `Positive` is a subset of `Cardinal` that represents positive integers.

The procedure `Gcd` computes the GCD of two positive integers using the Euclidean algorithm. It repeatedly divides the larger number by the smaller number and takes the remainder. The GCD is the last non-zero remainder.

The procedure `Lcm` computes the LCM of two positive integers using the formula `LCM = (x * y) / GCD`.

The test program `Main` prompts the user to enter two numbers, calls the procedures `Gcd` and `Lcm` to compute the GCD and LCM of the numbers, and prints the results to the console.