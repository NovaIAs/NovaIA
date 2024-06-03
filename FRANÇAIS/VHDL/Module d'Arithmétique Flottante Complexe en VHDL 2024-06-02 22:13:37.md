**Module d'arithmétique flottante complexe (CFloatArithmetic)**

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity CFloatArithmetic is
    generic (
        -- Nombre de bits de la mantisse
        MANTISSA_BITS: positive := 24
    );

    port (
        a_real: in real;
        a_imag: in real;
        b_real: in real;
        b_imag: in real;
        op: in std_logic_vector(2 downto 0); -- Opération à effectuer
        res_real: out real;
        res_imag: out real
    );
end CFloatArithmetic;

architecture Behavioral of CFloatArithmetic is
    subtype Mantissa is integer range -2 ** (MANTISSA_BITS - 1) to 2 ** (MANTISSA_BITS - 1);
    subtype Exponent is integer range 0 to 2 * MANTISSA_BITS;

    type Complex is record
        mantissa: Mantissa;
        exponent: Exponent;
    end record;

    -- Fonctions auxiliaires
    function ToComplex (real, real) return Complex;
    function FromComplex (Complex) return real;

    -- Opérations
    function Addition (Complex, Complex) return Complex;
    function Subtraction (Complex, Complex) return Complex;
    function Multiplication (Complex, Complex) return Complex;
    function Division (Complex, Complex) return Complex;

    -- Signal interne
    signal a, b, res: Complex;

begin
    -- Conversion des entrées en complexes
    a <= ToComplex(a_real, a_imag);
    b <= ToComplex(b_real, b_imag);

    -- Exécution de l'opération
    case op is
        when "000" => res <= Addition(a, b);
        when "001" => res <= Subtraction(a, b);
        when "010" => res <= Multiplication(a, b);
        when "011" => res <= Division(a, b);
        when others => null;
    end case;

    -- Conversion du résultat en réel
    res_real <= FromComplex(res.mantissa, res.exponent);
    res_imag <= FromComplex(res.mantissa, res.exponent);
end Behavioral;

-- Implémentation des fonctions auxiliaires
function ToComplex (real, real) return Complex is
    variable c: Complex;
begin
    c.mantissa := To_Binary(real, MANTISSA_BITS);
    c.exponent := To_Binary(real, MANTISSA_BITS);
    return c;
end ToComplex;

function FromComplex (c: Complex) return real is
begin
    return To_Real(c.mantissa, c.exponent);
end FromComplex;

-- Implémentation des opérations
function Addition (a, b: Complex) return Complex is
    variable c: Complex;
begin
    c.mantissa := a.mantissa + b.mantissa;
    c.exponent := a.exponent + b.exponent;
    return c;
end Addition;

function Subtraction (a, b: Complex) return Complex is
    variable c: Complex;
begin
    c.mantissa := a.mantissa - b.mantissa;
    c.exponent := a.exponent - b.exponent;
    return c;
end Subtraction;

function Multiplication (a, b: Complex) return Complex is
    variable c: Complex;
begin
    c.mantissa := a.mantissa * b.mantissa;
    c.exponent := a.exponent + b.exponent;
    return c;
end Multiplication;

function Division (a, b: Complex) return Complex is
    variable c: Complex;
begin
    c.mantissa := a.mantissa / b.mantissa;
    c.exponent := a.exponent - b.exponent;
    return c;
end Division;
```

**Explication du code:**

Ce code implémente un module d'arithmétique flottante complexe qui prend deux nombres complexes en entrée (réel et imaginaire) et une opération à effectuer (addition, soustraction, multiplication ou division).

**Type complexe:** Le type `Complex` est défini comme un enregistrement contenant une mantisse et un exposant.

**Fonctions auxiliaires:** Les fonctions `ToComplex` et `FromComplex` convertissent des nombres réels en complexes et vice-versa.

**Opérations:** Les fonctions `Addition`, `Soustraction`, `Multiplication` et `Division` effectuent les opérations arithmétiques respectives sur deux nombres complexes.

**Architecture:** L'architecture `Behavioral` convertit les entrées en complexes, exécute l'opération et convertit le résultat en réel. La conversion utilise les fonctions auxiliaires définies précédemment.