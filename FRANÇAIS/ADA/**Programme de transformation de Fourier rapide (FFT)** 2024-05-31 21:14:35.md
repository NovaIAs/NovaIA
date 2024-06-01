**Programme de calcul de la transformée de Fourier rapide (FFT)**

**Module d'entrées/sorties**

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure FFT_IO is
   function FFT_Read_File (File_Name : String; Data : out Complex_Array) return Boolean;
   procedure FFT_Write_File (File_Name : String; Data : in Complex_Array);
end FFT_IO;
```

**Module arithmétique**

```ada
with Ada.Numerics.Generic_Complex_Arrays; use Ada.Numerics.Generic_Complex_Arrays;

generic
   type Element_Type is private;
   function Complex_Inversion (E : Element_Type) return Element_Type;
   function Complex_Square (E : Element_Type) return Element_Type;
   function Complex_Equality (Op1, Op2 : Element_Type) return Boolean;
end generic;

package Arithmetic is
   function Invert (E : Element_Type) return Element_Type;
   function Square (E : Element_Type) return Element_Type;
   function Equal (Op1, Op2 : Element_Type) return Boolean;

   constant I : Element_Type := Complex_Inversion (1.0);
end Arithmetic;

generic
   with function Arithmetic.Invert (E : Element_Type) return Element_Type;
   with function Arithmetic.Square (E : Element_Type) return Element_Type;
   with function Arithmetic.Equal (Op1, Op2 : Element_Type) return Boolean;
end generic;

package Complex_Numbers is
   type Complex is record
      Re : Element_Type;
      Im : Element_Type;
   end record;

   function "="+ (Op1, Op2 : Complex) return Complex;
   function "-=" (Op1, Op2 : Complex) return Complex;
   function "=" (Op1, Op2 : Complex) return Boolean;
   function Inverse (C : Complex) return Complex;
   function Square (C : Complex) return Complex;
end Complex_Numbers;
```

**Module FFT**

```ada
with Complex_Numbers; use Complex_Numbers;

generic
   with function Complex_Numbers.Inverse (C : Complex) return Complex;
   with function Complex_Numbers.Square (C : Complex) return Complex;
end generic;

package FFT is
   -- Algorithme de la FFT itérative
   function Transform (Sig : Complex_Array) return Complex_Array;

   -- Algorithme de la FFT récursive
   function Recursive_Transform (Sig : Complex_Array) return Complex_Array;

   -- Utilities
   function Shift (Vecteur : Complex_Array) return Complex_Array;
   function Radix2_FFT (Vecteur : Complex_Array) return Complex_Array;
end FFT;

with FFT; use FFT;

generic
   with function FFT.Transform (Sig : Complex_Array) return Complex_Array;
end generic;

package FFT_Tests is
   procedure Test_FFT_Iterative (N : Positive);
   procedure Test_FFT_Recursive (N : Positive);
end FFT_Tests;
```

**Exécution du programme**

```ada
with FFT_Tests; use FFT_Tests;

procedure FFT_Main is
   N : Positive := 1024; -- Nombre d'échantillons
begin
   -- Test de la FFT itérative
   Test_FFT_Iterative (N);

   -- Test de la FFT récursive
   Test_FFT_Recursive (N);
end FFT_Main;
```

**Explication du code**

Ce code implémente l'algorithme de la transformée de Fourier rapide (FFT) en Ada. Le programme permet de tester les versions itérative et récursive de l'algorithme.

**Module d'entrées/sorties**

Ce module fournit des fonctions pour lire et écrire des données complexes à partir de fichiers.

**Module arithmétique**

Ce module fournit des fonctions génériques pour effectuer des opérations arithmétiques sur des éléments de type complexe. Les fonctions spécifiques pour chaque type complexe sont implémentées dans un package séparé.

**Module FFT**

Ce module fournit des fonctions pour effectuer la transformée de Fourier rapide sur des tableaux complexes. Il contient également des fonctions utilitaires pour déplacer les éléments d'un tableau et effectuer une FFT de base 2.

**Module de tests**

Ce module contient des procédures pour tester les versions itérative et récursive de la FFT.

**Exécution du programme**

La procédure FFT_Main est le point d'entrée du programme. Elle définit le nombre d'échantillons et lance les tests pour les versions itérative et récursive de la FFT.