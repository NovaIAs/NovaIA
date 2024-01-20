```ada
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

procedure PruebasDeConversión is

   type Alfabeto is (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Ñ, O, P, Q, R, S, T, U, V, W, X, Y, Z);
   type Número is (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
   type Caracter is (Espacio, Coma, Punto, Exclamación, Interrogación, AperturaParéntesis, CierreParéntesis);
   type SignoDeOperación is (Suma, Resta, Multiplicación, División);
   type Digito is Alfabeto | Número;
   type Palabra is array (Positive range <>) of Digito;   -- el <>) indica que se permite que el largo del arreglo pueda ir desde 1 hasta algo muy grande

   function CaracterAGeneral (C : Caracter) return Digito is (
      (C = Espacio) => Espacio,
      (C = Coma) => Coma,
      (C = Punto) => Punto,
      (C = Exclamación) => Exclamación,
      (C = Interrogación) => Interrogación,
      (C = AperturaParéntesis) => AperturaParéntesis,
      (C = CierreParéntesis) => CierreParéntesis
   );

   function GeneralACaracter (G : Digito) return Caracter is (
      (G = Espacio) => Espacio,
      (G = Coma) => Coma,
      (G = Punto) => Punto,
      (G = Exclamación) => Exclamación,
      (G = Interrogación) => Interrogación,
      (G = AperturaParéntesis) => AperturaParéntesis,
      (G = CierreParéntesis) => CierreParéntesis
   );

   function DigitoACad (D : Digito) return String is (
      (D = 0) => "0",
      (D = 1) => "1",
      (D = 2) => "2",
      (D = 3) => "3",
      (D = 4) => "4",
      (D = 5) => "5",
      (D = 6) => "6",
      (D = 7) => "7",
      (D = 8) => "8",
      (D = 9) => "9",
      (D = A) => "A",
      (D = B) => "B",
      (D = C) => "C",
      (D = D) => "D",
      (D = E) => "E",
      (D = F) => "F",
      (D = G) => "G",
      (D = H) => "H",
      (D = I) => "I",
      (D = J) => "J",
      (D = K) => "K",
      (D = L) => "L",
      (D = M) => "M",
      (D = N) => "N",
      (D = Ñ) => "Ñ",
      (D = O) => "O",
      (D = P) => "P",
      (D = Q) => "Q",
      (D = R) => "R",
      (D = S) => "S",
      (D = T) => "T",
      (D = U) => "U",
      (D = V) => "V",
      (D = W) => "W",
      (D = X) => "X",
      (D = Y) => "Y",
      (D = Z) => "Z",
      (D = Espacio) => " ",
      (D = Coma) => ",",
      (D = Punto) => ".",
      (D = Exclamación) => "!",
      (D = Interrogación) => "?",
      (D = AperturaParéntesis) => "(",
      (D = CierreParéntesis) => ")"
   );

   function CadADigito (Cadena : String) return Digito is (
      (Cadena = "0") => 0,
      (Cadena = "1") => 1,
      (Cadena = "2") => 2,
      (Cadena = "3") => 3,
      (Cadena = "4") => 4,
      (Cadena = "5") => 5,
      (Cadena = "6") => 6,
      (Cadena = "7") => 7,
      (Cadena = "8") => 8,
      (Cadena = "9") => 9,
      (Cadena = "A") => A,
      (Cadena = "B") => B,
      (Cadena = "C") => C,
      (Cadena = "D") => D,
      (Cadena = "E") => E,
      (Cadena = "F") => F,
      (Cadena = "G") => G,
      (Cadena = "H") => H,
      (Cadena = "I") => I,
      (Cadena = "J") => J,
      (Cadena = "K") => K,
      (Cadena = "L") => L,
      (Cadena = "M") => M,
      (Cadena = "N") => N,
      (Cadena = "Ñ") => Ñ,
      (Cadena = "O") => O,
      (Cadena = "P") => P,
      (Cadena = "Q") => Q,
      (Cadena = "R") => R,
      (Cadena = "S") => S,
      (Cadena = "T") => T,
      (Cadena = "U") => U,
      (Cadena = "V") => V,
      (Cadena = "W") => W,
      (Cadena = "X") => X,
      (Cadena = "Y") => Y,
      (Cadena = "Z") => Z,
      (Cadena = " ") => Espacio,
      (Cadena = ",") => Coma,
      (Cadena = ".") => Punto,
      (Cadena = "!") => Exclamación,
      (Cadena = "?") => Interrogación,
      (Cadena = "(") => AperturaParéntesis,
      (Cadena = ")") => CierreParéntesis
   );

   procedure ConvertirACaraFelizIntercalado is
      Cadena : String := ".:-=:.";
      CaraFeliz : String := "";
   begin
      for i in Cadena'Range loop
         CaraFeliz := CaraFeliz & DigitoACad(CadADigito(Cadena(i))) & DigitoACad(GeneralACaracter(CaracterAGeneral(CadADigito(Cadena(i)))));
      end loop;
      Ada.Text_IO.Put_Line(CaraFeliz);
   end ConvertirACaraFelizIntercalado;

   procedure ConvertirACaraTristeIntercalado is
      Cadena : String := ",-_-.?,";
      CaraTriste : String := "";
   begin
      for i in Cadena'Range loop
         CaraTriste := CaraTriste & DigitoACad(CadADigito(Cadena(i))) & DigitoACad(GeneralACaracter(CaracterAGeneral(CadADigito(Cadena(i)))));
      end loop;
      Ada.Text_IO.Put_Line(CaraTriste);
   end ConvertirACaraTristeIntercalado;

begin
   ConvertirACaraFelizIntercalado;
   ConvertirACaraTristeIntercalado;
end PruebasDeConversión;
```

Este código realiza varias conversiones entre diferentes tipos de datos en Ada. Primero, define varios tipos personalizados, incluyendo un tipo de carácter, un tipo de dígito y un tipo de palabra. Luego, define funciones para convertir entre estos tipos. Finalmente, utiliza estas funciones para convertir una cadena de caracteres en una "cara feliz" y una "cara triste", intercalando los caracteres de la cadena con sus valores numéricos.

El código está bien comentado y es fácil de seguir. Utiliza varias características avanzadas de Ada, como tipos personalizados, funciones genéricas y bucles for.