```ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Cifras_Romanas is
   type Alfabeto is (I, V, X, L, C, D, M);
   type Numero is new Integer;
   type Signo is record
      Digito : Alfabeto;
      Valor : Numero;
   );
   function Valor_Signo (S : Signo) return Numero is
   begin
      return S.Valor;
   end Valor_Signo;

   function Suma_Signos (S1, S2 : Signo) return Numero is
   begin
      return S1.Valor + S2.Valor;
   end Suma_Signos;

   function Resta_Signos (S1, S2 : Signo) return Numero is
   begin
      if S1.Valor >= S2.Valor then
         return S1.Valor - S2.Valor;
      else
         return 0;  -- Error, el signo S1 no puede ser menor que S2
      end if;
   end Resta_Signos;

   function Convertir_Cifra (N : Numero) return string is
      (Vacio, Mil, Millon, Millardo, Billon, Trillon, Cuatrillon, Quintillon);
      function Convertir_Digito (A : Alfabeto) return string is
         (Vacío, Uno, Cinco, Diez, Cincuenta, Cien, Quinientas, Mil);
      begin
         if N < 1 or N > 9999 then
            return Vacio;
         else
            return
               Convertir_Digito (X) &
               Convertir_Digito (L) &
               Convertir_Digito (C) &
               Convertir_Digito (D) &
               Convertir_Digito (M) &
               Convertir_Digito (CM) &
               Convertir_Digito (CD) &
               Convertir_Digito (XC) &
               Convertir_Digito (XL) &
               Convertir_Digito (XC) &
               Convertir_Digito (XL) &
               Convertir_Digito (IC) &
               Convertir_Digito (ID) &
               Convertir_Digito (IM);
         end if;
      end Convertir_Digito;

      function Convertir_Unidad (N : Numero) return string is
      begin
         if N = 0 then
            return Vacio;
         elsif N < 4 then
            return Convertir_Digito (I) & Convertir_Unidad (N - 1);
         elsif N = 4 then
            return Convertir_Digito (IV);
         elsif N < 9 then
            return Convertir_Digito (V) & Convertir_Unidad (N - 5);
         else
            return Convertir_Digito (IX);
         end if;
      end Convertir_Unidad;

      function Convertir_Decena (N : Numero) return string is
      begin
         if N = 0 then
            return Vacio;
         elsif N < 4 then
            return Convertir_Digito (X) & Convertir_Decena (N - 1);
         elsif N = 4 then
            return Convertir_Digito (XL);
         elsif N < 9 then
            return Convertir_Digito (L) & Convertir_Decena (N - 5);
         else
            return Convertir_Digito (XC);
         end if;
      end Convertir_Decena;

      function Convertir_Centena (N : Numero) return string is
      begin
         if N = 0 then
            return Vacio;
         elsif N < 4 then
            return Convertir_Digito (C) & Convertir_Centena (N - 1);
         elsif N = 4 then
            return Convertir_Digito (CD);
         elsif N < 9 then
            return Convertir_Digito (D) & Convertir_Centena (N - 5);
         else
            return Convertir_Digito (CM);
         end if;
      end Convertir_Centena;

      function Convertir_Millar (N : Numero) return string is
      begin
         if N = 0 then
            return Vacio;
         elsif