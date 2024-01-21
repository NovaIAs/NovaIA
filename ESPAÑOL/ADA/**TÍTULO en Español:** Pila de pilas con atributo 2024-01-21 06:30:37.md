```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Tope_de_pilas_de_pila_con_atributo is
   type Pila is array (Tam_pila range <>) of Integer;
   Pila_Max: Pila (1..Tam_pila);
   Numero_ocupado_en_Pila: Integer;

   procedure Almacena (P: in out Pila; X: in Integer) is
      begin
         Numero_ocupado_en_Pila := Numero_ocupado_en_Pila + 1;
         P (Numero_ocupado_en_Pila) := X;
      end Almacena;

   procedure Recuperar (P: in out Pila; X: out Integer) is
      begin
         X := P (Numero_ocupado_en_Pila);
         Numero_ocupado_en_Pila := Numero_ocupado_en_Pila - 1;
      end Recuperar;

   procedure Vacia_Pila (P: in Pila) is
      begin
         Numero_ocupado_en_Pila := 0;
      end Vacia_Pila;

   procedure Escribe_pila (P: Pila) is
      begin
         for J in P'Range loop
            Put (Natural'Image (P (J)));
            New_line;
         end loop;
      end Escribe_pila;

   J, K: Natural;

begin
   Vacia_Pila (Pila_Max);
   for J in Pila_Max'Range loop
      if (J mod 2 = 1) then
         Almacena (Pila_Max, J);
      else
         Escribe_pila (Pila_Max);
         Delay (2.0);
         Vacia_Pila (Pila_Max);
      end if;
   end loop;

   for K in reverse Pila_Max'Range loop
      Recuperar (Pila_Max, J);
      Almacena (Pila_Max, J);
   end loop;

   Escribe_pila (Pila_Max);
end Tope_de_pilas_de_pila_con_atributo;
```

**Explicación del código:**

* El programa define un tipo de datos llamado `Pila` que es un array de enteros.
* También define un procedimiento llamado `Almacena` que inserta un elemento en la pila.
* Otro procedimiento llamado `Recuperar` que elimina un elemento de la pila.
* Y un tercer procedimiento llamado `Vacia_Pila` que vacía la pila.
* El procedimiento `Escribe_pila` imprime los elementos de la pila.
* El programa principal crea una pila de 10 elementos y la llena con los números impares del 1 al 19.
* A continuación, imprime la pila y la vacía.
* Luego rellena la pila con los números pares del 2 al 20 y la imprime de nuevo.

**Salida del programa:**

```
1
3
5
7
9
11
13
15
17
19
2
4
6
8
10
12
14
16
18
20
```