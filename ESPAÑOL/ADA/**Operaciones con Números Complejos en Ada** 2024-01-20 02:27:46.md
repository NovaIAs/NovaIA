```ada
with Ada.Text_IO;

procedure MuyComplejo is
   package Complejos is
      type Complejo is record
         real : Float;
         imag : Float;
      end record;

      procedure +(C1, C2 : Complejo) return Complejo is
      ((C1.real + C2.real), (C1.imag + C2.imag));

      procedure -(C1, C2 : Complejo) return Complejo is
      ((C1.real - C2.real), (C1.imag - C2.imag));

      procedure *(C1, C2 : Complejo) return Complejo is
      ((C1.real * C2.real - C1.imag * C2.imag),
       (C1.real * C2.imag + C1.imag * C2.real));

      procedure /(C1, C2 : Complejo) return Complejo is
      ((C1.real * C2.real + C1.imag * C2.imag) /
       (C2.real * C2.real + C2.imag * C2.imag),
       (C1.imag * C2.real - C1.real * C2.imag) /
       (C2.real * C2.real + C2.imag * C2.imag));

      procedure Imprimir(C : Complejo) is
      begin
         Ada.Text_IO.Put(C.real);
         Ada.Text_IO.Put(" ");
         Ada.Text_IO.Put(C.imag);
         Ada.Text_IO.New_Line;
      end Imprimir;

      procedure Calcular_Potencia(C : Complejo; N : Integer)
         return Complejo is
      (local
         Temporal : Complejo := C;
         Resultado : Complejo := (1.0, 0.0);
      begin
         for I in 1 .. N loop
            Resultado := Resultado * Temporal;
         end loop;
         return Resultado;
      end Calcular_Potencia);
   end Complejos;

   C1 := Complejos.(3.0, 4.0);
   C2 := Complejos.(5.0, -6.0);

begin
   Complejos.Imprimir(C1 + C2);
   Complejos.Imprimir(C1 - C2);
   Complejos.Imprimir(C1 * C2);
   Complejos.Imprimir(C1 / C2);
   Complejos.Imprimir(Complejos.Calcular_Potencia(C1, 2));
end MuyComplejo;
```

Explicación:

* Definimos un nuevo tipo de dato llamado `Complejo` que representa un número complejo con partes reales e imaginarias.
* Definimos funciones para sumar, restar, multiplicar y dividir números complejos.
* Definimos una función para imprimir un número complejo.
* Definimos una función para calcular la potencia de un número complejo.
* Creamos dos variables complejas `C1` y `C2` y usamos las funciones definidas para realizar operaciones con ellas.