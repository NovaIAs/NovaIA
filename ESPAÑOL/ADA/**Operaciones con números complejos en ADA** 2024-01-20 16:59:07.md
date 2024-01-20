```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Complejo is
   type Complejo is record
      real: Float;
      imag: Float;
   end record;

   function Suma(a, b: Complejo) return Complejo is
   begin
      return Complejo'(a.real + b.real, a.imag + b.imag);
   end Suma;

   function Resta(a, b: Complejo) return Complejo is
   begin
      return Complejo'(a.real - b.real, a.imag - b.imag);
   end Resta;

   function Multiplicacion(a, b: Complejo) return Complejo is
   begin
      return Complejo'(a.real * b.real - a.imag * b.imag, a.real * b.imag + a.imag * b.real);
   end Multiplicacion;

   function Division(a, b: Complejo) return Complejo is
   begin
      if b.real = 0 and b.imag = 0 then
         raise Constraint_Error;
      end if;
      return Complejo'((a.real * b.real + a.imag * b.imag) / (b.real * b.real + b.imag * b.imag),
                        (a.imag * b.real - a.real * b.imag) / (b.real * b.real + b.imag * b.imag));
   end Division;

   function ToString(c: Complejo) return String is
   begin
      return Integer'Image(Integer(c.real)) & " + " & Integer'Image(Integer(c.imag)) & "i";
   end ToString;

   procedure Mostrar(c: Complejo) is
   begin
      Ada.Text_IO.Put_Line(ToString(c));
   end Mostrar;

   z: Complejo;

begin
   z := Suma(Complejo'(2.5, 3.1), Complejo'(-1.7, 4.2));
   Mostrar(z);

   z := Resta(Complejo'(2.5, 3.1), Complejo'(-1.7, 4.2));
   Mostrar(z);

   z := Multiplicacion(Complejo'(2.5, 3.1), Complejo'(-1.7, 4.2));
   Mostrar(z);

   z := Division(Complejo'(2.5, 3.1), Complejo'(-1.7, 4.2));
   Mostrar(z);
end Complejo;
```

Este código define un tipo de dato complejo y un conjunto de funciones y procedimientos para operar con números complejos. 

El tipo de dato `Complejo` está definido como un registro con dos campos, `real` e `imag`, que son números de punto flotante. 

Las funciones `Suma`, `Resta`, `Multiplicacion` y `Division` realizan las operaciones aritméticas básicas con números complejos. 

La función `ToString` convierte un número complejo en una cadena de texto. 

El procedimiento `Mostrar` muestra un número complejo en la consola. 

El código principal crea un número complejo `z` y llama a los procedimientos `Mostrar` para mostrar el resultado de las operaciones aritméticas.