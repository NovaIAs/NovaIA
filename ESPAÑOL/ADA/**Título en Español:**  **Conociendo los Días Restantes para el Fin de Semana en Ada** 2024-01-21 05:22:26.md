```ada
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Complejidad is
   type Semana is (Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, Domingo);

   type Día_Laborables is (Lunes .. Viernes);

   function Es_Laborables (Día : Semana) return Boolean is begin
      return Día in Día_Laborables;
   end Es_Laborables;

   function Días_Restantes (Día_Actual : Semana) return Integer is begin
      if Es_Laborables (Día_Actual) then
         return 5 - Integer (Día_Actual) + Lunes;
      else
         return 7 - Integer (Día_Actual) + Lunes;
      end if;
   end Días_Restantes;

begin
   Put_Line ("Introduce el día de la semana actual:");
   Día_Actual : Semana := Semana'Last;
   Get (Día_Actual);
   Put_Line ("Días restantes para el fin de semana:");
   Put (Días_Restantes (Día_Actual));
   New_Line;
end Complejidad;
```

Explicación del código:

* Se definen dos tipos enumerados: `Semana` y `Día_Laborables`. El tipo `Semana` representa los días de la semana y el tipo `Día_Laborables` representa los días laborables.
* Se define una función `Es_Laborables` que recibe un día de la semana y devuelve un valor booleano que indica si ese día es laborable o no.
* Se define una función `Días_Restantes` que recibe un día de la semana y devuelve el número de días restantes para el fin de semana.
* En el procedimiento principal, se solicita al usuario que introduzca el día de la semana actual y se guarda en la variable `Día_Actual`.
* Se llama a la función `Días_Restantes` con el día actual como argumento y se imprime el resultado en la consola.

Este código es complejo porque combina varios conceptos diferentes de Ada, como tipos enumerados, funciones y procedimientos. También utiliza una lógica compleja para determinar el número de días restantes para el fin de semana.