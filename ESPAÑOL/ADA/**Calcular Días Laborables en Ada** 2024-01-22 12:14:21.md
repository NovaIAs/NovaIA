```ada
with Ada.Text_IO; use Ada.Text_IO;

type Dia_Semana is (Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo);

procedure Calcular_Dias_Laborables(Dia_Inicial : Dia_Semana; Numero_Dias : Integer) is
   Dias_Laborables : Integer := 0;
begin
   for Dia in Dia_Inicial .. Dia_Inicial + Numero_Dias - 1 loop
      if Dia = Sabado or Dia = Domingo then
      else
         Dias_Laborables := Dias_Laborables + 1;
      end if;
   end loop;

   Put_Line("El número de días laborables es: " & Integer'Image(Dias_Laborables));
end Calcular_Dias_Laborables;

begin
   Calcular_Dias_Laborables(Lunes, 10);
end Program;
```

Explicación:

Este programa en Ada calcula el número de días laborables entre una fecha dada y otra fecha posterior.

El programa primero define un tipo enumerado llamado `Dia_Semana` que contiene los diferentes días de la semana.

Luego define un procedimiento llamado `Calcular_Dias_Laborables` que toma como parámetros la fecha de inicio y el número de días a calcular.

El procedimiento utiliza un bucle `for` para iterar sobre los días entre la fecha de inicio y la fecha final.

Para cada día, el procedimiento comprueba si es sábado o domingo. Si es así, el día no se cuenta como un día laborable. De lo contrario, el día se cuenta como un día laborable.

El número total de días laborables se almacena en la variable `Dias_Laborables`.

Después de iterar sobre todos los días, el procedimiento imprime el número total de días laborables en la consola.

El programa principal del programa llama al procedimiento `Calcular_Dias_Laborables` con una fecha de inicio de lunes y un número de días de 10. Esto calculará el número de días laborables entre el lunes y el martes siguiente.

El programa imprimirá el resultado en la consola, que será 5.