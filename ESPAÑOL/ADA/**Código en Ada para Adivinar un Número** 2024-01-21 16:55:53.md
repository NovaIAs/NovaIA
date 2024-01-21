```ada
with Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Strings.Unbounded, Ada.Strings.Maps;

procedure Adivina_el_Número is
   type Jugador is (Jugador1, Jugador2);

   subtype Turno is Player range 1 .. Jugador'Last;
   subramas para el ganador y el perdedor.

   function Número_Aleatorio return Natural;

   package Tabla_De_Números is new Ada.Strings.Unbounded.Vectors
     (Index_Type => Jugador'Range, Element_Type => Naturals);

   procedure Jugar is
      function Intento (N: Natural) return Boolean is
         -- Devuelve verdadero si N es igual al número adivinado
         (N = Número_Aleatorio);

      Jugador_Actual: Turno := Jugador1;

      Uso_De_Números: Tabla_De_Números.Table := (Jugador1 => {}, Jugador2 => {});

      loop
         Uso_De_Números (Jugador_Actual) :=
            Uso_De_Números (Jugador_Actual) &
            Player_Input (Número_Aleatorio);

         if Intento (Uso_De_Números (Jugador_Actual) (Uso_De_Números
            (Jugador_Actual)'Last)) then

            Ada.Text_IO.Put_Line ("¡Ganaste!");

            exit; -- sale del loop
         end if;

         Jugador_Actual := Jugador_Actual'Succ;
      end loop;
   end Jugar;

   Jugador_Objetivo: Turno;
   Número_Objetivo: Natural;

begin
   Jugar;
end Adivina_el_Número;
```