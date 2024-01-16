```ada
-- This Ada program solves the Traveling Salesman Problem (TSP) using a genetic algorithm.

with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Tsp is
   type City_Type is record
      X : Integer;
      Y : Integer;
   end record;

   type City_Array_Type is array (Integer range <>) of City_Type;

   type Tour_Type is record
      Cities : City_Array_Type;
      Fitness : Float;
   end record;

   type Tour_Array_Type is array (Integer range <>) of Tour_Type;

   function Distance (C1, C2 : City_Type) return Float is
   begin
      return Sqrt (Float ((C1.X - C2.X)**2 + (C1.Y - C2.Y)**2));
   end Distance;

   function Total_Distance (Tour : Tour_Type) return Float is
   begin
      declare
         Total : Float := 0.0;
      begin
         for I in Tour.Cities'Range loop
            Total := Total + Distance (Tour.Cities (I), Tour.Cities (I + 1));
         end loop;
         return Total;
      end;
   end Total_Distance;

   function Generate_Random_Tour (Cities : City_Array_Type) return Tour_Type is
   begin
      declare
         Random_Tour : Tour_Type;
      begin
         Random_Tour.Cities := Cities;
         for I in Cities'Range loop
            Random_Tour.Cities (I) := Cities (I + 1);
         end loop;
         Random_Tour.Cities (Cities'Last) := Cities (1);
         return Random_Tour;
      end;
   end Generate_Random_Tour;

   function Mutate_Tour (Tour : Tour_Type) return Tour_Type is
   begin
      declare
         I, J : Integer;
         Temp : City_Type;
      begin
         I := Random (Tour.Cities'Range);
         J := Random (Tour.Cities'Range);
         while J = I loop
            J := Random (Tour.Cities'Range);
         end loop;
         Temp := Tour.Cities (I);
         Tour.Cities (I) := Tour.Cities (J);
         Tour.Cities (J) := Temp;
         return Tour;
      end;
   end Mutate_Tour;

   function Crossover_Tours (Tour1, Tour2 : Tour_Type) return Tour_Type is
   begin
      declare
         Child : Tour_Type;
         I, J : Integer;
         Taken : array (Integer range <>) of Boolean;
      begin
         for I in Taken'Range loop
            Taken (I) := False;
         end loop;
         Child.Cities (1) := Tour1.Cities (1);
         Taken (1) := True;
         I := 2;
         while I <= Tour1.Cities'Last loop
            for J in Tour2.Cities'Range loop
               if not Taken (J) then
                  Child.Cities (I) := Tour2.Cities (J);
                  Taken (J) := True;
                  exit;
               end if;
            end loop;
            I := I + 1;
         end loop;
         return Child;
      end;
   end Crossover_Tours;

   function Select_Parents (Population : Tour_Array_Type) return Tour_Array_Type is
   begin
      declare
         return_value : Tour_Array_Type;
         I : Integer;
         Total_Fitness : Float := 0.0;
         Random_Value : Float;
         Current_Fitness : Float;
      begin
         for I in Population'Range loop
            Total_Fitness := Total_Fitness + Population (I).Fitness;
         end loop;
         for I in return_value'Range loop
            Random_Value := Random (Float (Total_Fitness));
            Current_Fitness := 0.0;
            J := 1;
            while Current_Fitness <= Random_Value loop
               Current_Fitness := Current_Fitness + Population (J).Fitness;
               J := J + 1;
            end loop;
            return_value (I) := Population (J - 1);
         end loop;
         return return_value;
      end;
   end Select_Parents;

   procedure Genetic_Algorithm (Cities : City_Array_Type) is
      declare
         Population : Tour_Array_Type;
         I : Integer;
         Best_Tour : Tour_Type;
      begin
         for I in Population'Range loop
            Population (I) := Generate_Random_Tour (Cities);
         end loop;
         Best_Tour := Population (1);
         for I in 2 .. Population'Last loop
            if Population (I).Fitness < Best_Tour.Fitness then
               Best_Tour := Population (I);
            end if;
         end loop;
         while Best_Tour.Fitness > 0.0 loop
            Population := Select_Parents (Population);
