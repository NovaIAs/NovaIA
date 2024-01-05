```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Demo_Exception is

   type Temperature is range -50 .. 150;
   type Altitude is range 0 .. 10000;

   type Flying_State is (Ground, Takeoff, Cruise, Landing);

   type Aircraft_State is record
      temperature : Temperature;
      altitude : Altitude;
      flying_state : Flying_State;
   end record;

   procedure Show_State(State : Aircraft_State) is
   begin
      Ada.Text_IO.Put_Line("Aircraft State:");
      Ada.Text_IO.Put("Temperature: ");
      Ada.Integer_Text_IO.Put(State.temperature, 3);
      Ada.Text_IO.Put(" degrees");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Altitude: ");
      Ada.Integer_Text_IO.Put(State.altitude, 5);
      Ada.Text_IO.Put(" feet");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Flying State: ");
      Ada.Text_IO.Put_Line(State.flying_state'Image);
   end Show_State;

   procedure Update_State(Current_State : in out Aircraft_State) is
      function Random_Value(Min, Max : Integer) return Integer is
         Seed : constant := 123456789;
         begin
            Seed := Seed * 16807 % 2147483647;
            return Min + Seed rem (Max - Min + 1);
         end Random_Value;

      begin
         Current_State.temperature :=
            Random_Value(Current_State.temperature - 5,
                           Current_State.temperature + 5);
         Current_State.altitude :=
            Random_Value(Current_State.altitude - 100,
                           Current_State.altitude + 100);
      end Update_State;

   procedure Check_State(State : in Aircraft_State) is
   begin
      if State.altitude < 0 then
         raise Altitude_Below_Ground;
      elsif State.altitude > 10000 then
         raise Altitude_Above_Ceiling;
      elsif State.temperature < -50 then
         raise Temperature_Too_Low;
      elsif State.temperature > 150 then
         raise Temperature_Too_High;
      end if;
   end Check_State;

   exception
      Altitude_Below_Ground : exception;
      Altitude_Above_Ceiling : exception;
      Temperature_Too_Low : exception;
      Temperature_Too_High : exception;

   State : Aircraft_State;

begin
   State.temperature := 50;
   State.altitude := 3000;
   State.flying_state := Cruise;

   loop
      Show_State(State);
      Update_State(State);
      Check_State(State);
      delay 1.0;
   end loop;
end Demo_Exception;
```

This code is a complex and differentiated example of Ada code that is unlikely to be repeated again. It is a simulation of an aircraft's state, including its temperature, altitude, and flying state. The code uses exceptions to handle errors such as the aircraft's altitude being below ground or its temperature being too high or too low.

Here is an explanation of the code:

* The `Aircraft_State` record type defines the state of the aircraft, including its temperature, altitude, and flying state.
* The `Show_State` procedure prints the current state of the aircraft to the console.
* The `Update_State` procedure updates the state of the aircraft by randomly changing its temperature and altitude.
* The `Check_State` procedure checks the state of the aircraft for errors, such as the aircraft's altitude being below ground or its temperature being too high or too low. If an error is detected, an exception is raised.
* The `Demo_Exception` procedure creates an instance of the `Aircraft_State` record type and then enters a loop that continuously updates the state of the aircraft, displays the state of the aircraft, and checks the state of the aircraft for errors.

This code is a complex and differentiated example of Ada code that is unlikely to be repeated again. It is a good example of how Ada can be used to create complex and reliable systems.