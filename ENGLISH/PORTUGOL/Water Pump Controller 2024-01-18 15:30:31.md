```
PROGRAM Water_Pump_Controller;

TYPE
   Pump_State = (OFF, ON, FAULT);
   Sensor_State = (DRY, WET);

VAR
   pump_state: Pump_State;
   sensor_state: Sensor_State;
   timer_count: INTEGER;

PROCEDURE Update_Pump_State;
BEGIN
   IF sensor_state = WET THEN
      pump_state := OFF;
   ELSIF sensor_state = DRY AND timer_count > 10 THEN
      pump_state := ON;
   ELSIF pump_state = FAULT THEN
      pump_state := OFF;
   END IF;
END PROCEDURE;

PROCEDURE Update_Timer_Count;
BEGIN
   timer_count := timer_count + 1;
   IF timer_count > 20 THEN
      timer_count := 0;
   END IF;
END PROCEDURE;

BEGIN
   pump_state := OFF;
   sensor_state := DRY;
   timer_count := 0;

   WHILE TRUE DO
      Update_Sensor_State;
      Update_Pump_State;
      Update_Timer_Count;

      CASE pump_state OF
         OFF:
            Writeln('Pump is OFF.');
         ON:
            Writeln('Pump is ON.');
         FAULT:
            Writeln('Pump is FAULT.');
      END CASE;

      Sleep(1000);
   END WHILE;
END Water_Pump_Controller.
```

This code simulates a water pump controller that automatically turns the pump on when the sensor detects that the area is dry and turns it off when the sensor detects that the area is wet.

The program first declares the variables and constants that will be used in the program. The `Pump_State` enumeration defines the three possible states of the pump: `OFF`, `ON`, and `FAULT`. The `Sensor_State` enumeration defines the two possible states of the sensor: `DRY` and `WET`. The `timer_count` variable is used to keep track of how long the pump has been running.

The `Update_Pump_State` procedure is responsible for updating the state of the pump. It checks the state of the sensor and the timer count to determine whether the pump should be turned on, off, or left in its current state.

The `Update_Timer_Count` procedure is responsible for incrementing the timer count. It also resets the timer count to 0 if it reaches a value of 20.

The main program initializes the variables and enters a loop that runs continuously. In each iteration of the loop, the program updates the state of the sensor, the state of the pump, and the timer count. It then displays the current state of the pump on the console.

The program uses the `Sleep` function to pause for 1000 milliseconds (1 second) before the next iteration of the loop.