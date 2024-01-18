```vhdl
-- This is a complex VHDL code that implements a finite state machine (FSM) to control a traffic light.
-- The FSM has multiple states, each representing a phase of the traffic light cycle.
-- The code includes comments to explain the functionality of each section.

-- Define the library and package to be used.
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Define the entity of the FSM.
entity Traffic_Light_FSM is
    -- Define the input ports.
    port (
        Clock : in STD_LOGIC;         -- Clock input for the FSM.
        Reset : in STD_LOGIC;         -- Reset input to reset the FSM to its initial state.
        Car_Sensor_N : in STD_LOGIC;  -- Sensor input to detect the presence of a car on the north side of the intersection.
        Car_Sensor_S : in STD_LOGIC;  -- Sensor input to detect the presence of a car on the south side of the intersection.
        Walk_Button_N : in STD_LOGIC; -- Push-button input to request a pedestrian crossing on the north side of the intersection.
        Walk_Button_S : in STD_LOGIC; -- Push-button input to request a pedestrian crossing on the south side of the intersection.
    );

    -- Define the output ports.
    port (
        Green_N : out STD_LOGIC;      -- Output signal to turn on the green light for the north side of the intersection.
        Yellow_N : out STD_LOGIC;     -- Output signal to turn on the yellow light for the north side of the intersection.
        Red_N : out STD_LOGIC;        -- Output signal to turn on the red light for the north side of the intersection.
        Green_S : out STD_LOGIC;      -- Output signal to turn on the green light for the south side of the intersection.
        Yellow_S : out STD_LOGIC;     -- Output signal to turn on the yellow light for the south side of the intersection.
        Red_S : out STD_LOGIC;        -- Output signal to turn on the red light for the south side of the intersection.
        Walk_Light_N : out STD_LOGIC; -- Output signal to turn on the pedestrian crossing light on the north side of the intersection.
        Walk_Light_S : out STD_LOGIC; -- Output signal to turn on the pedestrian crossing light on the south side of the intersection.
    );
end Traffic_Light_FSM;

-- Define the architecture of the FSM.
architecture Behavioral of Traffic_Light_FSM is

    -- Define the states of the FSM.
    type State is (S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12);
    signal Current_State : State := S0; -- Current state of the FSM.

    -- Define the next state logic.
    function Next_State(Current_State : State; Car_Sensor_N : STD_LOGIC; Car_Sensor_S : STD_LOGIC; Walk_Button_N : STD_LOGIC; Walk_Button_S : STD_LOGIC) return State is
    begin
        case Current_State is
            when S0 => -- Initial state
                if Car_Sensor_N = '1' or Car_Sensor_S = '1' then
                    return S1; -- Move to state S1 if a car is detected.
                elsif Walk_Button_N = '1' or Walk_Button_S = '1' then
                    return S9; -- Move to state S9 if a pedestrian button is pressed.
                else
                    return S0; -- Stay in state S0 if no car or pedestrian is detected.
                end if;
            when S1 => -- North-South green phase
                return S2; -- Move to state S2 after a fixed time.
            when S2 => -- North-South yellow phase
                return S3; -- Move to state S3 after a fixed time.
            when S3 => -- North-South red phase
                if Car_Sensor_N = '0' and Car_Sensor_S = '0' then
                    return S0; -- Move to state S0 if both sensors are clear.
                else
                    return S3; -- Stay in state S3 if a car is still detected.
                end if;
            when S9 => -- Pedestrian phase
                return S10; -- Move to state S10 after a fixed time.
            when S10 => -- Pedestrian yellow phase
                return S11; -- Move to state S11 after a fixed time.
            when S11 => -- Pedestrian red phase
                if Walk_Button_N = '0' and Walk_Button_S = '0' then
                    return S0; -- Move to state S0 if both pedestrian buttons are released.
                else
                    return S11; -- Stay in state S11 if a pedestrian button is still pressed.
                end if;
            when others => -- Default state
                return S0; -- Move to state S0 if an invalid state is reached.
        end case;
    end function Next_State;

    -- Define the output logic.
    function Outputs(Current_State : State) return STD_LOGIC_VECTOR(7 downto 0) is
    begin
        case Current_State is
            when S1 => return "00001111"; -- North-South green phase
            when S2 => return "00011110"; -- North-South yellow phase
            when S3 => return "00111100"; -- North-South red phase
            when S9 => return "11000011"; -- Pedestrian phase
            when S10 => return "11000110"; -- Pedestrian yellow phase
            when S11 => return "11001100"; -- Pedestrian red phase
            when others => return "00000000"; -- Default state
        end case;
    end function Outputs;

    -- Define the process that updates the state and outputs of the FSM.
    process(Clock, Reset)
    begin
        if Reset = '1' then
            -- Reset the FSM to its initial state.
            Current_State <= S0;
        elsif rising_edge(Clock) then
            -- Update the state and outputs of the FSM.
            Current_State <= Next_State(Current_State, Car_Sensor_N, Car_Sensor_S, Walk_Button_N, Walk_Button_S);
            {Green_N, Yellow_N, Red_N, Green_S, Yellow_S, Red_S, Walk_Light_N, Walk_Light_S} <= Outputs(Current_State);
        end if;
    end process;

end Behavioral;
```

This code implements a traffic light FSM that controls the traffic signals at an intersection. The FSM has multiple states, each representing a phase of the traffic light cycle. The state transitions are determined by the inputs from the car sensors and pedestrian buttons, and the outputs of the FSM control the traffic lights and pedestrian crossing lights.

The code includes comments to explain the functionality of each section, making it easy to understand the operation of the FSM and how it controls the traffic lights. The code is also written in a structured and organized manner, which makes it easier to maintain and modify in the future.