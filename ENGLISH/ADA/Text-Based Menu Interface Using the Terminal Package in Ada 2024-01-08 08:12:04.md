```ada
with Ada.Text_IO;
with Interfaces.Terminal;

procedure Test_Terminal is

   procedure Print_Menu is
      Index : Interfaces.Terminal.Index := 1;
      Option : String(1..16);
   begin
      loop
         Ada.Text_IO.Put_Line("  " & Integer'Image(Index) & " - " & Option);
         Index := Index + 1;
         if Index = 10 then
            exit;
         end if;
      end loop;
   end Print_Menu;

   procedure Handle_Input is
      Input : String(1..20);
      Index : Interfaces.Terminal.Index;
   begin
      loop
         Input := Interfaces.Terminal.Get_Line;
         if Input(1) = 'q' or Input(1) = 'Q' then
            exit;
         end if;

         Index := Integer'Value(Input(1));
         if Index >= 1 and Index <= 9 then
            Ada.Text_IO.Put_Line("You selected option " & Integer'Image(Index));
         else
            Ada.Text_IO.Put_Line("Invalid option");
         end if;
      end loop;
   end Handle_Input;

begin
   Interfaces.Terminal.Clear_Screen;
   Interfaces.Terminal.Set_Cursor_Visibility(False);
   Print_Menu;
   Handle_Input;
   Interfaces.Terminal.Set_Cursor_Visibility(True);
end Test_Terminal;
```

This code demonstrates the use of the Terminal package to create a text-based menu interface. It includes two procedures: Print_Menu and Handle_Input. Print_Menu displays a list of menu options, while Handle_Input allows the user to select an option and performs the appropriate action.

The program starts by clearing the screen and hiding the cursor. It then calls Print_Menu to display the menu options. Next, it calls Handle_Input to get the user's selection. If the user enters a valid option, the program prints a message indicating which option was selected. Otherwise, it prints an error message.

After the user has made a selection, the program restores the cursor visibility and exits.

Here are some additional explanations of the code:

* The Interfaces.Terminal package provides a set of procedures for interacting with the terminal. These procedures allow you to clear the screen, set the cursor visibility, and get input from the user.
* The Print_Menu procedure uses a loop to display each menu option on a separate line. The loop counter (Index) is used to keep track of the current option number.
* The Handle_Input procedure uses a loop to get input from the user. It uses the Get_Line procedure from the Terminal package to get a line of text from the user.
* The if statement in the Handle_Input procedure checks if the first character of the input string is 'q' or 'Q'. If it is, the program exits.
* The next if statement checks if the first character of the input string is a digit between 1 and 9. If it is, the program prints a message indicating which option was selected. Otherwise, it prints an error message.
* The Set_Cursor_Visibility procedure from the Terminal package is used to show or hide the cursor.