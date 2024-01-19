```ada
with Ada.Strings.Fixed;
with Ada.Text_IO; with Ada.Integer_Text_IO;

procedure Ada_95_Emoji is
   package String_Slice is new Ada.Strings.Fixed(1, 20);
   type Emoji_String is string(1 .. String_Slice'last);

   procedure Show(Message : in string; Option : in string) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      Cursor_Position : integer := Terminal_IO.Cursor_Position;
      Text_Length    : integer := String_Slice'last;
   begin
      if Text_Length > 0 then
         Terminal_IO.Set_Cursor(0, Terminal_IO.Line);
      Put(Message);
      if Text_Length > 0 then
         Terminal_IO.Set_Cursor(Cursor_Position, Terminal_IO.Line);
      elsif Option is not null then
         Put(Option);
      end if;
   end Show;

   Constant Emojis : Emoji_String := "" &
      Characters.Compose("\u{1F600}") &
      Characters.Compose("\u{1F601}") &
      Characters.Compose("\u{1F602}") &
      Characters.Compose("\u{1F603}") &
      Characters.Compose("\u{1F604}") &
      Characters.Compose("\u{1F605}") &
      Characters.Compose("\u{1F606}");
   Index : integer := Emojis'first - 1;
   Next  : Boolean := False;
begin
   loop
      delay 1.0;
      Index := Index + 1;
      Index := Index mod Emojis'last;
      Emoji_String'Put(Emojis, Index, Index + 1);
      Show(Emojis(Index .. Index),
           if Next then "\r" & Emojis(Index .. Index) else "" end if);
      Next := not Next;
      if Keyboard_Status then
         if Keyboard_IO.KeyPressed is 'Q' or Keyboard_IO.KeyPressed is 'q' then
            exit;
         end if;
      end if;
   end loop;
end Ada_95_Emoji;
```

Este código de ADA crea una colección de emojis y los muestra secuencialmente en la pantalla, alternando entre mostrar un solo emoji y mostrar una línea de emojis. El código utiliza el paquete Ada.Strings.Fixed para definir un tipo de cadena de longitud fija, Emoji_String, que se usa para almacenar los emojis. El procedimiento Show() se utiliza para mostrar los emojis en la pantalla, y el procedimiento principal Ada_95_Emoji() crea la colección de emojis y los muestra secuencialmente con un retardo de 1 segundo entre cada emoji. El código también incluye la posibilidad de salir del programa pulsando la tecla 'Q' o 'q'.