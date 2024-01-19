```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Letras_Reales is
   X_Cadena : String(1..24) := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   J_Cadena : String(1..24) := "abcdefghijklmnopqrstuvwxyz";
   type Letras_Vector is array(Natural range <>) of Character;
   X_Vector : Letras_Vector := (X_Cadena'Compose);
   J_Vector : Letras_Vector := (J_Cadena'Compose);
   C_Vector : Letras_Vector;

   type Letras_Matriz is array(Natural range <>, Natural range <>) of Character;
   X_J_Matriz : Letras_Matriz(1..2, 1..26) :=
      ((X_Vector(1), J_Vector(1)), (X_Vector(2), J_Vector(2)),
       (X_Vector(3), J_Vector(3)), (X_Vector(4), J_Vector(4)),
       (X_Vector(5), J_Vector(5)), (X_Vector(6), J_Vector(6)),
       (X_Vector(7), J_Vector(7)), (X_Vector(8), J_Vector(8)),
       (X_Vector(9), J_Vector(9)), (X_Vector(10), J_Vector(10)),
       (X_Vector(11), J_Vector(11)), (X_Vector(12), J_Vector(12)),
       (X_Vector(13), J_Vector(13)), (X_Vector(14), J_Vector(14)),
       (X_Vector(15), J_Vector(15)), (X_Vector(16), J_Vector(16)),
       (X_Vector(17), J_Vector(17)), (X_Vector(18), J_Vector(18)),
       (X_Vector(19), J_Vector(19)), (X_Vector(20), J_Vector(20)),
       (X_Vector(21), J_Vector(21)), (X_Vector(22), J_Vector(22)),
       (X_Vector(23), J_Vector(23)), (X_Vector(24), J_Vector(24)));

   X_Q_Vector : Letras_Vector := (X_Cadena'Compose(1..13));
   J_Q_Vector : Letras_Vector := (J_Cadena'Compose(1..13));

begin
   -- Calcula las letras en el medio de las X y las J
   C_Vector := X_J_Matriz(1, X_Q_Vector'Range(1));

   -- Muestra las letras en el medio de las X y las J
   Put_Line("Letras en el medio de las X y las J:");
   for K in C_Vector'Range loop
      Put(C_Vector(K));
   end loop;

   -- Calcula las letras en la primera fila de la matriz
   C_Vector := X_J_Matriz(1, X_Q_Vector'Range);

   -- Muestra las letras en la primera fila de la matriz
   Put_Line("\nLetras en la primera fila de la matriz:");
   for K in C_Vector'Range loop
      Put(C_Vector(K));
   end loop;

   -- Calcula las letras en la segunda fila de la matriz
   C_Vector := X_J_Matriz(2, X_Q_Vector'Range);

   -- Muestra las letras en la segunda fila de la matriz
   Put_Line("\nLetras en la segunda fila de la matriz:");
   for K in C_Vector'Range loop
      Put(C_Vector(K));
   end loop;

   -- Calcula las letras en la diagonal de la matriz
   C_Vector := X_J_Matriz(1..2, 1..13);

   -- Muestra las letras en la diagonal de la matriz
   Put_Line("\nLetras en la diagonal de la matriz:");
   for K in C_Vector'Range loop
      Put(C_Vector(K));
   end loop;

   -- Calcula las letras en la primera columna de J_Q_Vector
   C_Vector := J_Q_Vector(1..13);

   -- Muestra las letras en la primera columna de J_Q_Vector
   Put_Line("\nLetras en la primera columna de J_Q_Vector:");
   for K in C_Vector'Range loop
      Put(C_Vector(K));
   end loop;

   -- Calcula las letras en la última columna de J_Q_Vector
   C_Vector := J_Q_Vector(J_Q_Vector'Range'Last);

   -- Muestra las letras en la úl