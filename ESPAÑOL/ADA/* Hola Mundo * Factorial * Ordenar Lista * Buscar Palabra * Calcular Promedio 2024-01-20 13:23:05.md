```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Hola_Mundo is

 begin
 Ada.Text_IO.Put_Line("Hola, Mundo!");
 end Hola_Mundo;
```

Este código imprime el mensaje "Hola, Mundo!" en la consola.

```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Factorial is
    N: Integer;
    F: Integer := 1;

begin
    Ada.Text_IO.Put("Introduce un número entero positivo: ");
    Ada.Integer_Text_IO.Get(N);

    for I in 2 .. N loop
        F := F * I;
    end loop;

    Ada.Text_IO.Put("El factorial de ");
    Ada.Integer_Text_IO.Put(N);
    Ada.Text_IO.Put(" es ");
    Ada.Integer_Text_IO.Put(F);
    Ada.Text_IO.New_Line;
end Factorial;
```

Este código calcula el factorial de un número entero positivo introducido por el usuario.

```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Ordenar_Lista is
    Lista: array(1 .. 10) of Integer;
    I, J: Integer;
    Aux: Integer;

begin
    Ada.Text_IO.Put("Introduce 10 números enteros: ");
    for I in Lista'Range loop
        Ada.Integer_Text_IO.Get(Lista(I));
    end loop;

    for I in Lista'Range loop
        for J in I + 1 .. Lista'Last loop
            if Lista(I) > Lista(J) then
                Aux := Lista(I);
                Lista(I) := Lista(J);
                Lista(J) := Aux;
            end if;
        end loop;
    end loop;

    Ada.Text_IO.Put("Lista ordenada: ");
    for I in Lista'Range loop
        Ada.Integer_Text_IO.Put(Lista(I));
        Ada.Text_IO.Put(" ");
    end loop;
    Ada.Text_IO.New_Line;
end Ordenar_Lista;
```

Este código ordena una lista de 10 números enteros introducidos por el usuario.

```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Buscar_Palabra is
    Texto: String(1 .. 80);
    Palabra_a_Buscar: String(1 .. 20);
    Posicion: Integer := 0;

begin
    Ada.Text_IO.Put("Introduce un texto: ");
    Ada.Text_IO.Get_Line(Texto);
    Ada.Text_IO.Put("Introduce la palabra a buscar: ");
    Ada.Text_IO.Get_Line(Palabra_a_Buscar);

    for I in Texto'Range loop
        if Texto(I .. I + Palabra_a_Buscar'Length - 1) = Palabra_a_Buscar then
            Posicion := I;
            exit;
        end if;
    end loop;

    if Posicion > 0 then
        Ada.Text_IO.Put("La palabra ");
        Ada.Text_IO.Put(Palabra_a_Buscar);
        Ada.Text_IO.Put(" se encuentra en la posición ");
        Ada.Integer_Text_IO.Put(Posicion);
        Ada.Text_IO.New_Line;
    else
        Ada.Text_IO.Put_Line("La palabra no se encuentra en el texto.");
    end if;
end Buscar_Palabra;
```

Este código busca una palabra en un texto introducido por el usuario.

```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Calcular_Promedio is
    Notas: array(1 .. 10) of Integer;
    Suma_Notas: Integer := 0;
    Promedio: Float;
    I: Integer;

begin
    Ada.Text_IO.Put("Introduce 10 notas: ");
    for I in Notas'Range loop
        Ada.Integer_Text_IO.Get(Notas(I));
        Suma_Notas := Suma_Notas + Notas(I);
    end loop;

    Promedio := Float(Suma_Notas) / Float(Notas'Length);

    Ada.Text_IO.Put("El promedio de las notas es ");