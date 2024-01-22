```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure ImprimirMatriz(Matriz : in array (1 .. 10, 1 .. 10) of Integer) is
begin
    for I in 1 .. 10 loop
        for J in 1 .. 10 loop
            Put(Integer'Image(Matriz(I, J)), Width => 3);
        end loop;
        New_Line;
    end loop;
end ImprimirMatriz;

procedure RellenarMatriz(Matriz : out array (1 .. 10, 1 .. 10) of Integer) is
begin
    for I in 1 .. 10 loop
        for J in 1 .. 10 loop
            Matriz(I, J) := 0;
        end loop;
    end loop;
end RellenarMatriz;

procedure RotarMatriz(Matriz : in out array (1 .. 10, 1 .. 10) of Integer; Direccion : in Boolean) is
begin
    if Direccion then
        for I in 1 .. 10 loop
            for J in 1 .. 10 loop
                Temp := Matriz(I, J);
                Matriz(I, J) := Matriz(J, 10 - I + 1);
                Matriz(J, 10 - I + 1) := Temp;
            end loop;
        end loop;
    else
        for I in 1 .. 10 loop
            for J in 1 .. 10 loop
                Temp := Matriz(I, J);
                Matriz(I, J) := Matriz(10 - J + 1, I);
                Matriz(10 - J + 1, I) := Temp;
            end loop;
        end loop;
    end if;
end RotarMatriz;

Matriz : array (1 .. 10, 1 .. 10) of Integer;
Direccion : Boolean := True;

begin
    RellenarMatriz(Matriz);
    ImprimirMatriz(Matriz);
    RotarMatriz(Matriz, Direccion);
    ImprimirMatriz(Matriz);
    RotarMatriz(Matriz, Direccion);
    ImprimirMatriz(Matriz);
end;
```

Este código en ADA realiza las siguientes acciones:

1. Declara un procedimiento llamado `ImprimirMatriz` que toma una matriz bidimensional de enteros y la imprime en la salida estándar.
2. Declara un procedimiento llamado `RellenarMatriz` que toma una matriz bidimensional de enteros y la rellena con ceros.
3. Declara un procedimiento llamado `RotarMatriz` que toma una matriz bidimensional de enteros y una dirección (derecha o izquierda) y rota la matriz en esa dirección.
4. Declara una matriz bidimensional de enteros llamada `Matriz` y una variable booleana llamada `Direccion` que se utiliza para indicar la dirección de rotación.
5. Llama al procedimiento `RellenarMatriz` para rellenar `Matriz` con ceros.
6. Llama al procedimiento `ImprimirMatriz` para imprimir `Matriz` en la salida estándar.
7. Llama al procedimiento `RotarMatriz` para rotar `Matriz` en la dirección especificada por `Direccion`.
8. Llama al procedimiento `ImprimirMatriz` para imprimir `Matriz` en la salida estándar.
9. Llama al procedimiento `RotarMatriz` para rotar `Matriz` en la dirección especificada por `Direccion`.
10. Llama al procedimiento `ImprimirMatriz` para imprimir `Matriz` en la salida estándar.

El resultado es que se imprime la matriz original, la matriz rotada a la derecha y la matriz rotada a la izquierda.