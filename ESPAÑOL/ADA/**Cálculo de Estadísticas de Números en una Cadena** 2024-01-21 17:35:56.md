```ada
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;        use Ada.Float_Text_IO;

procedure Calcular_Estadisticas(Datos : Unbounded_String) is
    -- Declare las variables necesarias:
    -- Prom: promedio de los números en la cadena
    -- Cant_Numeros: cantidad de números en la cadena
    -- Suma: suma de los números en la cadena
    -- Menor: menor número en la cadena
    -- Mayor: mayor número en la cadena

    -- Inicialice las variables:
    Prom := 0.0;
    Cant_Numeros := 0;
    Suma := 0.0;
    Menor := Float'Last;
    Mayor := Float'First;

    -- Recorra la cadena y calcule las estadísticas:
    for I in Datos'Range loop
        -- Verifique si el carácter actual es un dígito decimal:
        if Character'Is_Digit(Datos(I)) then
            -- Obtenga el valor numérico del carácter:
            Num := To_Integer(Datos(I) - '0');

            -- Actualice las estadísticas:
            Suma := Suma + Float(Num);
            Cant_Numeros := Cant_Numeros + 1;
            if Num < Menor then
                Menor := Num;
            end if;
            if Num > Mayor then
                Mayor := Num;
            end if;
        end if;
    end loop;

    -- Calcule el promedio:
    if Cant_Numeros > 0 then
        Prom := Suma / Float(Cant_Numeros);
    end if;

    -- Imprima los resultados:
    Put_Line("Estadísticas de la cadena:");
    Put("Promedio: ");
    Put(Prom, 2, 1);
    New_Line;
    Put("Cantidad de números: ");
    Put(Cant_Numeros);
    New_Line;
    Put("Suma: ");
    Put(Suma, 0, 0);
    New_Line;
    Put("Menor número: ");
    Put(Menor);
    New_Line;
    Put("Mayor número: ");
    Put(Mayor);
    New_Line;
end Calcular_Estadisticas;

begin
    -- Obtenga la cadena del usuario:
    Put("Ingrese una cadena de números separados por comas: ");
    Datos := Get_Line;

    -- Calcule las estadísticas de la cadena:
    Calcular_Estadisticas(Datos);
end Ada_Main;
```

Explicación del código:

* El procedimiento `Calcular_Estadisticas` recibe una cadena de texto como parámetro y calcula las siguientes estadísticas:
    * Promedio de los números en la cadena.
    * Cantidad de números en la cadena.
    * Suma de los números en la cadena.
    * Menor número en la cadena.
    * Mayor número en la cadena.
* El procedimiento primero inicializa las variables necesarias para almacenar las estadísticas.
* Luego, recorre la cadena y comprueba si cada carácter es un dígito decimal. Si lo es, obtiene el valor numérico del carácter y actualiza las estadísticas.
* Después de recorrer toda la cadena, el procedimiento calcula el promedio de los números.
* Finalmente, el procedimiento imprime los resultados en la consola.

El código utiliza las siguientes unidades de Ada:

* `Ada.Text_IO`: proporciona funciones para la entrada y salida de texto.
* `Ada.Strings.Unbounded`: proporciona tipos y operaciones para cadenas de texto sin límite de tamaño.
* `Ada.Integer_Text_IO`: proporciona funciones para la entrada y salida de enteros.
* `Ada.Float_Text_IO`: proporciona funciones para la entrada y salida de números de punto flotante.