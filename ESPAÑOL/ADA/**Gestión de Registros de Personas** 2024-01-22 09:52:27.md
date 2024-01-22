```ada
-- Definición del tipo de datos
type Registro is record
    Nombre      : String(1..20);
    Apellido    : String(1..20);
    Edad        : Integer;
    Salario     : Float;
end record;

-- Definición de un tipo de datos enumerado
type EstadoCivil is (Soltero, Casado, Divorciado, Viudo);

-- Definición de la subrutina principal
procedure Principal is
    -- Declaración de variables
    ListaRegistros : array (1..10) of Registro;
    Opcion         : Integer;
    RegistroNuevo  : Registro;

begin
    -- Inicialización del array
    for I in 1..10 loop
        ListaRegistros(I) := (Nombre => "", Apellido => "", Edad => 0, Salario => 0.0);
    end loop;

    -- Bucle principal
    loop
        -- Mostrar el menú de opciones
        Put_Line("Menú de opciones:");
        Put_Line("1. Agregar un registro");
        Put_Line("2. Mostrar todos los registros");
        Put_Line("3. Buscar un registro por nombre");
        Put_Line("4. Salir");
        Put("Opción: ");
        Get(Opcion);

        -- Ejecutar la opción seleccionada
        case Opcion is
            when 1 =>
                -- Agregar un nuevo registro
                Put("Nombre: ");
                Get_Line(RegistroNuevo.Nombre);
                Put("Apellido: ");
                Get_Line(RegistroNuevo.Apellido);
                Put("Edad: ");
                Get(RegistroNuevo.Edad);
                Put("Salario: ");
                Get(RegistroNuevo.Salario);
                ListaRegistros(10) := RegistroNuevo;
            when 2 =>
                -- Mostrar todos los registros
                for I in 1..10 loop
                    Put_Line(ListaRegistros(I).Nombre & " " & ListaRegistros(I).Apellido &
                            " (" & ListaRegistros(I).Edad & " años, $" &
                            ListaRegistros(I).Salario & ")");
                end loop;
            when 3 =>
                -- Buscar un registro por nombre
                Put("Nombre: ");
                Get_Line(RegistroNuevo.Nombre);
                for I in 1..10 loop
                    if ListaRegistros(I).Nombre = RegistroNuevo.Nombre then
                        Put_Line(ListaRegistros(I).Nombre & " " & ListaRegistros(I).Apellido &
                                " (" & ListaRegistros(I).Edad & " años, $" &
                                ListaRegistros(I).Salario & ")");
                        exit;
                    end if;
                end loop;
            when 4 =>
                -- Salir del programa
                exit;
            otherwise =>
                -- Opción no válida
                Put_Line("Opción no válida");
        end case;

        -- Esperar a que el usuario presione una tecla
        Get;
    end loop;
end Principal;
```

**Explicación del código:**

* El código define un tipo de datos llamado `Registro` que tiene cuatro campos: `Nombre`, `Apellido`, `Edad` y `Salario`.
* También define un tipo de datos enumerado llamado `EstadoCivil` que tiene cuatro valores posibles: `Soltero`, `Casado`, `Divorciado` y `Viudo`.
* La subrutina principal `Principal` se ejecuta cuando se inicia el programa.
* La subrutina `Principal` declara un array llamado `ListaRegistros` que tiene 10 elementos de tipo `Registro`.
* La subrutina `Principal` también declara tres variables: `Opcion`, `RegistroNuevo` y `Estado`.
* La subrutina `Principal` muestra un menú de opciones al usuario y le pide que seleccione una opción.
* Según la opción seleccionada, la subrutina `Principal` ejecuta la acción correspondiente.
* La opción 1 permite al usuario agregar un nuevo registro al array `ListaRegistros`.
* La opción 2 muestra todos los registros del array `ListaRegistros`.
* La opción 3 permite al usuario buscar un registro en el array `ListaRegistros` por nombre.
* La opción 4 permite al usuario salir del programa.
* La subrutina `Principal` espera a que el usuario presione una tecla antes de continuar.
* El código utiliza las siguientes instrucciones de ADA:

    * `Put_Line`: Imprime una línea de texto en la consola.
    * `Put`: Imprime un mensaje en la consola.
    * `Get`: Lee un valor de la consola.
    * `case`: Instrucción de selección que ejecuta una acción según el valor de una variable.
    * `for`: Bucle que se ejecuta un número determinado de veces.
    * `exit`: Sale del bucle o de la subrutina actual.