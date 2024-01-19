```ada
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Complejo is

    type Estructura is record
        Nombre : String(1 .. 20);
        Edad : Integer;
        Estatura : Float;
    end record;

    type Lista is array (Integer range <>) of Estructura;

    function Crear_Lista(Dimension : Integer) return Lista is
        Lista_Nueva : Lista(1 .. Dimension);
    begin
        return Lista_Nueva;
    end Crear_Lista;

    function Agregar_Elemento(Lista : Lista; Persona : Estructura; Posicion : Integer) return Lista is
        Lista_Nueva : Lista(1 .. Lista'Length + 1);
    begin
        for I in Lista'Range loop
            Lista_Nueva(I) := Lista(I);
        end loop;

        Lista_Nueva(Posicion) := Persona;

        return Lista_Nueva;
    end Agregar_Elemento;

    function Buscar_Elemento(Lista : Lista; Nombre : String) return Integer is
        Posicion : Integer := 1;
    begin
        while Posicion <= Lista'Length and Lista(Posicion).Nombre /= Nombre loop
            Posicion := Posicion + 1;
        end loop;

        if Posicion > Lista'Length then
            return 0;
        else
            return Posicion;
        end if;
    end Buscar_Elemento;

    function Eliminar_Elemento(Lista : Lista; Posicion : Integer) return Lista is
        Lista_Nueva : Lista(1 .. Lista'Length - 1);
        I : Integer := 1;
    begin
        for J in 1 .. Lista'Length loop
            if J /= Posicion then
                Lista_Nueva(I) := Lista(J);
                I := I + 1;
            end if;
        end loop;

        return Lista_Nueva;
    end Eliminar_Elemento;

    function Ordenar_Lista(Lista : Lista) return Lista is
        Lista_Nueva : Lista := Lista;
        I : Integer := 1;
        J : Integer := 1;
        Aux : Estructura;
    begin
        for I in Lista_Nueva'Range loop
            for J in I + 1 .. Lista_Nueva'Length loop
                if Lista_Nueva(I).Nombre > Lista_Nueva(J).Nombre then
                    Aux := Lista_Nueva(I);
                    Lista_Nueva(I) := Lista_Nueva(J);
                    Lista_Nueva(J) := Aux;
                end if;
            end loop;
        end loop;

        return Lista_Nueva;
    end Ordenar_Lista;

    Lista_Personas : Lista := Crear_Lista(5);
    Posicion : Integer := 0;
    Nombre : String;
    Edad : Integer;
    Estatura : Float;
    Opcion : Integer;

begin
    loop
        Put_Line("1. Agregar persona");
        Put_Line("2. Buscar persona");
        Put_Line("3. Eliminar persona");
        Put_Line("4. Ordenar lista");
        Put_Line("5. Salir");
        New_Line;
        Put("Opción: ");
        Get(Opcion);
        New_Line;

        case Opcion is
            when 1 =>
                Put("Nombre: ");
                Get_Line(Nombre);
                New_Line;

                Put("Edad: ");
                Get(Edad);
                New_Line;

                Put("Estatura: ");
                Get(Estatura);
                New_Line;

                Lista_Personas := Agregar_Elemento(Lista_Personas, Estructura'(Nombre, Edad, Estatura), Lista_Personas'Length);

            when 2 =>
                Put("Nombre a buscar: ");
                Get_Line(Nombre);
                New_Line;

                Posicion := Buscar_Elemento(Lista_Personas, Nombre);

                if Posicion = 0 then
                    Put_Line("Persona no encontrada");
                else
                    Put_Line("Persona encontrada:");
                    Put_Line("Nombre: " & Lista_Personas(Posicion).Nombre);
                    Put_Line("Edad: " & Integer'Image(Lista_Personas(Posicion).Edad));
                    Put_Line("Estatura: " & Float'Image(Lista_Personas(Posicion).Estatura));
                end if;
                New_Line;

            when 3 =>
                Put("Posición de la persona a eliminar: ");
                Get(Posicion);
                New_Line;

                Lista_Personas := Eliminar_Elemento(Lista_Personas, Posicion);

            when 4 =>
                Lista_Personas := Ordenar_Lista(Lista_Personas);

            when 5 =>
                exit;

            others =>
                Put_Line("Opción no válida");
                New_Line;
        end case;
    end loop;
end Complejo;
```

Explicación del código:

* Se define un tipo de registro llamado `Estructura` que contiene los datos de una persona: nombre, edad y estatura.
* Se define un tipo de array llamado `Lista` que contiene una colección de estructuras `Estructura`.
* Se definen varias funciones para crear, agregar, buscar, eliminar y ordenar listas de personas.
* Se define una variable llamada `Lista_Personas` de tipo `Lista` que se utilizará para almacenar los datos de las personas.
* Se define una variable llamada `Posicion` de tipo `Integer` que se utilizará para almacenar la posición de una persona en la lista.
* Se definen varias variables de tipo `String`, `Integer` y `Float` para almacenar los datos de una persona.
* Se define una variable llamada `Opcion` de tipo `Integer` que se utilizará para almacenar la opción seleccionada por el usuario.
* Se define un bucle `loop` que se utilizará para mostrar el menú de opciones al usuario y recibir su selección.
* Se define un `case` que se utilizará para ejecutar la acción correspondiente a la opción seleccionada por el usuario.
* Se definen varias funciones para crear, agregar, buscar, eliminar y ordenar listas de personas.
* Se define una variable llamada `Lista_Personas` de tipo `Lista` que se utilizará para almacenar los datos de las personas.
* Se define una variable llamada `Posicion` de tipo `Integer` que se utilizará para almacenar la posición de una persona en la lista.
* Se definen varias variables de tipo `String`, `Integer` y `Float` para almacenar los datos de una persona.
* Se define una variable llamada `Opcion` de tipo `Integer` que se utilizará para almacenar la opción seleccionada por el usuario.
* Se define un bucle `loop` que se utilizará para mostrar el menú de opciones al usuario y recibir su selección.
* Se define un `case` que se utilizará para ejecutar la acción correspondiente a la opción seleccionada por el usuario.