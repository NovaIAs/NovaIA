```modula-2
MODULE Main;
FROM FormatString IMPORT WriteString;
FROM Terminal IMPORT WriteLn;

PROCEDURE Toggle(VAR Value: BOOLEAN);
BEGIN
    Value := NOT Value;
END Toggle;

PROCEDURE ShowMenu(VAR Exit: BOOLEAN);
VAR
    Option: CHAR;
    Value: BOOLEAN := FALSE;
BEGIN
    LOOP
        WriteString("Opciones:");
        WriteString(" [A]ctivar/Desactivar");
        WriteString(" [S]alir");
        WriteLn;
        WriteString("> ");
        ReadChar(Option);

        CASE Option OF
            'A', 'a': BEGIN
                Toggle(Value);
                WriteString("Valor: ");
                IF Value THEN WriteString("Verdadero") ELSE WriteString("Falso");
                WriteLn;
            END;

            'S', 's': BEGIN
                Exit := TRUE;
                WriteLn("Saliendo...");
            END;

            ELSE: BEGIN
                WriteString("Opción inválida. Intente nuevamente.");
                WriteLn;
            END;
        END;

        IF Exit THEN LEAVE;
    END;
END ShowMenu;

BEGIN
    VAR Exit: BOOLEAN := FALSE;
    ShowMenu(Exit);
END Main.
```

Explicación:

* El código define un módulo llamado `Main` que contiene la aplicación principal.

* El código importa los módulos `FormatString` y `Terminal` para utilizar funciones de formato de cadenas y entrada/salida de texto.

* El código define un procedimiento llamado `Toggle` que recibe una variable booleana por referencia y la invierte (cambia su valor de `VERDADERO` a `FALSO` o viceversa).

* El código define un procedimiento llamado `ShowMenu` que muestra un menú de opciones al usuario y le permite interactuar con la aplicación.

* El código define una variable local booleana llamada `Value` que se utiliza para almacenar el valor que se quiere activar o desactivar.

* El procedimiento `ShowMenu` entra en un bucle `LOOP` infinito hasta que el usuario elige salir.

* Dentro del bucle, se muestra el menú de opciones al usuario y se lee la opción elegida.

* Dependiendo de la opción elegida, se realiza la acción correspondiente:

    * Si el usuario elige 'A' o 'a', se llama al procedimiento `Toggle` para invertir el valor de `Value` y se muestra el valor actual.

    * Si el usuario elige 'S' o 's', se asigna `VERDADERO` a la variable `Exit` para indicar que el usuario quiere salir de la aplicación y se muestra un mensaje de salida.

    * Si el usuario elige cualquier otra opción, se muestra un mensaje de error.

* Si el usuario elige salir, se abandona el bucle `LOOP` mediante la instrucción `LEAVE`.

* El código principal de la aplicación llama al procedimiento `ShowMenu` y espera hasta que el usuario decida salir.