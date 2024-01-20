```modula-2
MODULE AgendaContactos;
FROM Formatos import Mostrar;
FROM Archivos import LeerFichero, EscribirFichero, FicheroExiste;

TYPE Nombre = ARRAY[1..20] OF CHAR;
TYPE Apellidos = ARRAY[1..40] OF CHAR;
TYPE Telefono = ARRAY[1..15] OF CHAR;
TYPE CorreoElectronico = ARRAY[1..50] OF CHAR;

TYPE Persona = RECORD
    nombre: Nombre;
    apellidos: Apellidos;
    telefono: Telefono;
    correoElectronico: CorreoElectronico;
END;

TYPE Agenda = ARRAY[1..MAX_PERSONAS] OF Persona;

VAR agenda: Agenda;
VAR numPersonas: INTEGER;

PROCEDURE LeerAgenda() (* Leer los datos de los contactos de un fichero *)
VAR fichero: TEXT;
VAR persona: Persona;
VAR i: INTEGER;
BEGIN
    IF FicheroExiste("agenda.txt") THEN
        fichero := LeerFichero("agenda.txt");
        numPersonas := 0;
        WHILE NOT EOF(fichero) DO
            Read(fichero, persona);
            numPersonas := numPersonas + 1;
            agenda[numPersonas] := persona;
        END;
        Close(fichero);
    END;
END LeerAgenda;

PROCEDURE GuardarAgenda() (* Guardar los datos de los contactos en un fichero *)
VAR fichero: TEXT;
VAR persona: Persona;
VAR i: INTEGER;
BEGIN
    fichero := EscribirFichero("agenda.txt");
    FOR i := 1 TO numPersonas DO
        persona := agenda[i];
        Write(fichero, persona);
    END;
    Close(fichero);
END GuardarAgenda;

PROCEDURE MostrarAgenda() (* Mostrar los datos de los contactos en la consola *)
VAR persona: Persona;
VAR i: INTEGER;
BEGIN
    FOR i := 1 TO numPersonas DO
        persona := agenda[i];
        Mostrar(persona.nombre, persona.apellidos, persona.correoElectronico);
    END;
END MostrarAgenda;

PROCEDURE AnyadirContacto() (* Añadir un nuevo contacto a la agenda *)
VAR persona: Persona;
BEGIN
    Write("Nombre: ");
    Read(persona.nombre);
    Write("Apellidos: ");
    Read(persona.apellidos);
    Write("Teléfono: ");
    Read(persona.telefono);
    Write("Correo electrónico: ");
    Read(persona.correoElectronico);
    numPersonas := numPersonas + 1;
    agenda[numPersonas] := persona;
END AnyadirContacto;

PROCEDURE EliminarContacto() (* Eliminar un contacto de la agenda *)
VAR nombre: Nombre;
VAR i: INTEGER;
BEGIN
    Write("Nombre del contacto a eliminar: ");
    Read(nombre);
    FOR i := 1 TO numPersonas DO
        IF agenda[i].nombre = nombre THEN
            FOR j := i + 1 TO numPersonas DO
                agenda[j - 1] := agenda[j];
            END;
            numPersonas := numPersonas - 1;
            EXIT;
        END;
    END;
END EliminarContacto;

PROCEDURE BuscarContacto() (* Buscar un contacto en la agenda *)
VAR nombre: Nombre;
VAR i: INTEGER;
BEGIN
    Write("Nombre del contacto a buscar: ");
    Read(nombre);
    FOR i := 1 TO numPersonas DO
        IF agenda[i].nombre = nombre THEN
            Mostrar(agenda[i].nombre, agenda[i].apellidos, agenda[i].correoElectronico);
            EXIT;
        END;
    END;
END BuscarContacto;

PROCEDURE Menu() (* Mostrar el menú de opciones *)
VAR opcion: CHAR;
BEGIN
    REPEAT
        WriteLn("Agenda de Contactos");
        WriteLn("-------------------");
        WriteLn("1. Mostrar agenda");
        WriteLn("2. Añadir contacto");
        WriteLn("3. Eliminar contacto");
        WriteLn("4. Buscar contacto");
        WriteLn("5. Salir");
        Write("Opción: ");
        Read(opcion);
        WriteLn;
        CASE opcion OF
            '1': MostrarAgenda();
            '2': AnyadirContacto();
            '3': EliminarContacto();
            '4': BuscarContacto();
            '5': EXIT;
            ELSE WriteLn("Opción no válida");
        END;
    UNTIL opcion = '5';
END Menu;

BEGIN
    LeerAgenda();
    Menu();
    GuardarAgenda();
END AgendaContactos.
```

**Explicación del código:**

1. **Módulos:** El programa utiliza módulos para organizar el código en unidades lógicas. Los módulos utilizados son:
    * **AgendaContactos:** Es el módulo principal del programa, donde se definen el tipo de datos **Agenda**, las variables globales **agenda** y **numPersonas**, y los procedimientos **LeerAgenda()**, **GuardarAgenda()**, **MostrarAgenda()**, **AnyadirContacto()**, **EliminarContacto()**, **BuscarContacto()** y **Menu()**.
    * **Formatos:** Contiene el procedimiento **Mostrar()**, que se utiliza para mostrar los datos de los contactos en la consola.
    * **Archivos:** Contiene los procedimientos **LeerFichero()**, **EscribirFichero()** y **FicheroExiste()**, que se utilizan para leer y escribir datos en un fichero.

2. **Tipos de datos:** Se definen los siguientes tipos de datos:
    * **Nombre:** Un tipo de datos de carácter de longitud 20, que se utiliza para almacenar el nombre de una persona.
    * **Apellidos:** Un tipo de datos de carácter de longitud 40, que se utiliza para almacenar los apellidos de una persona.
    * **Telefono:** Un tipo de datos de carácter de longitud 15, que se utiliza para almacenar el teléfono de una persona.
    * **CorreoElectronico:** Un tipo de datos de carácter de longitud 50, que se utiliza para almacenar el correo electrónico de una persona.
    * **Persona:** Un tipo de datos registro que contiene los campos **nombre**, **apellidos**, **telefono** y **correoElectronico**.
    * **Agenda:** Un tipo de datos array que contiene un máximo de **MAX_PERSONAS** elementos de tipo **Persona**.

3. **Variables globales:** Se definen las siguientes variables globales:
    * **agenda:** Una variable de tipo **Agenda**, que almacena los datos de los contactos.
    * **numPersonas:** Una variable de tipo entero, que almacena el número de contactos en la agenda.

4. **Procedimientos:** Se definen los siguientes procedimientos:
    * **LeerAgenda():** Lee los datos de los contactos de un fichero y los almacena en la variable **agenda**.
    * **GuardarAgenda():** Guarda los datos de los contactos de la variable **agenda** en un fichero.
    * **MostrarAgenda():** Muestra los datos de los contactos de la variable **agenda** en la consola.
    * **AnyadirContacto():** Añade un nuevo contacto a la variable **agenda**.
    * **EliminarContacto():** Elimina un contacto de la variable **agenda**.
    * **BuscarContacto():** Busca un contacto en la variable **agenda**.
    * **Menu():** Muestra el menú de opciones al usuario y le permite seleccionar una opción.

5. **Programa principal:** El programa principal se encuentra en el módulo **AgendaContactos**. El programa principal llama al procedimiento **LeerAgenda()** para leer los datos de los contactos de un fichero, luego llama al procedimiento **Menu()** para mostrar el menú de opciones al usuario y permitirle seleccionar una opción, y finalmente llama al procedimiento **GuardarAgenda()** para guardar los datos de los contactos en un fichero.