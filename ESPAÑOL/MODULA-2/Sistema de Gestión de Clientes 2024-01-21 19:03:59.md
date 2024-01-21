```modula-2
MODULE SistemaDeGestionDeClientes;
FROM Terminal IMPORT WriteString, ReadString;

TYPE Cliente = RECORD
    nombre: ARRAY[0..79] OF CHAR;
    direccion: ARRAY[0..79] OF CHAR;
    telefono: ARRAY[0..15] OF CHAR;
END;

VAR clientes: ARRAY[0..99] OF Cliente;
VAR numClientes: CARDINAL;

PROCEDURE LeerCliente(VAR c: Cliente);
BEGIN
    WriteString("Nombre: ");
    ReadString(c.nombre);
    WriteString("Dirección: ");
    ReadString(c.direccion);
    WriteString("Teléfono: ");
    ReadString(c.telefono);
END LeerCliente;

PROCEDURE EscribirCliente(c: Cliente);
BEGIN
    WriteString(c.nombre);
    WriteString(" - ");
    WriteString(c.direccion);
    WriteString(" - ");
    WriteString(c.telefono);
    WriteString(ENDL);
END EscribirCliente;

PROCEDURE AgregarCliente;
VAR c: Cliente;
BEGIN
    IF numClientes >= 100 THEN
        WriteString("No se pueden agregar más clientes.");
    ELSE
        LeerCliente(c);
        clientes[numClientes] := c;
        numClientes := numClientes + 1;
    END
END AgregarCliente;

PROCEDURE ListarClientes;
VAR i: CARDINAL;
BEGIN
    FOR i := 0 TO numClientes - 1 DO
        EscribirCliente(clientes[i]);
    END
END ListarClientes;

PROCEDURE BuscarCliente(nombre: ARRAY[0..79] OF CHAR): CARDINAL;
VAR i: CARDINAL;
BEGIN
    FOR i := 0 TO numClientes - 1 DO
        IF clientes[i].nombre = nombre THEN
            RETURN i;
        END
    END;
    RETURN -1;
END BuscarCliente;

PROCEDURE ModificarCliente(nombre: ARRAY[0..79] OF CHAR);
VAR i: CARDINAL;
BEGIN
    i := BuscarCliente(nombre);
    IF i = -1 THEN
        WriteString("Cliente no encontrado.");
    ELSE
        LeerCliente(clientes[i]);
    END
END ModificarCliente;

PROCEDURE EliminarCliente(nombre: ARRAY[0..79] OF CHAR);
VAR i: CARDINAL;
BEGIN
    i := BuscarCliente(nombre);
    IF i = -1 THEN
        WriteString("Cliente no encontrado.");
    ELSE
        FOR i := i + 1 TO numClientes - 1 DO
            clientes[i - 1] := clientes[i];
        END;
        numClientes := numClientes - 1;
    END
END EliminarCliente;

PROCEDURE Menu;
VAR opcion: CHAR;
BEGIN
    REPEAT
        WriteString(ENDL);
        WriteString("1. Agregar cliente");
        WriteString(ENDL);
        WriteString("2. Listar clientes");
        WriteString(ENDL);
        WriteString("3. Buscar cliente");
        WriteString(ENDL);
        WriteString("4. Modificar cliente");
        WriteString(ENDL);
        WriteString("5. Eliminar cliente");
        WriteString(ENDL);
        WriteString("6. Salir");
        WriteString(ENDL);
        WriteString("Opción: ");
        ReadString(opcion);
        WriteString(ENDL);

        CASE opcion OF
            '1': AgregarCliente;
            '2': ListarClientes;
            '3':
                VAR nombre: ARRAY[0..79] OF CHAR;
                BEGIN
                    WriteString("Nombre del cliente: ");
                    ReadString(nombre);
                    i := BuscarCliente(nombre);
                    IF i = -1 THEN
                        WriteString("Cliente no encontrado.");
                    ELSE
                        EscribirCliente(clientes[i]);
                    END
                END;
            '4':
                VAR nombre: ARRAY[0..79] OF CHAR;
                BEGIN
                    WriteString("Nombre del cliente: ");
                    ReadString(nombre);
                    ModificarCliente(nombre);
                END;
            '5':
                VAR nombre: ARRAY[0..79] OF CHAR;
                BEGIN
                    WriteString("Nombre del cliente: ");
                    ReadString(nombre);
                    EliminarCliente(nombre);
                END;
            '6': EXIT;
            ELSE: WriteString("Opción no válida.");
        END
    UNTIL opcion = '6'
END Menu;

BEGIN
    numClientes := 0;
    Menu;
END SistemaDeGestionDeClientes.
```

Explicación del código:

* El programa define un tipo de datos personalizado llamado `Cliente` que contiene información sobre un cliente, incluyendo su nombre, dirección y teléfono.
* El programa también define un array de clientes llamado `clientes` que puede almacenar hasta 100 clientes.
* El programa contiene varias funciones y procedimientos para agregar, listar, buscar, modificar y eliminar clientes de la base de datos.
* El procedimiento `Menu` muestra un menú de opciones al usuario y le permite seleccionar una opción para agregar, listar, buscar, modificar o eliminar clientes.
* El programa utiliza el procedimiento `LeerCliente` para leer información sobre un cliente desde la entrada estándar.
* El procedimiento `EscribirCliente` se utiliza para mostrar información sobre un cliente en la salida estándar.
* El procedimiento `AgregarCliente` agrega un nuevo cliente a la base de datos.
* El procedimiento `ListarClientes` lista todos los clientes de la base de datos.
* El procedimiento `BuscarCliente` busca un cliente por su nombre y devuelve su índice en el array `clientes`.
* El procedimiento `ModificarCliente` modifica un cliente existente en la base de datos.
* El procedimiento `EliminarCliente` elimina un cliente de la base de datos.