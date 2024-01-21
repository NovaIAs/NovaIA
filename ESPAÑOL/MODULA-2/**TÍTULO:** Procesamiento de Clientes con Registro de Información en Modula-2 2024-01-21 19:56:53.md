```modula-2
MODULE Main;
FROM FormatString IMPORT WriteString, WriteInt, WriteLn;

TYPE
  Estado = ENUMERATION (
    E_Inicio, E_Procesando, E_Fin
  );

  Cliente = RECORD
    nombre: ARRAY 20 OF CHAR;
    cedula: ARRAY 10 OF CHAR;
    estado: Estado;
  );

VAR
  clientes: ARRAY 10 OF Cliente;
  i: CARDINAL;

PROCEDURE ProcesarCliente(cliente: Cliente);
VAR
  nombre: ARRAY 20 OF CHAR;
  cedula: ARRAY 10 OF CHAR;
  estado: Estado;
BEGIN
  WriteString("Nombre: ");
  ReadString(nombre);
  WriteString("Cédula: ");
  ReadString(cedula);
  estado := E_Procesando;

  cliente.nombre := nombre;
  cliente.cedula := cedula;
  cliente.estado := estado;
END ProcesarCliente;

BEGIN
  FOR i := 0 TO 9 DO
    ProcesarCliente(clientes[i]);
  END;

  WriteString("Clientes procesados: ");
  WriteInt(i + 1, 0);
  WriteLn;

  FOR i := 0 TO 9 DO
    WriteString("Nombre: ");
    WriteString(clientes[i].nombre);
    WriteLn;
    WriteString("Cédula: ");
    WriteString(clientes[i].cedula);
    WriteLn;
    WriteString("Estado: ");
    WriteString(clientes[i].estado);
    WriteLn;
  END;
END Main.
```

Explicación del código:

* El módulo `Main` contiene el código principal del programa.
* El tipo `Estado` define los diferentes estados en los que se puede encontrar un cliente: `E_Inicio`, `E_Procesando` y `E_Fin`.
* El tipo `Cliente` define la estructura de un cliente, incluyendo su nombre, cédula y estado.
* La variable `clientes` es un array de 10 clientes.
* El procedimiento `ProcesarCliente` procesa un cliente, solicitando su nombre y cédula al usuario y estableciendo su estado en `E_Procesando`.
* El código principal del programa itera sobre el array de clientes, procesando cada cliente y mostrando su información en la consola.