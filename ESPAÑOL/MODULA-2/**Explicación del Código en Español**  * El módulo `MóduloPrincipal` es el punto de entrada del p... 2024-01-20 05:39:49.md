```modula-2
MODULE MóduloPrincipal;
FROM Clases IMPORT ClaseBase, ClaseDerivada;
TYPE
  Subrango = ARRAY [0..10] OF INTEGER;
  Registro = RECORD
    nombre: ARRAY [0..19] OF CHAR;
    edad: INTEGER;
    subrango: Subrango;
  END;
PROCEDURE PruebaDeClases;
VAR
  objeto1: ClaseBase;
  objeto2: ClaseDerivada;
BEGIN
  Nuevo(objeto1);
  Nuevo(objeto2);

  objeto1.LlamarMetodo();
  objeto2.LlamarMetodo();

  Liberar(objeto1);
  Liberar(objeto2);
END PruebaDeClases;

PROCEDURE PruebaDeTiposDeDatos;
VAR
  subrango1: Subrango;
  registro1: Registro;
  i: LONGINT;
BEGIN
  FOR i := 0 TO 10 DO
    subrango1[i] := i;
  END;

  registro1.nombre := "Juan Pérez";
  registro1.edad := 30;
  registro1.subrango := subrango1;

  Escribir("Nombre: ", registro1.nombre);
  Escribir("Edad: ", registro1.edad);
  Escribir("Subrango: ");
  FOR i := 0 TO 10 DO
    Escribir(registro1.subrango[i], " ");
  END;
  EscribirFin();
END PruebaDeTiposDeDatos;

PROCEDURE Main;
BEGIN
  PruebaDeClases;
  PruebaDeTiposDeDatos;
END Main.
```

Explicación del código:

* El módulo `MóduloPrincipal` es el punto de entrada del programa.
* El módulo `Clases` define dos clases, `ClaseBase` y `ClaseDerivada`.
* La clase `ClaseBase` tiene un método llamado `LlamarMetodo()`.
* La clase `ClaseDerivada` hereda de la clase `ClaseBase` y tiene un método adicional llamado `LlamarMetodoDerivado()`.
* El procedimiento `PruebaDeClases` crea objetos de las clases `ClaseBase` y `ClaseDerivada` y llama a sus métodos.
* El procedimiento `PruebaDeTiposDeDatos` crea variables de los tipos de datos `Subrango` y `Registro` y las inicializa con valores.
* El procedimiento `Main` llama a los procedimientos `PruebaDeClases` y `PruebaDeTiposDeDatos`.