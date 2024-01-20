```modula-2
MODULE OrdenarLista;
FROM Terminal IMPORT WriteString, WriteLn, ReadString;
FROM Ordenamiento IMPORT Lista, Seleccion, Burbuja;

TYPE
  Elemento = CARDINAL;
  ListaDeElementos = REF Lista;

PROCEDURE ImprimirLista(lista: ListaDeElementos);
VAR
  elemento: Elemento;
BEGIN
  WriteString("Lista: ");
  WHILE lista # NIL DO
  BEGIN
    elemento := lista^;
    WriteString(elemento);
    Write(" ");
    lista := lista^.sig;
  END;
  WriteLn;
END ImprimirLista;

PROCEDURE LeerLista(lista: ListaDeElementos);
VAR
  elemento: Elemento;
  cadena: ARRAY 16 OF CHAR;
BEGIN
  WriteString("Introduzca los elementos de la lista (separados por espacios): ");
  ReadString(cadena);
  lista := NIL;
  WHILE cadena # "" DO
  BEGIN
    elemento := VAL(cadena, 10);
    lista := InsertarFinal(lista, elemento);
    cadena := SUBSTRING(cadena, 2, SIZE(cadena) - 1);
  END;
END LeerLista;

PROCEDURE OrdenarSeleccion(lista: ListaDeElementos);
BEGIN
  Seleccion(lista);
END OrdenarSeleccion;

PROCEDURE OrdenarBurbuja(lista: ListaDeElementos);
BEGIN
  Burbuja(lista);
END OrdenarBurbuja;

PROCEDURE Principal;
VAR
  lista: ListaDeElementos;
  opcion: CHAR;
BEGIN
    LOOP
      WriteString("Elija una opción:\n");
      WriteString("1. Introducir lista\n");
      WriteString("2. Ordenar por selección\n");
      WriteString("3. Ordenar por burbuja\n");
      WriteString("4. Salir\n");
      ReadString(opcion);

      CASE opcion OF
        "1": LeerLista(lista);
        "2": OrdenarSeleccion(lista);
        "3": OrdenarBurbuja(lista);
        "4": EXIT;
        ELSE: WriteLn("Opción inválida, intente de nuevo");
      END;

      ImprimirLista(lista);
    END;
END Principal.
```

Explicación del código:

* El módulo `OrdenarLista` contiene las definiciones de los tipos, variables y procedimientos utilizados en el programa.
* El tipo `Elemento` se define como un tipo entero sin signo (CARDINAL).
* El tipo `ListaDeElementos` se define como una referencia a una lista de elementos.
* El procedimiento `ImprimirLista` se utiliza para imprimir una lista de elementos.
* El procedimiento `LeerLista` se utiliza para leer una lista de elementos del usuario.
* El procedimiento `OrdenarSeleccion` se utiliza para ordenar una lista de elementos utilizando el algoritmo de selección.
* El procedimiento `OrdenarBurbuja` se utiliza para ordenar una lista de elementos utilizando el algoritmo de burbuja.
* El procedimiento `Principal` es el punto de entrada del programa.
* En el procedimiento `Principal`, se solicita al usuario que elija una opción entre introducir una lista, ordenar la lista por selección, ordenar la lista por burbuja o salir del programa.
* En función de la opción elegida, se llama al procedimiento correspondiente para realizar la operación seleccionada.
* El programa imprime la lista ordenada después de cada operación.