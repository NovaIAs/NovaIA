```modula-2
MODULE Proyecto;
FROM Terminal IMPORT WriteString, WriteLn, ReadString;

TYPE Arbol = RECORD
    dato: CHAR;
    hijoIzquierdo: Arbol;
    hijoDerecho: Arbol;
END;

PROCEDURE CrearArbol(A: VAR Arbol);
BEGIN
    A := NIL;
END CrearArbol;

PROCEDURE InsertarNodo(A: VAR Arbol; D: CHAR);
VAR
    Nodo: Arbol;
BEGIN
    IF A = NIL THEN
        A := NEW(Arbol);
        A.dato := D;
        A.hijoIzquierdo := NIL;
        A.hijoDerecho := NIL;
    ELSE
        IF D < A.dato THEN
            InsertarNodo(A.hijoIzquierdo, D);
        ELSE
            InsertarNodo(A.hijoDerecho, D);
        END
    END
END InsertarNodo;

PROCEDURE BuscarNodo(A: Arbol; D: CHAR): BOOLEAN;
VAR
    Encontrado: BOOLEAN;
BEGIN
    Encontrado := FALSE;
    IF A /= NIL THEN
        IF A.dato = D THEN
            Encontrado := TRUE;
        ELSE
            Encontrado := BuscarNodo(A.hijoIzquierdo, D) OR BuscarNodo(A.hijoDerecho, D);
        END
    END;
    RETURN Encontrado;
END BuscarNodo;

PROCEDURE MostrarArbol(A: Arbol);
BEGIN
    IF A /= NIL THEN
        MostrarArbol(A.hijoIzquierdo);
        WriteString(A.dato:1);
        MostrarArbol(A.hijoDerecho);
    END
END MostrarArbol;

PROCEDURE BorrarArbol(A: VAR Arbol);
BEGIN
    IF A /= NIL THEN
        BorrarArbol(A.hijoIzquierdo);
        BorrarArbol(A.hijoDerecho);
        DISPOSE(A);
    END
END BorrarArbol;

VAR
    A: Arbol;
    D: CHAR;
BEGIN
    CrearArbol(A);
    WriteString("Ingrese un dato para insertar en el árbol: ");
    D := ReadString;
    InsertarNodo(A, D);
    WriteString("El árbol ahora es: ");
    MostrarArbol(A);
    WriteString(NL);
    WriteString("Ingrese un dato para buscar en el árbol: ");
    D := ReadString;
    IF BuscarNodo(A, D) THEN
        WriteString("El dato se encontró en el árbol");
    ELSE
        WriteString("El dato no se encontró en el árbol");
    END;
    WriteString(NL);
    BorrarArbol(A);
END Proyecto.
```

Este es un código MODULA-2 que implementa un árbol binario de búsqueda. Un árbol binario de búsqueda es una estructura de datos que almacena datos en un árbol. Los datos se almacenan en nodos, y los nodos están conectados entre sí por ramas. Cada nodo tiene un hijo izquierdo y un hijo derecho.

El código primero define el tipo de dato Arbol, que es un registro que contiene tres campos: dato, hijoIzquierdo e hijoDerecho. El campo dato almacena el valor del nodo, mientras que los campos hijoIzquierdo e hijoDerecho almacenan los nodos hijos del nodo actual.

A continuación, el código define una serie de procedimientos para manipular el árbol binario de búsqueda. El procedimiento CrearArbol crea un árbol binario de búsqueda vacío. El procedimiento InsertarNodo inserta un nuevo nodo en el árbol binario de búsqueda. El procedimiento BuscarNodo busca un nodo en el árbol binario de búsqueda. El procedimiento MostrarArbol muestra el árbol binario de búsqueda. Y el procedimiento BorrarArbol borra el árbol binario de búsqueda.

El código también define una variable global llamada A, que es un árbol binario de búsqueda.

El código principal primero crea un árbol binario de búsqueda vacío. A continuación, el código solicita al usuario que ingrese un dato para insertar en el árbol binario de búsqueda. El código inserta el dato en el árbol binario de búsqueda. A continuación, el código muestra el árbol binario de búsqueda. A continuación, el código solicita al usuario que ingrese un dato para buscar en el árbol binario de búsqueda. El código busca el dato en el árbol binario de búsqueda. Si el dato se encuentra en el árbol, el código muestra un mensaje diciendo que el dato se encontró en el árbol. Si el dato no se encuentra en el árbol, el código muestra un mensaje diciendo que el dato no se encontró en el árbol. Por último, el código borra el árbol binario de búsqueda.