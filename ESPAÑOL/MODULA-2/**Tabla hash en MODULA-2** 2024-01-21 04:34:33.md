```modula-2
MODULE HashTable;
FROM Terminal IMPORT WriteString, WriteLn;
TYPE HashEntry = RECORD
    key: INTEGER;
    value: STRING;
END;

VAR table: ARRAY[100] OF HashEntry;
VAR tableSize: INTEGER := 100;

PROCEDURE Hash(key: INTEGER): INTEGER;
VAR i: INTEGER;
BEGIN
    i := key MOD tableSize;
    RETURN i;
END Hash;

PROCEDURE Insert(key: INTEGER; value: STRING);
VAR entry: HashEntry;
BEGIN
    entry.key := key;
    entry.value := value;

    table[Hash(key)] := entry;
END Insert;

PROCEDURE Find(key: INTEGER): STRING;
BEGIN
    RETURN table[Hash(key)].value;
END Find;

PROCEDURE PrintTable;
VAR i: INTEGER;
BEGIN
    FOR i := 0 TO tableSize - 1 DO
        WriteString(table[i].key AS STRING);
        WriteString(": ");
        WriteString(table[i].value);
        WriteLn;
    END;
END PrintTable;

BEGIN

    Insert(1, "Uno");
    Insert(2, "Dos");
    Insert(3, "Tres");

    PrintTable;
END HashTable.
```

Explicación:

El código crea una tabla hash en MODULA-2, una estructura de datos que permite almacenar y recuperar datos de forma eficiente.

La tabla hash está implementada como una matriz de registros, donde cada registro contiene una clave y un valor. La clave se utiliza para identificar el valor almacenado en el registro.

La función _Hash()_ calcula el índice del registro en el que se debe almacenar un nuevo valor, en función de la clave.

La función _Insert()_ inserta un nuevo valor en la tabla hash, utilizando la función _Hash()_ para calcular el índice del registro en el que se debe almacenar el valor.

La función _Find()_ recupera el valor asociado a una clave determinada, utilizando la función _Hash()_ para calcular el índice del registro en el que se encuentra el valor.

La función _PrintTable()_ imprime el contenido de la tabla hash, mostrando la clave y el valor de cada registro.

En este ejemplo, se crea una tabla hash con un tamaño de 100 registros. A continuación, se insertan tres valores en la tabla hash: "Uno" con la clave 1, "Dos" con la clave 2, y "Tres" con la clave 3.

Finalmente, se imprime el contenido de la tabla hash, mostrando la clave y el valor de cada registro.

El código utiliza varias características de MODULA-2, incluyendo registros, arrays, procedimientos, y funciones. También utiliza la palabra clave _FROM_ para importar procedimientos y funciones de otros módulos.