```modula-2
MODULE StringConvertir;
FROM FormatString IMPORT *;
FROM String IMPORT String, Length;

VAR s: ARRAY 1 .. 256 OF CHAR;

PROCEDURE Convertir(s: ARRAY OF CHAR);
VAR i, l: CARDINAL;
    c: CHAR;
BEGIN
    l := Length(s);
    FOR i := 1 TO l DO
        c := s[i];
        IF c >= 'A' AND c <= 'Z' THEN
            s[i] := c + 32;
        ELSE IF c >= 'a' AND c <= 'z' THEN
            s[i] := c - 32;
        END;
    END;
END Convertir;

PROCEDURE Imprimir(s: ARRAY OF CHAR);
VAR i, l: CARDINAL;
BEGIN
    l := Length(s);
    FOR i := 1 TO l DO
        WRITE(s[i]);
    END;
END Imprimir;

BEGIN
    WriteString("Ingrese una cadena de caracteres: ");
    ReadString(s);
    Convertir(s);
    Imprimir(s);
    WriteString("\n");
END StringConvertir.
```

Explicación del código:

* El módulo `StringConvertir` define un procedimiento `Convertir` que convierte las letras mayúsculas en minúsculas y las letras minúsculas en mayúsculas en una cadena de caracteres.
* El procedimiento `Imprimir` imprime una cadena de caracteres en la consola.
* La cadena de caracteres que se desea convertir se introduce por el usuario mediante la función `ReadString`.
* El procedimiento `Convertir` utiliza un bucle `FOR` para recorrer cada carácter de la cadena de caracteres y convertirlo, si es necesario.
* El procedimiento `Imprimir` utiliza un bucle `FOR` para imprimir cada carácter de la cadena de caracteres en la consola.
* El código utiliza la unidad `FormatString` para leer y escribir cadenas de caracteres.
* El código utiliza la unidad `String` para obtener la longitud de una cadena de caracteres.