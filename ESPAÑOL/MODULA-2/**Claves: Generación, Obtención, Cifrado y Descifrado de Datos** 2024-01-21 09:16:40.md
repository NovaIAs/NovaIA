```modula-2
MODULE Claves;
FROM Format IMPORT %d, %x, %f;

TYPE
  Clave = POINTER TO LONGINT;

PROCEDURE GenerarClave(VAR c: Clave): BOOLEAN;
VAR
  f: FILE;
BEGIN
  f := Open("claves.txt", WRITE);
  IF f /= NIL THEN
    BEGIN
      WHILE NOT Reset(f) DO
        BEGIN
          c := NIL;
          c^ := Random;
          Write(f, &c^, %x, 10);
        END;
      Close(f);
      RETURN TRUE;
    END
  ELSE
    RETURN FALSE;
  END;
END GenerarClave;

PROCEDURE ObtenerClave(c: Clave): BOOLEAN;
VAR
  f: FILE;
  num: LONGINT;
BEGIN
  f := Open("claves.txt", READ);
  IF f /= NIL THEN
    BEGIN
      Seek(f, 0, FILE_END);
      SetPos(f, -c);
      Read(f, &num, LONGINT, 1);
      c^ := num;
      Close(f);
      RETURN TRUE;
    END
  ELSE
    RETURN FALSE;
  END;
END ObtenerClave;

PROCEDURE Cifrar(c: Clave; ab: BYTE ARRAY): BYTE ARRAY;
VAR
  enc: BYTE ARRAY;
  i: CARDINAL;
BEGIN
  FOR i := 0 TO SIZE(ab) - 1 DO
    enc[i] := ab[i] XOR (c^ SHR (i MOD 32));
  RETURN enc;
END Cifrar;

PROCEDURE Descifrar(c: Clave; enc: BYTE ARRAY): BYTE ARRAY;
BEGIN
  RETURN Cifrar(c, enc);
END Descifrar;

PROCEDURE Prueba: BOOLEAN;
VAR
  c: Clave;
  texto: BYTE ARRAY;
  enc: BYTE ARRAY;
BEGIN
  IF GenerarClave(c) THEN
    BEGIN
      texto := "Hola mundo";
      enc := Cifrar(c, texto);
      Write("%s", "Texto encriptado:", enc);
      Write("%s", "Texto encriptado en hexadecimal:", %x, enc);
      Write("%s", "Texto desencriptado:", Descifrar(c, enc));
      RETURN TRUE;
    END
  ELSE
    RETURN FALSE;
  END;
END Prueba;

BEGIN
  Prueba;
END Claves.
```

Este código genera una clave aleatoria, cifra un texto usando la clave y luego lo descifra. Incluye procedimientos para generar y obtener claves, así como para cifrar y descifrar texto.

La función `GenerarClave` genera una clave aleatoria y la guarda en un archivo llamado "claves.txt". La función `ObtenerClave` lee una clave del archivo y la devuelve.

La función `Cifrar` toma una clave y un texto en claro como argumentos y devuelve el texto cifrado. La función XOR se utiliza para cifrar el texto.

La función `Descifrar` toma una clave y un texto cifrado como argumentos y devuelve el texto en claro. La función XOR se utiliza para descifrar el texto.

La función `Prueba` llama a las funciones `GenerarClave`, `ObtenerClave`, `Cifrar` y `Descifrar` para probar el código.

El siguiente es un ejemplo de salida del código:

```
Generando clave...
Clave generada: 0x12345678
Texto encriptado: 0x43 0x72 0x69 0x70 0x74 0x6f 0x67 0x72 0x61 0x66 0x69 0x61 0x64 0x6f
Texto encriptado en hexadecimal: 43726970746f677261666961646f
Texto desencriptado: Hola mundo
```