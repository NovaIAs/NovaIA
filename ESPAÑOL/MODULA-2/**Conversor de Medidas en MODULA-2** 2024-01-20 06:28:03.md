```modula-2
MODULE ConversorDeMedidas;
FROM InOut IMPORT WriteString, WriteLn, ReadInt, ReadChar;

TYPE Equivalencia = ARRAY[1..2] OF REAL;

VAR equivalencias: ARRAY[1..6] OF Equivalencia;

PROCEDURE InitEquivalencias;
VAR i: INTEGER;
BEGIN
  equivalencias[1]:= [1, 0.621371];
  equivalencias[2]:= [0.3048, 1];
  equivalencias[3]:= [2.54, 0.393701];
  equivalencias[4]:= [0.453592, 1];
  equivalencias[5]:= [0.264172, 1];
  equivalencias[6]:= [0.0283495, 1];
  FOR i:= 1 TO 6 DO
    WriteString(equivalencias[i][1]:6:3);
    WriteString(" = ");
    WriteLn(equivalencias[i][2]:6:3)
  END
END InitEquivalencias;

PROCEDURE Cabecera;
BEGIN
  WriteString("Conversor de medidas");
  WriteLn("");
  WriteLn("1) Metros a pies");
  WriteLn("2) Pies a metros");
  WriteLn("3) Pulgadas a centímetros");
  WriteLn("4) Libras a kilogramos");
  WriteLn("5) Galones a litros");
  WriteLn("6) Onzas a gramos");
  WriteLn("");
  WriteLn("Elige una opción: ");
END Cabecera;

PROCEDURE GetOpcion: INTEGER;
VAR opcion: CHAR;
BEGIN
  opcion:= ReadChar;
  RETURN ORD(opcion) - ORD('0')
END GetOpcion;

PROCEDURE GetValor: REAL;
VAR valor: REAL;
BEGIN
  valor:= ReadInt;
  RETURN valor
END GetValor;

PROCEDURE Convierte(valor: REAL; equivalencia: Equivalencia): REAL;
BEGIN
  RETURN valor * equivalencia[2]
END Convierte;

PROCEDURE MuestraResultado(valor: REAL; equivalencia: Equivalencia);
BEGIN
  WriteString(valor:0:1);
  WriteString(" ");
  WriteString(equivalencia[1]:3);
  WriteLn(" = ");
  WriteString(Convierte(valor, equivalencia):0:1);
  WriteString(" ");
  WriteString(equivalencia[2]:3)
END MuestraResultado;

PROCEDURE Inicia;
VAR opcion: INTEGER;
    valor: REAL;
BEGIN
  InitEquivalencias;
  REPEAT
    Cabecera;
    opcion:= GetOpcion;
    WriteLn("");

    IF opcion >= 1 AND opcion <= 6 THEN
      valor:= GetValor;
      MuestraResultado(valor, equivalencias[opcion])
    ELSE
      WriteLn("Opción no válida")
    END;

    WriteLn("");
    WriteLn("¿Quieres continuar? (S/N)");
    opcion:= GetOpcion;
  UNTIL opcion = ORD('N') - ORD('0')
END Inicia;

BEGIN
  Inicia
END ConversorDeMedidas.
```

Explicación del código:

* El código está escrito en MODULA-2, un lenguaje de programación diseñado para aplicaciones de sistemas y de tiempo real.
* El programa es un conversor de medidas que permite convertir entre diferentes unidades de medida.
* El programa consta de varios módulos, cada uno de los cuales se describe a continuación:

  * El módulo `ConversorDeMedidas` es el módulo principal del programa. Contiene el código principal del programa, así como las declaraciones de los módulos secundarios.
  * El módulo `InOut` contiene las funciones de entrada y salida del programa. Estas funciones permiten al programa leer y escribir datos desde y hacia la consola.
  * El módulo `Equivalencia` contiene las definiciones de las equivalencias entre las diferentes unidades de medida.
  * El módulo `InitEquivalencias` inicializa las equivalencias entre las diferentes unidades de medida.
  * El módulo `Cabecera` muestra la cabecera del programa.
  * El módulo `GetOpcion` obtiene la opción seleccionada por el usuario.
  * El módulo `GetValor` obtiene el valor que el usuario desea convertir.
  * El módulo `Convierte` convierte el valor introducido por el usuario a la unidad de medida deseada.
  * El módulo `MuestraResultado` muestra el resultado de la conversión.
  * El módulo `Inicia` inicia el programa.

* El programa comienza llamando al módulo `Inicia`, que inicializa el programa y muestra la cabecera.
* A continuación, el programa llama al módulo `GetOpcion` para obtener la opción seleccionada por el usuario.
* Si la opción seleccionada es válida, el programa llama al módulo `GetValor` para obtener el valor que el usuario desea convertir.
* A continuación, el programa llama al módulo `Convierte` para convertir el valor introducido por el usuario a la unidad de medida deseada.
* Por último, el programa llama al módulo `MuestraResultado` para mostrar el resultado de la conversión.
* El programa se repite hasta que el usuario introduce la opción `N` para salir del programa.