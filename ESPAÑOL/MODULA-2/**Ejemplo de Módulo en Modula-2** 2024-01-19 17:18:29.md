```modula-2
MODULE ModuloDeEjemplo;

FROM Terminal IMPORT WriteString, WriteLn;

TYPE
  TipoEntero = CARDINAL;
  TipoReal = REAL;
  TipoTexto = ARRAY OF CHAR;
  TipoEstructura = RECORD
    Entero: TipoEntero;
    Real: TipoReal;
    Texto: TipoTexto;
  END;

VAR
  Entero: TipoEntero;
  Real: TipoReal;
  Texto: TipoTexto;
  Estructura: TipoEstructura;

BEGIN
  Entero := 10;
  Real := 3.14;
  Texto := "Hola, mundo!";

  Estructura.Entero := 20;
  Estructura.Real := 6.28;
  Estructura.Texto := "Adiós, mundo!";

  WriteString("Entero: ");
  WriteLn(Entero);

  WriteString("Real: ");
  WriteLn(Real);

  WriteString("Texto: ");
  WriteLn(Texto);

  WriteString("Estructura.Entero: ");
  WriteLn(Estructura.Entero);

  WriteString("Estructura.Real: ");
  WriteLn(Estructura.Real);

  WriteString("Estructura.Texto: ");
  WriteLn(Estructura.Texto);
END ModuloDeEjemplo.
```

Explicación del código:

* El código define un módulo llamado `ModuloDeEjemplo`.


* El módulo importa la unidad `Terminal` para poder utilizar las funciones `WriteString` y `WriteLn`.


* Se definen cuatro tipos de datos: `TipoEntero`, `TipoReal`, `TipoTexto` y `TipoEstructura`.


* Se definen cuatro variables: `Entero`, `Real`, `Texto` y `Estructura`.


* Se asigna un valor a cada variable.


* Se utilizan las funciones `WriteString` y `WriteLn` para imprimir el valor de cada variable en la consola.