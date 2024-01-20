```modula-2

MODULE Funcionarios;
FROM Terminal IMPORT WriteString, WriteLn, WriteCard, WriteReal,
                    ReadCard, ReadLine, ReadBool, ReadReal;

TYPE
  Funcionario = RECORD
    nome: ARRAY[1..20] OF CHAR;
    codigo: CARDINAL;
    cargo: ARRAY[1..20] OF CHAR;
    salario: REAL;
    tempoServico: CARDINAL;
  END;

VAR
  listaFuncionarios: ARRAY[1..100] OF Funcionario;
  numFuncionarios: CARDINAL;

PROCEDURE LeerDatosFuncionarios();
VAR
  i: CARDINAL;
BEGIN
    FOR i := 1 TO 100 DO
      WriteString("Nombre: ");
      ReadLine(listaFuncionarios[i].nome);
      WriteString("Código: ");
      listaFuncionarios[i].codigo := ReadCard;
      WriteString("Cargo: ");
      ReadLine(listaFuncionarios[i].cargo);
      WriteString("Salario: ");
      listaFuncionarios[i].salario := ReadReal;
      WriteString("Tiempo de servicio: ");
      listaFuncionarios[i].tempoServico := ReadCard;
    END;
END LeerDatosFuncionarios;

PROCEDURE ImprimirDatosFuncionarios();
VAR
  i: CARDINAL;
BEGIN
    FOR i := 1 TO 100 DO
      WriteString("Nombre: ");
      WriteString(listaFuncionarios[i].nome);
      WriteString(", Código: ");
      WriteCard(listaFuncionarios[i].codigo);
      WriteString(", Cargo: ");
      WriteString(listaFuncionarios[i].cargo);
      WriteString(", Salario: ");
      WriteReal(listaFuncionarios[i].salario);
      WriteString(", Tiempo de servicio: ");
      WriteCard(listaFuncionarios[i].tempoServico);
      WriteLn;
    END;
END ImprimirDatosFuncionarios;

PROCEDURE OrdenarFuncionariosPorSalario();
VAR
  i, j: CARDINAL;
  aux: Funcionario;
BEGIN
    FOR i := 1 TO 99 DO
      FOR j := i + 1 TO 100 DO
        IF listaFuncionarios[i].salario > listaFuncionarios[j].salario THEN
          aux := listaFuncionarios[i];
          listaFuncionarios[i] := listaFuncionarios[j];
          listaFuncionarios[j] := aux;
        END;
      END;
    END;
END OrdenarFuncionariosPorSalario;

PROCEDURE BuscarFuncionarioPorCodigo(codigo: CARDINAL): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
    FOR i := 1 TO 100 DO
      IF listaFuncionarios[i].codigo = codigo THEN
        RETURN i;
      END;
    END;
    RETURN 0;
END BuscarFuncionarioPorCodigo;

BEGIN
  LeerDatosFuncionarios;
  ImprimirDatosFuncionarios;
  OrdenarFuncionariosPorSalario;
  ImprimirDatosFuncionarios;
  WriteString("Código del funcionario a buscar: ");
  numFuncionarios := ReadCard;
  IF BuscarFuncionarioPorCodigo(numFuncionarios) = 0 THEN
    WriteString("Funcionario no encontrado");
  ELSE
    WriteString("Funcionario encontrado:");
    WriteString(listaFuncionarios[BuscarFuncionarioPorCodigo(numFuncionarios)].nome);
  END;
END Funcionarios.

```

Este código en MODULA-2 crea un programa para gestionar una lista de funcionarios.

El programa contiene los siguientes procedimientos:

* `LeerDatosFuncionarios`: Lee los datos de los funcionarios desde la entrada estándar.
* `ImprimirDatosFuncionarios`: Imprime los datos de los funcionarios en la salida estándar.
* `OrdenarFuncionariosPorSalario`: Ordena los funcionarios por su salario.
* `BuscarFuncionarioPorCodigo`: Busca un funcionario por su código.

El programa primero llama al procedimiento `LeerDatosFuncionarios` para leer los datos de los funcionarios desde la entrada estándar. Luego llama al procedimiento `ImprimirDatosFuncionarios` para imprimir los datos de los funcionarios en la salida estándar. A continuación, llama al procedimiento `OrdenarFuncionariosPorSalario` para ordenar los funcionarios por su salario. Finalmente, llama al procedimiento `BuscarFuncionarioPorCodigo` para buscar un funcionario por su código.

El programa también contiene las siguientes variables:

* `listaFuncionarios`: Un array de registros que contiene los datos de los funcionarios.
* `numFuncionarios`: El número de funcionarios en la lista.

El programa primero asigna el valor 0 a la variable `numFuncionarios`. Luego, llama al procedimiento `LeerDatosFuncionarios` para leer los datos de los funcionarios desde la entrada estándar. El procedimiento `LeerDatosFuncionarios` lee los datos de los funcionarios desde la entrada estándar y los almacena en el array `listaFuncionarios`. El programa luego llama al procedimiento `ImprimirDatosFuncionarios` para imprimir los datos de los funcionarios en la salida estándar. El procedimiento `ImprimirDatosFuncionarios` imprime los datos de los funcionarios en la salida estándar.

A continuación, el programa llama al procedimiento `OrdenarFuncionariosPorSalario` para ordenar los funcionarios por su salario. El procedimiento `OrdenarFuncionariosPorSalario` ordena los funcionarios por su salario mediante el algoritmo de ordenación por selección. El programa luego llama al procedimiento `ImprimirDatosFuncionarios` para imprimir los datos de los funcionarios en la salida estándar. El procedimiento `ImprimirDatosFuncionarios` imprime los datos de los funcionarios en la salida estándar.

Finalmente, el programa llama al procedimiento `BuscarFuncionarioPorCodigo` para buscar un funcionario por su código. El procedimiento `BuscarFuncionarioPorCodigo` busca un funcionario por su código mediante el algoritmo de búsqueda lineal. El programa imprime el nombre del funcionario que se encuentra. Si no se encuentra ningún funcionario, el programa imprime un mensaje de error.