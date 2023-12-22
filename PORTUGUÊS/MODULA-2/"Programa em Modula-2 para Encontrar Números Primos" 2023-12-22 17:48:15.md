Claro! Aqui está um código complexo em Modula-2 que busca e imprime todos os números primos de 1 a um número especificado pelo usuário:

```
MODULE NumerosPrimos;

FROM InOut IMPORT WriteString, WriteInt, WriteLn;
FROM Input IMPORT ReadInt;

PROCEDURE EhPrimo(n: INTEGER): BOOLEAN;
VAR
    i: INTEGER;
BEGIN
    IF n <= 1 THEN
        RETURN FALSE;
    END;

    FOR i := 2 TO n DIV 2 DO
        IF n MOD i = 0 THEN
            RETURN FALSE;
        END;
    END;

    RETURN TRUE;
END EhPrimo;

PROCEDURE ImprimirNumerosPrimos;
VAR
    numeroLimite, i: INTEGER;
BEGIN
    WriteString("Digite um número inteiro positivo: ");
    numeroLimite := ReadInt();

    IF numeroLimite < 2 THEN
        WriteString("Não existem números primos menores que 2.");
        RETURN;
    END;

    WriteLn();
    WriteString("Números primos de 1 a ");
    WriteInt(numeroLimite);
    WriteString(":");
    WriteLn();

    FOR i := 2 TO numeroLimite DO
        IF EhPrimo(i) THEN
            WriteInt(i);
            WriteString(" ");
        END;
    END;

    WriteLn();
END ImprimirNumerosPrimos;

BEGIN
    ImprimirNumerosPrimos;
END NumerosPrimos.
```

Neste código, o módulo `NumerosPrimos` contém duas funções principais: `EhPrimo` e `ImprimirNumerosPrimos`.

A função `EhPrimo` verifica se um número é primo ou não. Ela recebe um número inteiro `n` e retorna um valor booleano `TRUE` se `n` for primo, e `FALSE` caso contrário. A função utiliza um loop `FOR` para verificar se `n` é divisível por algum número de 2 até `n/2`. Se encontrar algum número que divide `n` sem deixar resto, ele retorna `FALSE`. Caso contrário, retorna `TRUE`.

A função `ImprimirNumerosPrimos` solicita ao usuário que digite um número inteiro positivo através da função `ReadInt`. Em seguida, verifica se o número digitado é menor que 2. Caso seja, imprime a mensagem "Não existem números primos menores que 2." e encerra a função.

Caso contrário, imprime os números primos de 1 até o número digitado pelo usuário. Utiliza um loop `FOR` para iterar de 2 até o número limite. Chama a função `EhPrimo` para verificar se cada número é primo. Se for, imprime o número na tela.

Por fim, o programa principal chama a função `ImprimirNumerosPrimos` para executar o programa.