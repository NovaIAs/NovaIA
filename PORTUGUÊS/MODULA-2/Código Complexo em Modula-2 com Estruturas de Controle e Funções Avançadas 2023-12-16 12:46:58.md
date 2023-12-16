Claro! Aqui está um código complexo em Modula-2 que utiliza várias estruturas de controle e funcionalidades avançadas:

```modula-2
MODULE Complexo;

IMPORT InOut, Math;

VAR
    numero: REAL;
    contador: INTEGER;

PROCEDURE VerificarNumero(numero: REAL);
BEGIN
    IF numero > 0 THEN
        InOut.WriteLn(numero, " é um número positivo");
    ELSIF numero < 0 THEN
        InOut.WriteLn(numero, " é um número negativo");
    ELSE
        InOut.WriteLn("O número é zero");
    END;
END VerificarNumero;

PROCEDURE CalcularFatorial(numero: INTEGER): INTEGER;
VAR
    fatorial: INTEGER;
    i: INTEGER;
BEGIN
    fatorial := 1;
    
    FOR i := 1 TO numero DO
        fatorial := fatorial * i;
    END;
    
    RETURN fatorial;
END CalcularFatorial;

PROCEDURE CalcularPotencia(numero: REAL; expoente: INTEGER): REAL;
VAR
    potencia: REAL;
BEGIN
    potencia := Math.Power(numero, expoente);
    RETURN potencia;
END CalcularPotencia;

BEGIN
    InOut.Write("Digite um número: ");
    InOut.ReadLn(numero);
    
    VerificarNumero(numero);
    
    InOut.WriteLn("O fatorial de 5 é: ", CalcularFatorial(5));
    
    InOut.WriteLn("2 elevado a 3 é: ", CalcularPotencia(2, 3));
    
    contador := 0;
    WHILE contador < 10 DO
        contador := contador + 1;
        InOut.WriteLn("Contador: ", contador);
    END;
    
    InOut.WriteLn("Fim do programa");
END Complexo.
```

Neste código, utilizei várias estruturas de controle, como `IF-ELSE`, `FOR` e `WHILE`. Também utilizei funções para verificar se um número é positivo, negativo ou zero, calcular o fatorial de um número e calcular uma potência. No bloco principal, solicito ao usuário que digite um número, chamo a função `VerificarNumero` para verificar o tipo de número, imprimo o fatorial de 5 e a potência de 2 elevado a 3, e faço um loop `WHILE` para exibir uma sequência de números até 10. Por fim, imprimo "Fim do programa".