Claro! Aqui está um exemplo de um código em FORTRAN que realiza a soma de dois números inteiros:

```
PROGRAM SOMA_NUMEROS
    IMPLICIT NONE
    
    INTEGER :: num1, num2, resultado
    
    ! Solicita ao usuário os dois números a serem somados
    WRITE(*,*) "Digite o primeiro número: "
    READ(*,*) num1
    
    WRITE(*,*) "Digite o segundo número: "
    READ(*,*) num2
    
    ! Realiza a soma dos números
    resultado = num1 + num2
    
    ! Exibe o resultado para o usuário
    WRITE(*,*) "A soma de", num1, "e", num2, "é igual a", resultado
    
END PROGRAM SOMA_NUMEROS
```

Neste código, iniciamos declarando as variáveis `num1`, `num2` e `resultado` como inteiros. Em seguida, solicitamos ao usuário que digite os dois números a serem somados usando a instrução `WRITE` seguida de `READ` para ler os valores digitados. 

Após isso, realizamos a soma dos dois números e armazenamos o resultado na variável `resultado`. Por fim, exibimos a mensagem com o resultado da soma usando a instrução `WRITE`.

Espero que este código possa te ajudar a entender como realizar uma soma simples em FORTRAN!