Claro! Aqui está um código em FORTRAN que calcula o fatorial de um número fornecido pelo usuário:

```fortran
program fatorial
    implicit none
    
    integer :: n, i, resultado = 1
    
    write(*,*) "Digite um número inteiro positivo:"
    read(*,*) n
    
    if (n < 0) then
        write(*,*) "O número fornecido é inválido. Por favor, forneça um número inteiro positivo."
    else if (n == 0) then
        write(*,*) "O fatorial de 0 é 1."
    else
        do i = 1, n
            resultado = resultado * i
        end do
        
        write(*,*) "O fatorial de", n, "é", resultado
    end if
    
end program fatorial
```

Neste código, começamos declarando as variáveis necessárias: `n` para armazenar o número fornecido pelo usuário, `i` para o contador do loop e `resultado` para armazenar o resultado do cálculo.

Em seguida, exibimos uma mensagem solicitando ao usuário que digite um número inteiro positivo e lemos o valor fornecido.

Depois, verificamos se o número fornecido é negativo. Se for, exibimos uma mensagem de erro informando que o número é inválido. Caso contrário, verificamos se o número é igual a zero. Nesse caso, exibimos uma mensagem informando que o fatorial de 0 é 1.

Se o número for maior que zero, entramos em um loop `do` que executa `n` vezes. Dentro do loop, multiplicamos o valor atual de `resultado` pelo valor de `i`. Após o término do loop, exibimos uma mensagem informando o resultado do fatorial.

Espero que este código seja útil! Se tiver alguma dúvida, é só perguntar.