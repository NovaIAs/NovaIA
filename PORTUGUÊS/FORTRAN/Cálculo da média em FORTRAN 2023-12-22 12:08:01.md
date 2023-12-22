Claro! Aqui está um exemplo de código em FORTRAN, que calcula a média de um conjunto de números fornecidos pelo usuário:

```fortran
program media_numeros
    implicit none
    
    integer :: n, i
    real :: numero, soma, media
    
    ! Solicitando a quantidade de números a serem inseridos
    write(*,*) 'Digite a quantidade de números a serem inseridos:'
    read(*,*) n
    
    ! Inicializando a soma
    soma = 0.0
    
    ! Solicitando e somando os números inseridos pelo usuário
    do i = 1, n
        write(*,*) 'Digite o número ', i, ':'
        read(*,*) numero
        soma = soma + numero
    end do
    
    ! Calculando a média
    media = soma / real(n)
    
    ! Imprimindo o resultado
    write(*,*) 'A média dos números inseridos é: ', media
    
end program media_numeros
```

Neste código, utilizamos o comando `write(*,*)` para exibir mensagens ao usuário e `read(*,*)` para ler os valores digitados pelo usuário. O comando `do` realiza um loop para ler todos os números e somá-los. Em seguida, calculamos a média dividindo a soma pelo número total de elementos. Por fim, utilizamos `write(*,*)` novamente para mostrar a média calculada.

Espero que este exemplo atenda às suas expectativas!