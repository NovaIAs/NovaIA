Claro! Aqui está um exemplo de um código em FORTRAN que realiza a análise de dados de uma pesquisa:

```fortran
PROGRAM AnalisePesquisa
  IMPLICIT NONE
  
  INTEGER :: N, i
  REAL :: media, desvio, soma, somaQuadrados, variancia
  
  ! Solicita o número de participantes da pesquisa
  WRITE(*,*) 'Digite o número de participantes da pesquisa:'
  READ(*,*) N
  
  ! Declaração do vetor para armazenar as respostas
  REAL :: respostas(N)
  
  ! Solicita as respostas dos participantes
  DO i = 1, N
     WRITE(*,*) 'Digite a resposta do participante', i
     READ(*,*) respostas(i)
  END DO
  
  ! Cálculo da média
  soma = 0.0
  DO i = 1, N
    soma = soma + respostas(i)
  END DO
  media = soma / N
  
  ! Cálculo da variância e desvio padrão
  somaQuadrados = 0.0
  DO i = 1, N
    somaQuadrados = somaQuadrados + (respostas(i) - media) ** 2
  END DO
  variancia = somaQuadrados / N
  desvio = SQRT(variancia)
  
  ! Exibição dos resultados
  WRITE(*,*) 'A média das respostas é:', media
  WRITE(*,*) 'A variância das respostas é:', variancia
  WRITE(*,*) 'O desvio padrão das respostas é:', desvio
  
  STOP
END PROGRAM AnalisePesquisa
```

Neste código, o programa solicita o número de participantes de uma pesquisa e, em seguida, captura as respostas de cada participante. Em seguida, ele calcula a média, a variância e o desvio padrão das respostas coletadas. Os resultados são exibidos no final do programa.

Espero que isso seja útil! Se você tiver alguma outra solicitação, é só pedir.