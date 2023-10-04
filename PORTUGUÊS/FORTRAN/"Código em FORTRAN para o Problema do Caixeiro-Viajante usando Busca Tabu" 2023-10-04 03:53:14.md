Claro! Aqui está um código complexo em FORTRAN que resolve o problema do caixeiro-viajante usando o algoritmo de otimização heurística conhecido como busca tabu. O caixeiro-viajante é um problema clássico de otimização combinatória em que o objetivo é encontrar o caminho mais curto que um vendedor deve percorrer para visitar um conjunto de cidades e retornar ao ponto de partida.

```
PROGRAM CaixeiroViajante
  IMPLICIT NONE

  INTEGER, PARAMETER :: num_cidades = 10  ! Número de cidades a serem visitadas
  INTEGER, PARAMETER :: num_iteracoes = 1000  ! Número máximo de iterações

  INTEGER :: distancias(num_cidades, num_cidades)  ! Matriz de distâncias entre as cidades
  INTEGER :: cidade_inicial  ! Cidade inicial do caixeiro-viajante
  INTEGER :: melhor_solucao(num_cidades)  ! Melhor solução encontrada até o momento
  INTEGER :: melhor_distancia  ! Melhor distância encontrada até o momento
  INTEGER :: iteracao

  ! Inicialização das distâncias entre as cidades
  distancias = [[0, 10, 15, 20, 25, 30, 35, 40, 45, 50],
                [10, 0, 9, 14, 19, 24, 29, 34, 39, 44],
                [15, 9, 0, 8, 13, 18, 23, 28, 33, 38],
                [20, 14, 8, 0, 7, 12, 17, 22, 27, 32],
                [25, 19, 13, 7, 0, 6, 11, 16, 21, 26],
                [30, 24, 18, 12, 6, 0, 5, 10, 15, 20],
                [35, 29, 23, 17, 11, 5, 0, 4, 9, 14],
                [40, 34, 28, 22, 16, 10, 4, 0, 3, 8],
                [45, 39, 33, 27, 21, 15, 9, 3, 0, 7],
                [50, 44, 38, 32, 26, 20, 14, 8, 7, 0]]

  ! Definição da cidade inicial
  cidade_inicial = 1

  ! Inicialização da melhor solução e melhor distância com um valor alto
  melhor_solucao = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  melhor_distancia = 9999

  ! Algoritmo de busca tabu
  DO iteracao = 1, num_iteracoes
    CALL gerar_vizinho(melhor_solucao, cidade_inicial)
    CALL calcular_distancia(melhor_solucao, distancias, melhor_distancia)
    CALL atualizar_melhor_solucao(melhor_solucao, melhor_distancia)
  END DO

  ! Exibição da melhor solução encontrada
  PRINT *, "Melhor solução encontrada:"
  DO iteracao = 1, num_cidades
    PRINT *, melhor_solucao(iteracao)
  END DO

  ! Exibição da melhor distância encontrada
  PRINT *, "Melhor distância encontrada:", melhor_distancia

CONTAINS

  SUBROUTINE gerar_vizinho(solucao, cidade_inicial)
    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: solucao(num_cidades)  ! Solução atual do caixeiro-viajante
    INTEGER, INTENT(IN) :: cidade_inicial  ! Cidade inicial do caixeiro-viajante

    INTEGER :: i, j, cidade_temp, num_vizinhos, vizinho

    num_vizinhos = 3  ! Número de vizinhos a serem gerados

    DO i = 1, num_vizinhos
      DO j = 1, num_cidades-1
        IF (solucao(j) == cidade_inicial) THEN
          cidade_temp = solucao(j+1)
          solucao(j+1) = cidade_inicial
          solucao(j) = cidade_temp
          EXIT
        END IF
      END DO
    END DO

    DO i = 1, num_vizinhos
      vizinho = 1 + MOD(i, num_cidades-1)
      IF (solucao(vizinho) /= cidade_inicial) THEN
        cidade_temp = solucao(vizinho)
        solucao(vizinho) = cidade_inicial
        solucao(num_cidades) = cidade_temp
        EXIT
      END IF
    END DO

  END SUBROUTINE gerar_vizinho

  SUBROUTINE calcular_distancia(solucao, distancias, distancia)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: solucao(num_cidades)  ! Solução atual do caixeiro-viajante
    INTEGER, INTENT(IN) :: distancias(num_cidades, num_cidades)  ! Matriz de distâncias entre as cidades
    INTEGER, INTENT(OUT) :: distancia  ! Distância total da solução atual

    INTEGER :: i

    distancia = 0

    DO i = 1, num_cidades-1
      distancia = distancia + distancias(solucao(i), solucao(i+1))
    END DO

    distancia = distancia + distancias(solucao(num_cidades), solucao(1))

  END SUBROUTINE calcular_distancia

  SUBROUTINE atualizar_melhor_solucao(solucao, distancia)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: solucao(num_cidades)  ! Solução atual do caixeiro-viajante
    INTEGER, INTENT(IN) :: distancia  ! Distância total da solução atual

    IF (distancia < melhor_distancia) THEN
      melhor_solucao = solucao
      melhor_distancia = distancia
    END IF

  END SUBROUTINE atualizar_melhor_solucao

END PROGRAM CaixeiroViajante
```

Este código implementa o algoritmo de busca tabu para resolver o problema do caixeiro-viajante. Ele utiliza uma matriz de distâncias predefinida para representar as distâncias entre as cidades. A cidade inicial é definida como a primeira cidade da solução. O algoritmo realiza um número máximo de iterações, gerando vizinhos da solução atual através da troca de cidades adjacentes e calculando a distância total da solução atual. A melhor solução e a melhor distância são atualizadas sempre que uma solução com uma distância menor é encontrada. No final, o programa exibe a melhor solução encontrada e a melhor distância encontrada.