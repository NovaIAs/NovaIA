**Programa Simulador de Lançamento de Cohetes Espaciales**

```portuguol
inicio
    inteiro velocidade_inicial, altura_maxima, tempo_voo, massa_cohete, consumo_combustivel, empuxo_motor;
    real gravidade = 9.80665;

    escreva("Velocidade inicial (m/s): ");
    leia(velocidade_inicial);

    escreva("Altura máxima (m): ");
    leia(altura_maxima);

    escreva("Tempo de voo (s): ");
    leia(tempo_voo);

    escreva("Massa do foguete (kg): ");
    leia(massa_cohete);

    escreva("Consumo de combustível (kg/s): ");
    leia(consumo_combustivel);

    escreva("Empuxo do motor (N): ");
    leia(empuxo_motor);

    real velocidade_final = velocidade_inicial + (empuxo_motor / massa_cohete) * tempo_voo;
    real distancia_percorrida = (velocidade_inicial * tempo_voo) + (0.5 * gravidade * tempo_voo^2);
    real delta_v = velocidade_final - velocidade_inicial;
    real massa_final = massa_cohete - (consumo_combustivel * tempo_voo);
    real impulso_específico = empuxo_motor / (consumo_combustivel * gravidade);

    escreva("\nResultados:\n");
    escreva("Velocidade final: ", velocidade_final, " m/s\n");
    escreva("Distância percorrida: ", distancia_percorrida, " m\n");
    escreva("Delta-V: ", delta_v, " m/s\n");
    escreva("Massa final: ", massa_final, " kg\n");
    escreva("Impulso específico: ", impulso_específico, " s\n");

fim
```

**Explicação do Código:**

1. O programa começa com a declaração de variáveis para armazenar os valores dos parâmetros e resultados do lançamento do foguete.

2. Em seguida, o programa solicita ao usuário que insira os valores para velocidade inicial, altura máxima, tempo de voo, massa do foguete, consumo de combustível e empuxo do motor.

3. O programa calcula a velocidade final, distância percorrida, delta-V, massa final e impulso específico do foguete usando as equações relevantes da física.

4. Finalmente, o programa exibe os resultados do lançamento do foguete.