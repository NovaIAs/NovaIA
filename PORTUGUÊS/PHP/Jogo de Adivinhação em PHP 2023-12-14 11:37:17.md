Claro! Aqui está um código complexo em PHP que cria um jogo de adivinhação onde o usuário tenta adivinhar um número secreto:

```php
<?php

// Função que gera um número aleatório entre 1 e 100
function gerarNumeroSecreto() {
    return rand(1, 100);
}

// Função que verifica se o número fornecido pelo usuário é igual ao número secreto
function verificarAdivinhacao($numeroSecreto, $tentativa) {
    if ($tentativa == $numeroSecreto) {
        return "Parabéns! Você acertou o número secreto.";
    } elseif ($tentativa < $numeroSecreto) {
        return "Tente um número maior.";
    } else {
        return "Tente um número menor.";
    }
}

// Inicializando o jogo
$numeroSecreto = gerarNumeroSecreto();
$numeroTentativas = 0;

echo "Bem-vindo ao jogo de adivinhação!\n";
echo "Tente adivinhar o número secreto entre 1 e 100.\n";

// Loop principal do jogo
while (true) {
    $tentativa = readline("Digite um número: ");
    $numeroTentativas++;

    // Verifica se a entrada é um número válido
    if (!is_numeric($tentativa)) {
        echo "Por favor, digite um número válido.\n";
        continue;
    }

    // Converte a tentativa para inteiro
    $tentativa = intval($tentativa);

    // Verifica se a tentativa está dentro do intervalo permitido
    if ($tentativa < 1 || $tentativa > 100) {
        echo "Por favor, digite um número entre 1 e 100.\n";
        continue;
    }

    // Verifica se o número foi adivinhado corretamente
    $resultado = verificarAdivinhacao($numeroSecreto, $tentativa);
    echo $resultado . "\n";

    // Verifica se o jogo terminou
    if ($resultado == "Parabéns! Você acertou o número secreto.") {
        break;
    }
}

echo "Você acertou o número secreto em $numeroTentativas tentativas.\n";

?>
```

Neste código, temos duas funções principais:

1. `gerarNumeroSecreto()`: Essa função utiliza a função `rand()` para gerar um número aleatório entre 1 e 100, que será o número que o jogador deve adivinhar.

2. `verificarAdivinhacao($numeroSecreto, $tentativa)`: Essa função recebe o número secreto e a tentativa do jogador como parâmetros e verifica se o número fornecido é igual, maior ou menor que o número secreto. Ela retorna uma mensagem correspondente para cada caso.

O jogo começa com uma mensagem de boas-vindas e instruções para o jogador. Em seguida, um loop infinito é iniciado para receber as tentativas do jogador.

Dentro do loop, a função `readline()` é usada para obter a tentativa do jogador a cada iteração. As validações são feitas para garantir que a entrada seja um número válido e esteja dentro do intervalo permitido.

Após cada tentativa, a função `verificarAdivinhacao()` é chamada para verificar se o jogador acertou o número secreto. A mensagem de retorno é exibida na tela.

O jogo continua até que o jogador acerte o número secreto, momento em que o loop é interrompido e uma mensagem final é exibida com o número de tentativas realizadas.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, é só pedir.