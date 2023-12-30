Claro! Aqui está um exemplo de código em Smalltalk, que faz uso de algumas classes e métodos avançados:

```
| listaNumeros soma |

listaNumeros := #(10 20 30 40 50). "Cria um array com números"

soma := 0. "Inicializa a variável soma com zero"

listaNumeros do: [:numero |
    soma := soma + numero. "Itera sobre cada número do array e acumula na variável soma"
].

Transcript show: 'A soma dos números é: ', soma asString. "Exibe a soma dos números na console"
```

Neste código, criamos um array de números `(10 20 30 40 50)` e em seguida, inicializamos uma variável chamada `soma` com zero. Em seguida, usamos o método `do:` para iterar sobre cada elemento do array de números.

Dentro do bloco `[:numero | ...]`, somamos o valor da variável `numero` à variável `soma` usando o operador `+`. Isso é feito para cada número no array, acumulando o resultado na variável `soma`.

Por fim, usamos o método `asString` para converter o valor da variável `soma` em uma string e usamos o método `show:` do objeto `Transcript` para exibir a soma dos números na console.

Esse código é apenas um exemplo básico de como utilizar estruturas de controle e métodos em Smalltalk. Existem muitas outras funcionalidades na linguagem que podem ser exploradas para criar programas mais complexos e sofisticados.