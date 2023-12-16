Claro! Aqui está um código complexo em JavaScript que envolve manipulação de strings, arrays, funções e expressões regulares. 

```javascript
// Função para encontrar palavras palíndromas em uma string
function encontrarPalindromas(frase) {
  // Remover pontuações e converter para minúsculas
  frase = frase.replace(/[^\w\s]|_/g, "").toLowerCase();
  
  // Dividir a frase em palavras
  const palavras = frase.split(/\s+/);
  
  // Array para armazenar as palavras palíndromas encontradas
  const palindromas = [];
  
  // Verificar se cada palavra é um palíndromo
  for (let i = 0; i < palavras.length; i++) {
    const palavra = palavras[i];
    const inverso = palavra.split("").reverse().join("");
    
    if (palavra === inverso) {
      palindromas.push(palavra);
    }
  }
  
  // Retornar as palavras palíndromas encontradas
  return palindromas;
}

// Exemplo de uso da função
const fraseExemplo = "Ame a ema";
const palindromasEncontradas = encontrarPalindromas(fraseExemplo);

console.log("Palíndromas encontradas na frase:", palindromasEncontradas);
```

Neste código, temos uma função chamada `encontrarPalindromas` que recebe uma frase como parâmetro. A função realiza as seguintes etapas:

1. Remove todas as pontuações da frase, substituindo por um espaço em branco, utilizando a expressão regular `/[^\w\s]|_/g`.
2. Converte a frase para minúsculas utilizando o método `toLowerCase()`.
3. Divide a frase em palavras utilizando a expressão regular `/\s+/`.
4. Inicializa um array chamado `palindromas` para armazenar as palavras palíndromas encontradas.
5. Verifica se cada palavra é um palíndromo:
   - Inverte a palavra utilizando os métodos `split("")`, `reverse()` e `join("")`.
   - Compara a palavra original com a palavra invertida.
   - Caso sejam iguais, a palavra é considerada um palíndromo e é adicionada ao array `palindromas`.
6. Retorna o array `palindromas` contendo as palavras palíndromas encontradas.

Em seguida, temos um exemplo de uso da função `encontrarPalindromas`. A frase de exemplo é "Ame a ema". A função é chamada com essa frase e o resultado é armazenado na variável `palindromasEncontradas`. Por fim, o resultado é exibido no console utilizando `console.log`.