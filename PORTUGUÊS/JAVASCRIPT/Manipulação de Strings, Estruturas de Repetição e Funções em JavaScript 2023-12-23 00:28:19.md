Claro! Aqui está um código complexo em JavaScript que envolve manipulação de strings, estruturas de repetição e funções:

```javascript
// Função para inverter uma string
function inverterString(str) {
  let invertedStr = "";
  for (let i = str.length - 1; i >= 0; i--) {
    invertedStr += str[i];
  }
  return invertedStr;
}

// Função para verificar se uma string é um palíndromo
function verificarPalindromo(str) {
  const lowerCaseStr = str.toLowerCase();
  const reversedStr = inverterString(lowerCaseStr);
  return lowerCaseStr === reversedStr;
}

// Função para contar o número de palavras em uma frase
function contarPalavras(frase) {
  const words = frase.split(" ");
  return words.length;
}

// Função para encontrar o caracter mais comum em uma string
function encontrarCaracterMaisComum(str) {
  const charCount = {};
  let maxCount = 0;
  let mostCommonChar = "";

  for (let i = 0; i < str.length; i++) {
    const currentChar = str[i];

    if (charCount[currentChar]) {
      charCount[currentChar]++;
    } else {
      charCount[currentChar] = 1;
    }

    if (charCount[currentChar] > maxCount) {
      maxCount = charCount[currentChar];
      mostCommonChar = currentChar;
    }
  }

  return mostCommonChar;
}

// Exemplo de utilização das funções
const frase = "Aprender programação é muito divertido!";
const palavra = "arara";
const caracter = "banana";

console.log(`A frase "${frase}" possui ${contarPalavras(frase)} palavras.`);
console.log(`A palavra "${palavra}" ${verificarPalindromo(palavra) ? "é" : "não é"} um palíndromo.`);
console.log(`O caracter mais comum na string "${caracter}" é "${encontrarCaracterMaisComum(caracter)}".`);
```

Neste código, criamos quatro funções: `inverterString`, `verificarPalindromo`, `contarPalavras` e `encontrarCaracterMaisComum`. 

A função `inverterString` recebe uma string como parâmetro e retorna a string invertida. Utilizamos um laço de repetição `for` para percorrer a string de trás para frente e armazenar os caracteres na variável `invertedStr`.

A função `verificarPalindromo` recebe uma string como parâmetro e retorna `true` se a string for um palíndromo (ou seja, se ela permanecer a mesma quando invertida) e `false` caso contrário. Para isso, primeiro convertemos a string para caixa baixa usando o método `toLowerCase()`. Em seguida, utilizamos a função `inverterString` para obter a versão invertida da string e comparamos se a string original é igual à sua versão invertida.

A função `contarPalavras` recebe uma frase como parâmetro e retorna o número de palavras presentes na frase. Utilizamos o método `split(" ")` para dividir a frase em um array de palavras, considerando o espaço como separador. Em seguida, retornamos o comprimento desse array.

A função `encontrarCaracterMaisComum` recebe uma string como parâmetro e retorna o caractere mais comum presente na string. Utilizamos um objeto `charCount` para armazenar a contagem de cada caracter. Percorremos a string com um laço de repetição `for`, atualizando o contador para cada caracter encontrado. Armazenamos o caractere mais comum na variável `mostCommonChar` e o número de ocorrências na variável `maxCount`.

No exemplo final, utilizamos as funções para realizar diferentes operações. Imprimimos o número de palavras na frase, verificamos se uma palavra é um palíndromo e encontramos o caractere mais comum em uma string.