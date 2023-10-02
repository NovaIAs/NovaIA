Claro! Aqui está um código complexo em TypeScript que cria uma função para encontrar todas as palavras palíndromas em uma lista de strings:

```typescript
function encontrarPalindromas(lista: string[]): string[] {
  const palindromas: string[] = [];

  for (let i = 0; i < lista.length; i++) {
    const palavra = lista[i];
    const palavraInvertida = palavra.split('').reverse().join('');

    if (palavra === palavraInvertida) {
      palindromas.push(palavra);
    }
  }

  return palindromas;
}

const listaDePalavras = ["arara", "banana", "radar", "casa", "ovo"];
const palindromasEncontradas = encontrarPalindromas(listaDePalavras);
console.log("Palindromas encontradas:", palindromasEncontradas);
```

Nesse código, a função `encontrarPalindromas` recebe uma lista de strings e utiliza um loop `for` para iterar por cada palavra da lista. Em seguida, a função inverte a palavra utilizando os métodos `split`, `reverse` e `join` para comparar com a palavra original. Se a palavra for um palíndromo, ela é adicionada ao array `palindromas`. Por fim, a função retorna a lista de palíndromos encontrados.

No exemplo acima, a lista `listaDePalavras` contém algumas palavras palíndromas ("arara", "radar" e "ovo") e outras que não são palíndromas ("banana" e "casa"). O resultado é exibido no console, mostrando apenas as palavras palíndromas encontradas.