Claro! Aqui está um exemplo de um código complexo em JavaScript que faz a verificação da validade de um CPF:

```javascript
function validarCPF(cpf) {
  cpf = cpf.replace(/[^\d]+/g, ''); // Remove caracteres especiais

  if (cpf.length !== 11 || cpf === "00000000000" || cpf === "11111111111" || cpf === "22222222222" || cpf === "33333333333" || cpf === "44444444444" || cpf === "55555555555" || cpf === "66666666666" || cpf === "77777777777" || cpf === "88888888888" || cpf === "99999999999") {
    return false;
  }

  let soma = 0;
  let resto;

  for (let i = 1; i <= 9; i++) {
    soma += parseInt(cpf.substring(i - 1, i)) * (11 - i);
  }

  resto = (soma * 10) % 11;

  if ((resto === 10) || (resto === 11)) {
    resto = 0;
  }

  if (resto !== parseInt(cpf.substring(9, 10))) {
    return false;
  }

  soma = 0;

  for (let i = 1; i <= 10; i++) {
    soma += parseInt(cpf.substring(i - 1, i)) * (12 - i);
  }

  resto = (soma * 10) % 11;

  if ((resto === 10) || (resto === 11)) {
    resto = 0;
  }

  if (resto !== parseInt(cpf.substring(10, 11))) {
    return false;
  }

  return true;
}

const cpf = "123.456.789-00";
const valido = validarCPF(cpf);

if (valido) {
  console.log("CPF válido.");
} else {
  console.log("CPF inválido.");
}
```

Neste código, temos uma função chamada `validarCPF` que recebe um CPF como parâmetro. Primeiro, removemos todos os caracteres especiais do CPF utilizando a função `replace` e a expressão regular `/[^\d]+/g`, que substitui qualquer caractere que não seja dígito por uma string vazia.

Em seguida, fazemos algumas verificações iniciais para descartar CPFs inválidos que possuem todos os dígitos iguais ou têm um comprimento diferente de 11.

Depois, utilizamos um loop `for` para calcular a soma dos produtos dos dígitos do CPF pelos seus respectivos pesos. A fórmula utilizada é `(11 - i)`, onde `i` é a posição do dígito no CPF. Armazenamos o resultado na variável `soma`.

Em seguida, calculamos o resto da divisão da soma por 11 e verificamos se o resto é igual ao dígito verificador do CPF. Caso não seja, o CPF é considerado inválido.

Repetimos o processo anterior considerando todos os dígitos do CPF, incluindo o dígito verificador, e realizamos a mesma verificação no final.

Por fim, a função retorna `true` se o CPF for válido e `false` caso contrário.

No exemplo final, criamos uma variável `cpf` com um CPF de exemplo e chamamos a função `validarCPF` passando esse CPF como argumento. O resultado é armazenado na variável `valido`.

Por fim, utilizamos um `if` para imprimir no console se o CPF é válido ou inválido.

Este código é uma implementação básica da validação de CPF em JavaScript e pode ser utilizado em aplicações que necessitam dessa funcionalidade.