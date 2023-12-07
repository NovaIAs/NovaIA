Claro! Aqui está um exemplo de um código complexo em JavaScript que atende aos requisitos mencionados:

```javascript
// Calculadora avançada

// Função para obter a entrada do usuário
function obterEntrada() {
  let entrada = prompt("Digite uma operação matemática (Ex: 2 + 2):");
  return entrada;
}

// Função para analisar a operação matemática
function analisarOperacao(entrada) {
  // Removendo espaços em branco da entrada
  entrada = entrada.replace(/\s+/g, '');

  // Verificando se a entrada é válida
  if (!/^\d+(\.\d+)?[+\-*\/]\d+(\.\d+)?$/.test(entrada)) {
    console.log("Entrada inválida!");
    return;
  }

  // Separando os operandos e o operador
  let operandos = entrada.match(/\d+(\.\d+)?/g);
  let operador = entrada.match(/[+\-*\/]/)[0];

  // Convertendo os operandos para números
  operandos = operandos.map((operando) => parseFloat(operando));

  // Realizando a operação
  let resultado;

  switch (operador) {
    case '+':
      resultado = operandos.reduce((a, b) => a + b);
      break;
    case '-':
      resultado = operandos.reduce((a, b) => a - b);
      break;
    case '*':
      resultado = operandos.reduce((a, b) => a * b);
      break;
    case '/':
      resultado = operandos.reduce((a, b) => a / b);
      break;
  }

  // Exibindo o resultado
  console.log(`Resultado da operação: ${entrada} = ${resultado}`);
}

// Chamando as funções para obter e analisar a operação
let entrada = obterEntrada();
analisarOperacao(entrada);
```

Este código é uma calculadora avançada em JavaScript. Ele solicita que o usuário digite uma operação matemática (por exemplo, "2 + 2") e, em seguida, analisa a entrada, identificando os operandos (números) e o operador (+, -, * ou /). Em seguida, ele realiza a operação matemática correspondente, usando um switch case para determinar a operação a ser executada. Por fim, ele exibe o resultado no console.

Espero que este código atenda às suas expectativas! Lembre-se de sempre testar o código e realizar as devidas validações para garantir que ele funcione corretamente em diferentes cenários.