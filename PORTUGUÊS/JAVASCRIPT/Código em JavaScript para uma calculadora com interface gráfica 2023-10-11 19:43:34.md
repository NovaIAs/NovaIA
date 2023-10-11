Claro! Aqui está um código complexo em JavaScript que cria uma calculadora com uma interface gráfica:

```javascript
// Variáveis globais
let display = document.getElementById("display");
let buttons = document.getElementsByClassName("button");
let equation = "";

// Função para atualizar o display
function updateDisplay() {
  display.value = equation;
}

// Função para limpar a equação
function clearEquation() {
  equation = "";
  updateDisplay();
}

// Função para adicionar um número ou operador à equação
function addToEquation(value) {
  equation += value;
  updateDisplay();
}

// Função para calcular o resultado da equação
function calculate() {
  try {
    let result = eval(equation);
    equation = String(result);
    updateDisplay();
  } catch (error) {
    equation = "Erro";
    updateDisplay();
  }
}

// Loop para adicionar eventos de clique aos botões
for (let i = 0; i < buttons.length; i++) {
  buttons[i].addEventListener("click", function() {
    let value = buttons[i].getAttribute("data-value");
    addToEquation(value);
  });
}
```

Neste código, temos uma calculadora com uma interface gráfica básica. A interface é criada em HTML e cada botão possui um atributo `data-value` que representa o valor que será adicionado à equação.

A função `updateDisplay` é responsável por atualizar o valor exibido no display da calculadora. Ela é chamada sempre que há uma alteração na equação.

A função `clearEquation` limpa a equação, ou seja, remove todos os valores presentes nela.

A função `addToEquation` é utilizada para adicionar um número ou operador à equação. Ela recebe o valor como parâmetro e o concatena à variável `equation`.

A função `calculate` é responsável por calcular o resultado da equação. Ela utiliza a função `eval` para avaliar a expressão da equação e obter o resultado. Em caso de erro, a variável `equation` é definida como "Erro" e o valor é exibido no display.

Por fim, utilizamos um loop para adicionar um evento de clique a cada botão da calculadora. Quando um botão é clicado, a função `addToEquation` é chamada passando o valor correspondente ao botão como argumento.

Espero que este código atenda às suas expectativas! Caso tenha alguma dúvida, estou à disposição para ajudar.