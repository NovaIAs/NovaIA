Claro! Aqui está um código complexo em JavaScript, que envolve a criação de uma calculadora com uma interface gráfica:

```javascript
// Definindo uma função para criar os botões da calculadora
function criarBotoes() {
  const botoes = document.getElementById('botoes');
  
  const operadores = ['+', '-', '*', '/'];
  const numeros = [7, 8, 9, 4, 5, 6, 1, 2, 3, 0];
  
  // Criando os botões para os números
  numeros.forEach((numero) => {
    const botao = document.createElement('button');
    botao.textContent = numero;
    botao.addEventListener('click', () => {
      adicionarValor(numero);
    });
    botoes.appendChild(botao);
  });
  
  // Criando os botões para os operadores
  operadores.forEach((operador) => {
    const botao = document.createElement('button');
    botao.textContent = operador;
    botao.addEventListener('click', () => {
      adicionarOperador(operador);
    });
    botoes.appendChild(botao);
  });
  
  // Criando o botão para o resultado
  const botaoResultado = document.createElement('button');
  botaoResultado.textContent = '=';
  botaoResultado.addEventListener('click', calcularResultado);
  botoes.appendChild(botaoResultado);
}

// Função para adicionar um valor à expressão
function adicionarValor(valor) {
  const expressao = document.getElementById('expressao');
  expressao.textContent += valor;
}

// Função para adicionar um operador à expressão
function adicionarOperador(operador) {
  const expressao = document.getElementById('expressao');
  expressao.textContent += operador;
}

// Função para calcular o resultado da expressão
function calcularResultado() {
  const expressao = document.getElementById('expressao');
  const resultado = document.getElementById('resultado');
  
  try {
    resultado.textContent = eval(expressao.textContent);
  } catch (error) {
    resultado.textContent = 'Erro';
  }
  
  expressao.textContent = '';
}

// Chamando a função para criar os botões da calculadora
criarBotoes();
```

Neste código, criamos uma calculadora simples com uma interface gráfica em HTML e CSS. A função `criarBotoes()` é responsável por gerar os botões numéricos e operadores da calculadora. Os botões são criados dinamicamente usando o método `createElement()` e adicionados ao elemento com o ID "botoes".

A função `adicionarValor(valor)` é chamada quando um número é clicado e adiciona o valor correspondente à expressão exibida na tela da calculadora.

A função `adicionarOperador(operador)` é chamada quando um operador é clicado e adiciona o operador à expressão.

A função `calcularResultado()` é chamada quando o botão "=" é clicado e utiliza a função `eval()` para executar a expressão completa e exibir o resultado na tela.

Espero que este código atenda às suas expectativas!