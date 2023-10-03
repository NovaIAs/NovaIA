Claro! Aqui está um código em JavaScript que cria um efeito de digitação em um elemento HTML:

```javascript
// Função para simular digitação em um elemento HTML
function digitar(elemento, texto, velocidade) {
  let i = 0;
  
  // Função interna para adicionar cada caractere ao elemento HTML
  function adicionarCaractere() {
    if (i < texto.length) {
      elemento.innerHTML += texto.charAt(i);
      i++;
      setTimeout(adicionarCaractere, velocidade); // Chama a função novamente após um intervalo de tempo
    }
  }
  
  // Inicia a digitação
  adicionarCaractere();
}

// Exemplo de uso
const elementoHTML = document.getElementById("texto-digitado"); // Elemento HTML onde o texto será exibido
const textoCompleto = "Olá, eu sou um código de exemplo em JavaScript!"; // Texto a ser digitado
const velocidadeDigitar = 100; // Velocidade em milissegundos para cada caractere ser adicionado

digitar(elementoHTML, textoCompleto, velocidadeDigitar);
```

Neste código, definimos uma função chamada `digitar` que recebe três parâmetros: `elemento`, `texto` e `velocidade`. O parâmetro `elemento` representa o elemento HTML onde o texto será exibido, `texto` é o conteúdo que será digitado e `velocidade` define a velocidade em milissegundos para cada caractere ser adicionado.

Dentro da função `digitar`, inicializamos uma variável `i` com o valor zero. Em seguida, criamos uma função interna chamada `adicionarCaractere` que verifica se `i` é menor que o comprimento do texto. Se for, adiciona o caractere correspondente ao índice `i` ao elemento HTML usando a propriedade `innerHTML` e incrementa `i`. Em seguida, a função `adicionarCaractere` é chamada novamente após um intervalo de tempo definido pela variável `velocidade`.

Finalmente, chamamos a função `digitar` passando o elemento HTML onde o texto será exibido, o texto completo e a velocidade desejada. Neste exemplo, assumimos que existe um elemento HTML com o id "texto-digitado" onde o texto será exibido. O resultado será um efeito de digitação gradual do texto fornecido.