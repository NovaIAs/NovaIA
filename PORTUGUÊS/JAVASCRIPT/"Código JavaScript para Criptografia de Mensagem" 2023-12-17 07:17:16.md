Claro! Aqui está um código complexo em JavaScript que envolve manipulação de strings, arrays e estruturas de controle. O código irá realizar um processo de criptografia de uma mensagem informada pelo usuário. Vou explicar cada parte do código para facilitar o entendimento:

```javascript
// Função para obter o índice da letra no alfabeto
function indiceDaLetra(letra) {
  const alfabeto = "abcdefghijklmnopqrstuvwxyz";
  return alfabeto.indexOf(letra.toLowerCase());
}

// Função para criptografar a mensagem
function criptografar(mensagem, chave) {
  let mensagemCriptografada = "";

  for (let i = 0; i < mensagem.length; i++) {
    const letra = mensagem[i];
    const indice = indiceDaLetra(letra);

    if (indice >= 0) {
      const novaIndice = (indice + chave) % 26;
      const novaLetra = String.fromCharCode(novaIndice + 97);
      mensagemCriptografada += novaLetra;
    } else {
      mensagemCriptografada += letra;
    }
  }

  return mensagemCriptografada;
}

// Função para descriptografar a mensagem
function descriptografar(mensagem, chave) {
  return criptografar(mensagem, 26 - chave);
}

// Solicita ao usuário a mensagem e a chave de criptografia
const mensagemOriginal = prompt("Digite a mensagem a ser criptografada:");
const chaveCriptografia = parseInt(prompt("Digite a chave de criptografia (número inteiro):"));

// Criptografa a mensagem informada pelo usuário
const mensagemCriptografada = criptografar(mensagemOriginal, chaveCriptografia);
console.log("Mensagem criptografada:", mensagemCriptografada);

// Descriptografa a mensagem criptografada
const mensagemDescriptografada = descriptografar(mensagemCriptografada, chaveCriptografia);
console.log("Mensagem descriptografada:", mensagemDescriptografada);
```

Explicação do código:

1. Começamos definindo uma função chamada `indiceDaLetra` que recebe uma letra como parâmetro e retorna o índice dessa letra no alfabeto. Essa função será utilizada posteriormente para auxiliar na criptografia e descriptografia da mensagem.

2. Em seguida, temos a função `criptografar`, que recebe a mensagem original e a chave de criptografia como parâmetros. Essa função percorre cada caractere da mensagem e, se for uma letra, realiza a criptografia adicionando a chave ao índice da letra no alfabeto. Caso contrário, mantém o caractere original. A função retorna a mensagem criptografada.

3. Logo após, temos a função `descriptografar`, que recebe a mensagem criptografada e a chave de criptografia como parâmetros. Essa função utiliza a função `criptografar`, mas com uma chave complementar (26 - chave) para realizar a descriptografia. Dessa forma, ao aplicar a operação inversa da criptografia, obtemos a mensagem original.

4. Em seguida, solicitamos ao usuário a mensagem original e a chave de criptografia usando a função `prompt`. A mensagem e a chave são armazenadas nas variáveis `mensagemOriginal` e `chaveCriptografia`, respectivamente.

5. Utilizamos a função `criptografar` para criptografar a mensagem original com a chave informada e armazenamos o resultado na variável `mensagemCriptografada`.

6. Por fim, utilizamos a função `descriptografar` para descriptografar a mensagem criptografada com a mesma chave e armazenamos o resultado na variável `mensagemDescriptografada`. Exibimos as mensagens criptografada e descriptografada no console usando `console.log`.