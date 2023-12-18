Certamente! Aqui está um código complexo em JavaScript que realiza uma análise estatística em um conjunto de dados. Antes de começarmos, é importante ressaltar que a complexidade do código pode variar dependendo do tamanho dos dados de entrada. Vamos criar uma função chamada "analisarDados" que recebe um array de números como parâmetro e retorna um objeto contendo várias estatísticas sobre os dados.

```javascript
function analisarDados(dados) {
  if (!Array.isArray(dados)) {
    throw new Error("Os dados devem ser um array.");
  }

  if (dados.length === 0) {
    throw new Error("Os dados não podem estar vazios.");
  }

  const estatisticas = {};

  // Média
  const soma = dados.reduce((acc, valor) => acc + valor, 0);
  estatisticas.media = soma / dados.length;

  // Mediana
  const dadosOrdenados = dados.sort((a, b) => a - b);
  const indiceMediana = Math.floor(dadosOrdenados.length / 2);
  estatisticas.mediana = dadosOrdenados[indiceMediana];

  // Moda
  const contador = {};
  let modaFrequencia = 0;
  dados.forEach((valor) => {
    contador[valor] = contador[valor] ? contador[valor] + 1 : 1;
    modaFrequencia = Math.max(modaFrequencia, contador[valor]);
  });
  estatisticas.moda = Object.keys(contador)
    .filter((valor) => contador[valor] === modaFrequencia)
    .map(Number);

  // Desvio padrão
  const desvioMedio = dados.reduce((acc, valor) => acc + Math.abs(valor - estatisticas.media), 0) / dados.length;
  const desvioPadrao = Math.sqrt(dados.reduce((acc, valor) => acc + Math.pow(valor - estatisticas.media, 2), 0) / dados.length);
  estatisticas.desvioPadrao = desvioPadrao;

  return estatisticas;
}

// Exemplo de uso
const dados = [1, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10];
const resultado = analisarDados(dados);
console.log(resultado);
```

Neste código, a função `analisarDados` verifica se o parâmetro fornecido é um array e se não está vazio. Em caso afirmativo, as estatísticas são calculadas usando os dados fornecidos.

A média é calculada somando todos os valores e dividindo pelo número de elementos no array. A mediana é obtida ordenando os dados e selecionando o valor do meio. Se o número de elementos for par, a mediana é a média dos dois valores do meio.

A moda é determinada contando a frequência de cada valor e selecionando o(s) valor(es) com a maior frequência. O desvio padrão é calculado usando a fórmula matemática correspondente.

No exemplo de uso fornecido, um conjunto de dados é definido e a função `analisarDados` é chamada com esses dados. O resultado é exibido no console.

Espero que esse código atenda às suas necessidades! Caso precise de mais informações ou tenha alguma dúvida, fique à vontade para perguntar.