Claro! Aqui está um código complexo em Dart que realiza a classificação de um conjunto de dados usando o algoritmo de árvore de decisão.

```dart
import 'dart:math';

class Dado {
  final List<dynamic> atributos;
  final String classe;

  Dado(this.atributos, this.classe);
}

class No {
  String atributo;
  Map<dynamic, No> filhos = {};
  String classe;

  No(this.atributo);
}

class ArvoreDecisao {
  List<Dado> dados;
  List<String> atributos;
  No raiz;

  ArvoreDecisao(this.dados, this.atributos) {
    raiz = construirArvore(dados, atributos);
  }

  No construirArvore(List<Dado> dados, List<String> atributos) {
    // Se todos os dados têm a mesma classe, retornar um nó folha com a classe
    if (dados.every((dado) => dado.classe == dados[0].classe)) {
      return No(dados[0].classe);
    }
    
    // Se não há mais atributos para dividir, retornar um nó folha com a classe mais comum
    if (atributos.isEmpty) {
      var classes = dados.map((dado) => dado.classe);
      var classeMaisComum = classes.reduce((a, b) =>
          classes.where((classe) => classe == a).length >
                  classes.where((classe) => classe == b).length
              ? a
              : b);
      return No(classeMaisComum);
    }

    // Encontrar o atributo que proporciona a maior ganho de informação
    var ganhosInformacao = <String, double>{};
    for (var atributo in atributos) {
      ganhosInformacao[atributo] = calcularGanhoInformacao(dados, atributo);
    }
    var atributoMaiorGanho =
        ganhosInformacao.entries.reduce((a, b) => a.value > b.value ? a : b).key;

    // Criar um novo nó com o atributo de maior ganho
    var novoNo = No(atributoMaiorGanho);

    // Separar os dados de acordo com os valores do atributo de maior ganho
    var valoresAtributo = dados.map((dado) => dado.atributos[atributos.indexOf(atributoMaiorGanho)]).toSet();
    for (var valorAtributo in valoresAtributo) {
      var subdados = dados
          .where((dado) =>
              dado.atributos[atributos.indexOf(atributoMaiorGanho)] ==
              valorAtributo)
          .toList();
      var subatributos = List.of(atributos);
      subatributos.remove(atributoMaiorGanho);
      novoNo.filhos[valorAtributo] = construirArvore(subdados, subatributos);
    }
    
    return novoNo;
  }

  double calcularGanhoInformacao(List<Dado> dados, String atributo) {
    var entropiaOriginal = calcularEntropia(dados);

    var valoresAtributo =
        dados.map((dado) => dado.atributos[atributos.indexOf(atributo)]).toSet();

    var entropias = <double>[];
    for (var valorAtributo in valoresAtributo) {
      var subdados = dados
          .where((dado) =>
              dado.atributos[atributos.indexOf(atributo)] == valorAtributo)
          .toList();
      entropias.add(calcularEntropia(subdados));
    }

    var proporcoes = entropias
        .map((entropia) => entropias.length > 1 ? entropias.length / dados.length * entropia : entropia)
        .toList();

    var ganhoInformacao = entropiaOriginal -
        proporcoes.reduce((a, b) => a + b);

    return ganhoInformacao;
  }

  double calcularEntropia(List<Dado> dados) {
    var classes = dados.map((dado) => dado.classe);
    var proporcoes = classes
        .map((classe) => classes.length / dados.length * classe)
        .toList();

    var entropia = proporcoes
        .map((proporcao) => proporcao == 0 ? 0 : -proporcao * log2(proporcao))
        .reduce((a, b) => a + b);

    return entropia;
  }

  String classificar(List<dynamic> atributos) {
    return percorrerArvore(raiz, atributos);
  }

  String percorrerArvore(No no, List<dynamic> atributos) {
    if (no.filhos.isEmpty) {
      return no.classe;
    }

    var valorAtributo = atributos[atributos.indexOf(no.atributo)];
    if (no.filhos.containsKey(valorAtributo)) {
      return percorrerArvore(no.filhos[valorAtributo], atributos);
    } else {
      var filhosClasses = no.filhos.values.map((filho) => filho.classe).toSet();
      var classeMaisComum = filhosClasses.reduce((a, b) =>
          filhosClasses.where((classe) => classe == a).length >
                  filhosClasses.where((classe) => classe == b).length
              ? a
              : b);
      return classeMaisComum;
    }
  }
}

void main() {
  // Conjunto de dados de exemplo - Iris dataset
  var dados = [
    Dado([5.1, 3.5, 1.4, 0.2], 'Setosa'),
    Dado([4.9, 3.0, 1.4, 0.2], 'Setosa'),
    Dado([4.7, 3.2, 1.3, 0.2], 'Setosa'),
    Dado([6.4, 3.2, 4.5, 1.5], 'Versicolor'),
    Dado([6.9, 3.1, 4.9, 1.5], 'Versicolor'),
    Dado([5.5, 2.3, 4.0, 1.3], 'Versicolor'),
    Dado([6.5, 3.0, 5.2, 2.0], 'Virginica'),
    Dado([7.6, 3.0, 6.6, 2.1], 'Virginica'),
    Dado([6.0, 3.0, 4.8, 1.8], 'Virginica'),
  ];

  var atributos = ['ComprimentoSepala', 'LarguraSepala', 'ComprimentoPetala', 'LarguraPetala'];

  var arvore = ArvoreDecisao(dados, atributos);

  var novoDado = [5.8, 2.7, 5.1, 1.9];
  var classe = arvore.classificar(novoDado);

  print('Novo dado: $novoDado');
  print('Classe: $classe');
}
```

Neste código, implementamos uma classe `Dado` para representar cada instância de dados, contendo uma lista de atributos e uma classe. 

Em seguida, temos a classe `No`, que representa um nó na árvore de decisão. Cada nó possui um atributo, um mapa de filhos (onde a chave é o valor do atributo e o valor é o nó filho correspondente) e uma classe. 

A classe `ArvoreDecisao` é responsável por construir a árvore de decisão com base nos dados e atributos fornecidos. Ela utiliza o algoritmo de árvore de decisão para dividir os dados de acordo com o atributo que proporciona o maior ganho de informação e construir a árvore recursivamente. 

O método `calcularGanhoInformacao` é utilizado para calcular o ganho de informação de um atributo específico em relação aos dados fornecidos. Ele utiliza o método `calcularEntropia` para calcular a entropia dos dados.

Por fim, temos o método `classificar` que recebe uma lista de atributos e retorna a classe prevista para esses atributos de acordo com a árvore de decisão construída.

No exemplo fornecido, utilizamos o conjunto de dados Iris, que consiste em medidas de comprimento e largura das sépalas e pétalas de três espécies de íris (Setosa, Versicolor e Virginica). A árvore de decisão é construída com base nesses dados e atributos, e em seguida é utilizada para classificar um novo dado com valores `[5.8, 2.7, 5.1, 1.9]`. A classe prevista para esse dado é impressa no console.