Claro! Aqui está um código complexo em Dart, que demonstra a implementação de uma árvore binária de busca com funções de inserção, busca e remoção de nós. A árvore binária de busca é uma estrutura de dados que permite a organização eficiente de elementos em uma árvore, onde cada nó possui no máximo dois filhos, sendo que o filho da esquerda é menor e o filho da direita é maior que o valor do nó atual. Vamos lá!

```dart
class No {
  int valor;
  No esquerda;
  No direita;

  No(this.valor);
}

class ArvoreBinariaBusca {
  No raiz;

  void inserir(int valor) {
    raiz = _inserirNo(raiz, valor);
  }

  No _inserirNo(No no, int valor) {
    if (no == null) {
      return No(valor);
    }

    if (valor < no.valor) {
      no.esquerda = _inserirNo(no.esquerda, valor);
    } else if (valor > no.valor) {
      no.direita = _inserirNo(no.direita, valor);
    }

    return no;
  }

  bool buscar(int valor) {
    return _buscarNo(raiz, valor);
  }

  bool _buscarNo(No no, int valor) {
    if (no == null) {
      return false;
    }

    if (valor == no.valor) {
      return true;
    }

    if (valor < no.valor) {
      return _buscarNo(no.esquerda, valor);
    } else {
      return _buscarNo(no.direita, valor);
    }
  }

  void remover(int valor) {
    raiz = _removerNo(raiz, valor);
  }

  No _removerNo(No no, int valor) {
    if (no == null) {
      return null;
    }

    if (valor < no.valor) {
      no.esquerda = _removerNo(no.esquerda, valor);
    } else if (valor > no.valor) {
      no.direita = _removerNo(no.direita, valor);
    } else {
      if (no.esquerda == null && no.direita == null) {
        return null;
      } else if (no.esquerda == null) {
        return no.direita;
      } else if (no.direita == null) {
        return no.esquerda;
      }

      No menorNoDireita = _encontrarMenorNo(no.direita);
      no.valor = menorNoDireita.valor;
      no.direita = _removerNo(no.direita, menorNoDireita.valor);
    }

    return no;
  }

  No _encontrarMenorNo(No no) {
    No atual = no;
    while (atual.esquerda != null) {
      atual = atual.esquerda;
    }
    return atual;
  }
}

void main() {
  ArvoreBinariaBusca arvore = ArvoreBinariaBusca();
  
  arvore.inserir(50);
  arvore.inserir(30);
  arvore.inserir(20);
  arvore.inserir(40);
  arvore.inserir(70);
  arvore.inserir(60);
  arvore.inserir(80);

  print(arvore.buscar(40)); // true
  print(arvore.buscar(90)); // false

  arvore.remover(30);
  print(arvore.buscar(30)); // false
}
```

Neste código, criamos duas classes: `No` e `ArvoreBinariaBusca`. A classe `No` representa um nó da árvore, contendo um valor e referências para os nós filhos. A classe `ArvoreBinariaBusca` representa a árvore em si, possuindo uma referência para a raiz.

A função `inserir` recebe um valor como parâmetro e insere um novo nó na árvore de forma ordenada, seguindo as regras da árvore binária de busca.

A função `buscar` recebe um valor como parâmetro e retorna true se o valor estiver presente na árvore e false caso contrário.

A função `remover` recebe um valor como parâmetro e remove o nó correspondente da árvore, mantendo a organização da estrutura.

No `main`, criamos uma instância da árvore binária de busca e inserimos alguns valores. Em seguida, realizamos algumas buscas e removemos um nó da árvore.

Espero que este código complexo em Dart atenda às suas expectativas! Caso você precise de mais informações ou tenha alguma dúvida, estou à disposição para ajudar.