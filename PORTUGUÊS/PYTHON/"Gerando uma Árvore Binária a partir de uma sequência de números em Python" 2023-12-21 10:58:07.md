Claro! Aqui está um código complexo em Python que gera uma árvore binária a partir de uma sequência de números fornecida pelo usuário. A árvore binária é então percorrida em pré-ordem, em ordem e em pós-ordem, e os valores dos nós são exibidos na saída.

```python
class No:
    def __init__(self, valor):
        self.valor = valor
        self.esquerda = None
        self.direita = None

def inserir(raiz, valor):
    if raiz is None:
        return No(valor)
    elif valor < raiz.valor:
        raiz.esquerda = inserir(raiz.esquerda, valor)
    else:
        raiz.direita = inserir(raiz.direita, valor)
    return raiz

def percorrer_pre_ordem(raiz):
    if raiz:
        print(raiz.valor, end=" ")
        percorrer_pre_ordem(raiz.esquerda)
        percorrer_pre_ordem(raiz.direita)

def percorrer_em_ordem(raiz):
    if raiz:
        percorrer_em_ordem(raiz.esquerda)
        print(raiz.valor, end=" ")
        percorrer_em_ordem(raiz.direita)

def percorrer_pos_ordem(raiz):
    if raiz:
        percorrer_pos_ordem(raiz.esquerda)
        percorrer_pos_ordem(raiz.direita)
        print(raiz.valor, end=" ")

# Função para criar a árvore binária a partir de uma sequência de números
def criar_arvore_binaria(sequencia):
    raiz = None
    for num in sequencia:
        raiz = inserir(raiz, num)
    return raiz

# Entrada dos números
sequencia = input("Digite uma sequência de números separados por espaço: ").split()
sequencia = [int(num) for num in sequencia]

# Criação da árvore binária
arvore = criar_arvore_binaria(sequencia)

# Percorrendo e exibindo os valores
print("Percurso em Pré-Ordem:")
percorrer_pre_ordem(arvore)

print("\nPercurso em Ordem:")
percorrer_em_ordem(arvore)

print("\nPercurso em Pós-Ordem:")
percorrer_pos_ordem(arvore)
```

Explicação do código:
1. Começamos definindo a classe `No`, que representa um nó da árvore binária. Cada nó possui um valor, uma referência para o nó da esquerda e uma referência para o nó da direita.

2. Em seguida, temos a função `inserir`, que insere um novo valor na árvore. Ela é uma função recursiva que percorre a árvore até encontrar uma posição adequada para o novo valor.

3. Depois, temos as funções `percorrer_pre_ordem`, `percorrer_em_ordem` e `percorrer_pos_ordem`, que realizam os percursos pré-ordem, em ordem e pós-ordem, respectivamente. Essas funções também são recursivas, percorrendo a árvore de forma recursiva e exibindo os valores dos nós.

4. A função `criar_arvore_binaria` recebe a sequência de números fornecida pelo usuário e cria a árvore binária a partir desses números. Ela utiliza a função `inserir` para inserir cada número na árvore.

5. Por fim, o código solicita ao usuário que digite uma sequência de números separados por espaço. Essa sequência é convertida em uma lista de inteiros e passada para a função `criar_arvore_binaria`. Em seguida, os percursos em pré-ordem, em ordem e em pós-ordem são realizados e os valores dos nós são exibidos na saída.