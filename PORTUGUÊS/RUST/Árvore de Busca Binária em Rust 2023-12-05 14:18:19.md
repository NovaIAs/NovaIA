Claro! Aqui está um exemplo de código em Rust que implementa uma árvore de busca binária.

```rust
// Definição da estrutura para os nós da árvore
struct Node {
    valor: i32,
    esquerda: Option<Box<Node>>,
    direita: Option<Box<Node>>,
}

impl Node {
    // Função para criar um novo nó da árvore
    fn novo(valor: i32) -> Self {
        Node {
            valor,
            esquerda: None,
            direita: None,
        }
    }
    
    // Função para inserir um valor na árvore
    fn inserir(&mut self, valor: i32) {
        if valor < self.valor {
            match self.esquerda {
                Some(ref mut esquerda) => esquerda.inserir(valor),
                None => self.esquerda = Some(Box::new(Node::novo(valor))),
            }
        } else if valor > self.valor {
            match self.direita {
                Some(ref mut direita) => direita.inserir(valor),
                None => self.direita = Some(Box::new(Node::novo(valor))),
            }
        }
    }
    
    // Função para buscar um valor na árvore
    fn buscar(&self, valor: i32) -> bool {
        if valor == self.valor {
            return true;
        } else if valor < self.valor {
            match self.esquerda {
                Some(ref esquerda) => esquerda.buscar(valor),
                None => false,
            }
        } else {
            match self.direita {
                Some(ref direita) => direita.buscar(valor),
                None => false,
            }
        }
    }
}

fn main() {
    // Criação da árvore de busca binária
    let mut arvore = Node::novo(8);
    arvore.inserir(3);
    arvore.inserir(10);
    arvore.inserir(1);
    arvore.inserir(6);
    arvore.inserir(14);
    arvore.inserir(4);
    arvore.inserir(7);
    arvore.inserir(13);
    
    // Busca por valores na árvore
    println!("O valor 6 está presente na árvore? {}", arvore.buscar(6));
    println!("O valor 12 está presente na árvore? {}", arvore.buscar(12));
}
```

Neste código, definimos a estrutura `Node` que representa os nós da árvore de busca binária. Cada nó possui um valor, uma referência para o nó à esquerda e uma referência para o nó à direita.

A função `inserir` permite inserir um valor na árvore, seguindo as regras de uma árvore de busca binária. Se o valor a ser inserido for menor que o valor do nó atual, ele é inserido na subárvore à esquerda. Caso contrário, é inserido na subárvore à direita.

A função `buscar` busca um valor na árvore, percorrendo-a de forma recursiva a partir do nó atual. Se o valor for encontrado, retorna verdadeiro. Caso contrário, continua a busca na subárvore apropriada. Se a busca chegar a um nó nulo (None) significa que o valor não está presente na árvore e retorna falso.

No `main`, criamos a árvore de busca binária e inserimos alguns valores. Em seguida, realizamos duas buscas na árvore e exibimos o resultado no console.