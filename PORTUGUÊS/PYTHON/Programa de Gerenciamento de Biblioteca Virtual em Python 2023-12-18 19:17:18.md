Claro! Vou gerar um código complexo em Python para você. Este código será um programa de gerenciamento de uma biblioteca virtual. Ele permitirá adicionar, buscar, atualizar e remover livros em um catálogo virtual. Vamos lá:

```python
class Livro:
    def __init__(self, titulo, autor, genero, ano_publicacao):
        self.titulo = titulo
        self.autor = autor
        self.genero = genero
        self.ano_publicacao = ano_publicacao

class Biblioteca:
    def __init__(self):
        self.catalogo = []

    def adicionar_livro(self, livro):
        self.catalogo.append(livro)
        print(f'O livro "{livro.titulo}" foi adicionado ao catálogo.')

    def buscar_livro(self, titulo):
        for livro in self.catalogo:
            if livro.titulo.lower() == titulo.lower():
                print(f'Livro encontrado: {livro.titulo} - {livro.autor}')
                return
        print('Livro não encontrado.')

    def atualizar_livro(self, titulo, novo_titulo):
        for livro in self.catalogo:
            if livro.titulo.lower() == titulo.lower():
                livro.titulo = novo_titulo
                print(f'O livro "{titulo}" foi atualizado para "{novo_titulo}".')
                return
        print(f'O livro "{titulo}" não foi encontrado.')

    def remover_livro(self, titulo):
        for livro in self.catalogo:
            if livro.titulo.lower() == titulo.lower():
                self.catalogo.remove(livro)
                print(f'O livro "{livro.titulo}" foi removido do catálogo.')
                return
        print(f'O livro "{titulo}" não foi encontrado.')

    def exibir_catalogo(self):
        if len(self.catalogo) == 0:
            print('O catálogo está vazio.')
            return
        print('Catálogo de livros:')
        for livro in self.catalogo:
            print(f'{livro.titulo} - {livro.autor}')

# Exemplo de uso:
biblioteca = Biblioteca()

livro1 = Livro("Harry Potter e a Pedra Filosofal", "J.K. Rowling", "Fantasia", 1997)
biblioteca.adicionar_livro(livro1)

livro2 = Livro("O Hobbit", "J.R.R. Tolkien", "Fantasia", 1937)
biblioteca.adicionar_livro(livro2)

biblioteca.buscar_livro("Harry Potter e a Pedra Filosofal")

biblioteca.atualizar_livro("O Hobbit", "O Senhor dos Anéis")
biblioteca.buscar_livro("O Senhor dos Anéis")

biblioteca.remover_livro("Harry Potter e a Pedra Filosofal")

biblioteca.exibir_catalogo()
```

Este código cria duas classes, `Livro` e `Biblioteca`. A classe `Livro` é utilizada para representar as informações de um livro, como título, autor, gênero e ano de publicação. A classe `Biblioteca` é responsável pelo gerenciamento dos livros, permitindo adicionar, buscar, atualizar e remover livros do catálogo.

No exemplo de uso fornecido, criamos uma instância da classe `Biblioteca` e adicionamos dois livros ao catálogo. Em seguida, realizamos uma busca pelo livro "Harry Potter e a Pedra Filosofal" e atualizamos o título do livro "O Hobbit" para "O Senhor dos Anéis". Depois, removemos o livro "Harry Potter e a Pedra Filosofal" e exibimos o catálogo atualizado.

Espero que este código atenda às suas expectativas!