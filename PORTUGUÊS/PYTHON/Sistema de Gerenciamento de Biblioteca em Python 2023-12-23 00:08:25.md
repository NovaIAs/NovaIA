Claro! Aqui está um código complexo em Python que envolve a criação de um sistema de gerenciamento de biblioteca. Este código permite adicionar livros, listar todos os livros disponíveis, pesquisar livros por título ou autor, emprestar livros para usuários e registrar a devolução de livros.

```python
class Livro:
    def __init__(self, titulo, autor, disponivel=True):
        self.titulo = titulo
        self.autor = autor
        self.disponivel = disponivel

class Biblioteca:
    def __init__(self):
        self.livros = []

    def adicionar_livro(self, titulo, autor):
        livro = Livro(titulo, autor)
        self.livros.append(livro)
        print(f'O livro "{titulo}" de {autor} foi adicionado à biblioteca.')

    def listar_livros(self):
        if not self.livros:
            print('A biblioteca está vazia.')
        else:
            print('Livros disponíveis na biblioteca:')
            for livro in self.livros:
                disponibilidade = 'Disponível' if livro.disponivel else 'Indisponível'
                print(f'Título: {livro.titulo} | Autor: {livro.autor} | Disponibilidade: {disponibilidade}')

    def pesquisar_livro(self, titulo):
        for livro in self.livros:
            if livro.titulo.lower() == titulo.lower():
                disponibilidade = 'Disponível' if livro.disponivel else 'Indisponível'
                print(f'Título: {livro.titulo} | Autor: {livro.autor} | Disponibilidade: {disponibilidade}')
                return
        print(f'O livro "{titulo}" não foi encontrado na biblioteca.')

    def pesquisar_por_autor(self, autor):
        livros_por_autor = []
        for livro in self.livros:
            if livro.autor.lower() == autor.lower():
                livros_por_autor.append(livro)
        if not livros_por_autor:
            print(f'Não foram encontrados livros escritos por {autor}.')
        else:
            print(f'Livros escritos por {autor}:')
            for livro in livros_por_autor:
                disponibilidade = 'Disponível' if livro.disponivel else 'Indisponível'
                print(f'Título: {livro.titulo} | Autor: {livro.autor} | Disponibilidade: {disponibilidade}')

    def emprestar_livro(self, titulo):
        for livro in self.livros:
            if livro.titulo.lower() == titulo.lower() and livro.disponivel:
                livro.disponivel = False
                print(f'O livro "{titulo}" foi emprestado com sucesso.')
                return
        print(f'O livro "{titulo}" não está disponível para empréstimo.')

    def registrar_devolucao(self, titulo):
        for livro in self.livros:
            if livro.titulo.lower() == titulo.lower() and not livro.disponivel:
                livro.disponivel = True
                print(f'O livro "{titulo}" foi devolvido com sucesso.')
                return
        print(f'O livro "{titulo}" não pode ser devolvido ou não pertence à biblioteca.')


# Exemplo de uso do código:

biblioteca = Biblioteca()

biblioteca.adicionar_livro('Dom Casmurro', 'Machado de Assis')
biblioteca.adicionar_livro('A Revolução dos Bichos', 'George Orwell')

biblioteca.listar_livros()

biblioteca.pesquisar_livro('Dom Casmurro')
biblioteca.pesquisar_por_autor('George Orwell')

biblioteca.emprestar_livro('Dom Casmurro')

biblioteca.registrar_devolucao('Dom Casmurro')

biblioteca.listar_livros()
```

Nesse código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com atributos como título, autor e disponibilidade. A classe `Biblioteca` possui métodos para adicionar livros, listar livros disponíveis, pesquisar livros por título ou autor, emprestar livros e registrar devoluções.

O exemplo de uso demonstra como criar uma biblioteca, adicionar livros, listar livros disponíveis, pesquisar livros por título ou autor, emprestar livros e registrar devoluções.