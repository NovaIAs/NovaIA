Claro! Aqui está um exemplo de código complexo em Python:

```python
# Classe Pessoa
class Pessoa:
    def __init__(self, nome, idade):
        self.nome = nome
        self.idade = idade

    def cumprimentar(self):
        print(f"Olá, meu nome é {self.nome} e eu tenho {self.idade} anos.")

# Classe Estudante que herda da classe Pessoa
class Estudante(Pessoa):
    def __init__(self, nome, idade, instituicao):
        super().__init__(nome, idade)
        self.instituicao = instituicao

    def estudar(self):
        print(f"Estou estudando na instituição {self.instituicao}.")

# Classe Professor que herda da classe Pessoa
class Professor(Pessoa):
    def __init__(self, nome, idade, disciplina):
        super().__init__(nome, idade)
        self.disciplina = disciplina

    def ensinar(self):
        print(f"Estou ensinando a disciplina {self.disciplina}.")

# Classe Universidade
class Universidade:
    def __init__(self, nome):
        self.nome = nome
        self.estudantes = []
        self.professores = []

    def adicionar_estudante(self, estudante):
        self.estudantes.append(estudante)

    def adicionar_professor(self, professor):
        self.professores.append(professor)

    def listar_estudantes(self):
        print(f"Estudantes da universidade {self.nome}:")
        for estudante in self.estudantes:
            print(f" - {estudante.nome}")

    def listar_professores(self):
        print(f"Professores da universidade {self.nome}:")
        for professor in self.professores:
            print(f" - {professor.nome}")

# Criação de instâncias das classes
joao = Estudante("João", 20, "Universidade XYZ")
maria = Estudante("Maria", 22, "Universidade XYZ")
pedro = Professor("Pedro", 35, "Matemática")
ana = Professor("Ana", 40, "História")
universidade = Universidade("Universidade XYZ")

# Adicionar estudantes e professores à universidade
universidade.adicionar_estudante(joao)
universidade.adicionar_estudante(maria)
universidade.adicionar_professor(pedro)
universidade.adicionar_professor(ana)

# Exibir lista de estudantes e professores da universidade
universidade.listar_estudantes()
universidade.listar_professores()

# Interagir com os objetos
joao.estudar()
maria.estudar()
pedro.ensinar()
ana.ensinar()
joao.cumprimentar()
maria.cumprimentar()
pedro.cumprimentar()
ana.cumprimentar()
```

Neste código, criamos classes para representar pessoas, estudantes, professores e uma universidade. A classe Pessoa serve como classe base para as classes Estudante e Professor, que herdam seus atributos e métodos.

A classe Universidade é responsável por armazenar os estudantes e professores em listas e possui métodos para adicionar e listar os mesmos.

No final do código, criamos instâncias das classes e interagimos com os objetos, chamando os métodos específicos de cada um, como estudar, ensinar e cumprimentar.

Esse código é apenas um exemplo de como estruturar classes em Python e como as mesmas podem se relacionar entre si. Claro, existem muitas outras possibilidades e estruturas de código que podem ser criadas.