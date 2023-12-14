Claro! Aqui está um código em COOL (Classroom Object Oriented Language) que cria uma implementação simplificada de um sistema de gerenciamento de biblioteca:

```
class Livro {
    nome: String;
    autor: String;
    ano: Int;
    disponivel: Bool;
    emprestadoPor: String;

    inicializar(nome: String, autor: String, ano: Int) : AUTO_TYPE {
        {
            self.nome <- nome;
            self.autor <- autor;
            self.ano <- ano;
            self.disponivel <- true;
            self.emprestadoPor <- "";
        }
    };

    emprestar(pessoa: String) : AUTO_TYPE {
        {
            if self.disponivel then
                {
                    self.disponivel <- false;
                    self.emprestadoPor <- pessoa;
                    "Livro emprestado com sucesso.".out_string();
                }
            else
                {
                    "Livro indisponível no momento.".out_string();
                }
        }
    };

    devolver() : AUTO_TYPE {
        {
            if self.disponivel then
                {
                    "Este livro não está emprestado.".out_string();
                }
            else
                {
                    self.disponivel <- true;
                    self.emprestadoPor <- "";
                    "Livro devolvido com sucesso.".out_string();
                }
        }
    };
};

class Biblioteca {
    livros: Array of Livro;

    inicializar() : AUTO_TYPE {
        {
            self.livros <- new Array of Livro;
        }
    };

    adicionarLivro(nome: String, autor: String, ano: Int) : AUTO_TYPE {
        {
            let novoLivro: Livro <- new Livro.inicializar(nome, autor, ano);
            self.livros.add(novoLivro);
            "Livro adicionado com sucesso.".out_string();
        }
    };

    buscarLivro(nome: String) : AUTO_TYPE {
        {
            let encontrado: Bool <- false;

            for livro in self.livros loop
                {
                    if livro.nome = nome then
                        {
                            "Livro encontrado:".out_string();
                            "Nome: ".out_string();
                            livro.nome.out_string();
                            "Autor: ".out_string();
                            livro.autor.out_string();
                            "Ano: ".out_string();
                            livro.ano.out_string();
                            encontrado <- true;
                            break;
                        }
                }

            if not encontrado then
                {
                    "Livro não encontrado.".out_string();
                }
        }
    };

    listarLivros() : AUTO_TYPE {
        {
            if self.livros.size() = 0 then
                {
                    "Não há livros na biblioteca.".out_string();
                }
            else
                {
                    "Lista de livros:".out_string();

                    for livro in self.livros loop
                        {
                            "Nome: ".out_string();
                            livro.nome.out_string();
                            "Autor: ".out_string();
                            livro.autor.out_string();
                            "Ano: ".out_string();
                            livro.ano.out_string();
                            "Disponível: ".out_string();
                            if livro.disponivel then
                                {
                                    "Sim".out_string();
                                }
                            else
                                {
                                    "Não (Emprestado para: ".out_string();
                                    livro.emprestadoPor.out_string();
                                    ")".out_string();
                                }
                            "\n".out_string();
                        }
                }
        }
    };
};

class Main {
    main() : AUTO_TYPE {
        {
            let biblioteca: Biblioteca <- new Biblioteca.inicializar();

            biblioteca.adicionarLivro("Livro 1", "Autor 1", 2021);
            biblioteca.adicionarLivro("Livro 2", "Autor 2", 2022);
            biblioteca.adicionarLivro("Livro 3", "Autor 3", 2023);

            biblioteca.listarLivros();

            biblioteca.buscarLivro("Livro 2");

            biblioteca.livros[1].emprestar("João");

            biblioteca.listarLivros();

            biblioteca.buscarLivro("Livro 2");

            biblioteca.livros[1].devolver();

            biblioteca.listarLivros();
        }
    };
};
```

Neste código, temos três classes: `Livro`, `Biblioteca` e `Main`. 

A classe `Livro` possui atributos como `nome`, `autor`, `ano`, `disponivel` (indicando se o livro está disponível para empréstimo) e `emprestadoPor` (indicando a pessoa que emprestou o livro, caso esteja emprestado). Ela também possui métodos para inicializar um livro, emprestar e devolver um livro.

A classe `Biblioteca` possui um atributo `livros` do tipo `Array of Livro`, que armazena todos os livros presentes na biblioteca. Ela possui métodos para inicializar a biblioteca, adicionar um novo livro, buscar um livro pelo nome e listar todos os livros presentes na biblioteca.

A classe `Main` possui o método `main`, onde é criada uma instância da classe `Biblioteca`, adicionados alguns livros, listados os livros, buscado um livro específico, emprestado e devolvido um livro, e listados os livros novamente.

Esse código é uma implementação básica e simplificada de um sistema de gerenciamento de biblioteca em COOL. Ele pode ser expandido e aprimorado de acordo com as necessidades específicas de um projeto real.