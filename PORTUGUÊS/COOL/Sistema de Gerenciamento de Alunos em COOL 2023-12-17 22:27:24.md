Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que simula um sistema de gerenciamento de alunos em uma escola:

```cool
class Aluno {
   attribute String nome
   attribute Int idade
   attribute Int nota

   method init(n: String, i: Int, nt: Int) : SELF_TYPE {
       nome <- n
       idade <- i
       nota <- nt
       self
   }

   method getNome() : String {
       nome
   }

   method getIdade() : Int {
       idade
   }

   method getNota() : Int {
       nota
   }

   method setNota(n: Int) : Object {
       nota <- n
   }
}

class Turma {
   attribute List alunos

   method init() : SELF_TYPE {
       alunos <- nil
       self
   }

   method adicionarAluno(a: Aluno) : Object {
       alunos <- alunos.concat(a)
   }

   method removerAluno(a: Aluno) : Object {
       let listSize: Int <- alunos.length in
       let i: Int <- 0 in
       while i < listSize loop
           if alunos[i] = a then
               alunos.remove(i)
               listSize <- listSize - 1
           else
               i <- i + 1
           fi
       pool
   }

   method mediaNotas() : Int {
       let sum: Int <- 0 in
       let count: Int <- 0 in
       let listSize: Int <- alunos.length in
       let i: Int <- 0 in
       while i < listSize loop
           sum <- sum + alunos[i].getNota()
           count <- count + 1
           i <- i + 1
       pool
       if count = 0 then
           0
       else
           sum / count
       fi
   }
}

class Main {
   method main() : Object {
       let turma: Turma <- Turma.init() in
       let aluno1: Aluno <- Aluno.init("João", 17, 80) in
       let aluno2: Aluno <- Aluno.init("Maria", 16, 90) in
       let aluno3: Aluno <- Aluno.init("Pedro", 18, 70) in
       let aluno4: Aluno <- Aluno.init("Ana", 17, 85) in
       turma.adicionarAluno(aluno1)
       turma.adicionarAluno(aluno2)
       turma.adicionarAluno(aluno3)
       turma.adicionarAluno(aluno4)
       turma.removerAluno(aluno3)

       let media: Int <- turma.mediaNotas() in

       out_string("A média das notas é: ")
       out_int(media)
   }
}
```

Explicação do código:

- Definimos duas classes, `Aluno` e `Turma`. A classe `Aluno` possui atributos como `nome`, `idade` e `nota`, bem como métodos para obter e definir esses atributos. A classe `Turma` contém uma lista de alunos e métodos para adicionar e remover alunos, além de calcular a média das notas dos alunos.

- A classe `Main` é a classe principal que contém o método `main()`. Dentro do método `main()`, criamos uma instância da classe `Turma` e quatro instâncias da classe `Aluno` com diferentes informações.

- Em seguida, adicionamos os alunos à turma usando o método `adicionarAluno()`. Também removemos o aluno3 usando o método `removerAluno()`.

- Finalmente, calculamos a média das notas usando o método `mediaNotas()` e a exibimos na saída usando `out_string()` e `out_int()`.

Espero que este código atenda às suas expectativas!