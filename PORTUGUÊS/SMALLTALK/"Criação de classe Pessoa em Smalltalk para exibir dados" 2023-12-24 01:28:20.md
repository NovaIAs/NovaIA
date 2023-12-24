Claro! Aqui está um exemplo de código em Smalltalk que cria uma classe chamada "Pessoa" com alguns atributos e métodos:

```smalltalk
"Definindo a classe Pessoa"
Object subclass: Pessoa [
    | nome idade |
    
    "Método de inicialização da classe"
    initialize [
        super initialize.
        nome := ''.
        idade := 0.
    ]
    
    "Método para definir o nome"
    setNome: umNome [
        nome := umNome.
    ]
    
    "Método para definir a idade"
    setIdade: umaIdade [
        idade := umaIdade.
    ]
    
    "Método para exibir os dados da pessoa"
    exibirDados [
        Transcript show: 'Nome: ', nome; cr.
        Transcript show: 'Idade: ', idade asString; cr.
    ]
]

"Exemplo de uso da classe Pessoa"
| pessoa |
pessoa := Pessoa new.
pessoa setNome: 'João'.
pessoa setIdade: 25.
pessoa exibirDados.
```

Neste código, criamos uma classe chamada "Pessoa" que possui os atributos "nome" e "idade". O método `initialize` é usado para inicializar os atributos com valores padrão. Os métodos `setNome:` e `setIdade:` são usados para definir o nome e a idade da pessoa, respectivamente.

O método `exibirDados` é responsável por exibir na tela os dados da pessoa, utilizando o objeto `Transcript` para mostrar as informações. No exemplo final, criamos um objeto da classe Pessoa, definimos o nome e a idade e então chamamos o método `exibirDados` para mostrar as informações na tela.

É importante ressaltar que o Smalltalk é uma linguagem orientada a objetos pura, onde tudo é um objeto, inclusive os números e os operadores matemáticos são objetos. Além disso, a linguagem utiliza uma sintaxe peculiar, com a separação de palavras-chave por dois pontos.