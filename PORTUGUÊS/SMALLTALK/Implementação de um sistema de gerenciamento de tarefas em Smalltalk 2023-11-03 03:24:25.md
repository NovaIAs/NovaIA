Claro! Aqui está um exemplo de código em Smalltalk que demonstra a implementação de um sistema de gerenciamento de tarefas:

```
Object subclass: GerenciadorTarefas [
    | tarefas |
    
    GerenciadorTarefas class >> novo [
        ^ self basicNew initialize
    ]
    
    GerenciadorTarefas >> initialize [
        tarefas := OrderedCollection new
    ]
    
    GerenciadorTarefas >> adicionarTarefa: umaTarefa [
        tarefas add: umaTarefa
    ]
    
    GerenciadorTarefas >> removerTarefa: umaTarefa [
        tarefas remove: umaTarefa ifAbsent: []
    ]
    
    GerenciadorTarefas >> listarTarefas [
        tarefas do: [:tarefa |
            Transcript show: tarefa; cr
        ]
    ]
    
    GerenciadorTarefas >> concluirTarefa: umaTarefa [
        umaTarefa concluir
    ]
]

Object subclass: Tarefa [
    | descricao concluida |
    
    Tarefa class >> nova: umaDescricao [
        ^ self basicNew initialize: umaDescricao
    ]
    
    Tarefa >> initialize: umaDescricao [
        descricao := umaDescricao.
        concluida := false
    ]
    
    Tarefa >> concluir [
        concluida := true
    ]
    
    Tarefa >> descricao [
        ^ descricao
    ]
    
    Tarefa >> concluida [
        ^ concluida
    ]
]

"Exemplo de uso do sistema de gerenciamento de tarefas"
| gerenciador tarefa1 tarefa2 |

gerenciador := GerenciadorTarefas novo.
tarefa1 := Tarefa nova: 'Limpar a casa'.
gerenciador adicionarTarefa: tarefa1.
tarefa2 := Tarefa nova: 'Fazer compras'.
gerenciador adicionarTarefa: tarefa2.
gerenciador listarTarefas.
Transcript show: '---'. Transcript cr.
gerenciador concluirTarefa: tarefa1.
gerenciador listarTarefas.
```

Neste código, temos duas classes principais: `GerenciadorTarefas` e `Tarefa`. 

A classe `GerenciadorTarefas` é responsável por gerenciar a lista de tarefas e possui os métodos necessários para adicionar, remover, listar e concluir tarefas. A classe `Tarefa` representa uma tarefa individual e possui uma descrição e um estado de conclusão.

O método `nova` da classe `GerenciadorTarefas` cria uma nova instância do gerenciador de tarefas. O método `initialize` inicializa a lista de tarefas vazia.

O método `adicionarTarefa:` permite adicionar uma tarefa à lista de tarefas. O método `removerTarefa:` remove uma tarefa da lista. O método `listarTarefas` exibe no Transcript as descrições de todas as tarefas da lista. O método `concluirTarefa:` permite marcar uma tarefa como concluída.

A classe `Tarefa` possui um método `nova:` que cria uma nova instância de tarefa com a descrição fornecida. O método `initialize:` inicializa a descrição e define o estado de conclusão como falso.

O método `concluir` marca a tarefa como concluída. Os métodos `descricao` e `concluida` retornam a descrição e o estado de conclusão de uma tarefa, respectivamente.

No exemplo de uso do sistema de gerenciamento de tarefas, criamos uma nova instância do gerenciador de tarefas, duas tarefas e as adicionamos ao gerenciador. Em seguida, listamos as tarefas na tela e marcamos a primeira tarefa como concluída. Por fim, listamos novamente as tarefas para ver o resultado da conclusão.

Este código em Smalltalk ilustra uma implementação simples de um sistema de gerenciamento de tarefas, permitindo a adição, remoção, listagem e conclusão de tarefas.