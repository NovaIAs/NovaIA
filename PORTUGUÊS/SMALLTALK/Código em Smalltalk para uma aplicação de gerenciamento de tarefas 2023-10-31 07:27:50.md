Claro! Aqui está um código em Smalltalk que cria uma aplicação de gerenciamento de tarefas. A aplicação permite ao usuário adicionar, visualizar e concluir tarefas.

Começaremos criando a classe `Tarefa` para representar uma tarefa com seus atributos:

```smalltalk
Object subclass: #Tarefa
    instanceVariableNames: 'descricao concluida'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'GerenciamentoTarefas'
```

Aqui, definimos as variáveis de instância `descricao` e `concluida` para armazenar a descrição da tarefa e o estado de conclusão (verdadeiro ou falso).

Agora, vamos adicionar os métodos para manipular as tarefas:

```smalltalk
Tarefa>>descricao: aDescricao
    descricao := aDescricao

Tarefa>>descricao
    ^ descricao

Tarefa>>concluida: aConcluida
    concluida := aConcluida

Tarefa>>concluida
    ^ concluida
```

Os métodos `descricao:` e `descricao` são usados para definir e obter a descrição da tarefa, enquanto `concluida:` e `concluida` fazem o mesmo para o estado de conclusão.

A seguir, criaremos a classe `GerenciadorTarefas` para controlar as tarefas:

```smalltalk
Object subclass: #GerenciadorTarefas
    instanceVariableNames: 'tarefas'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'GerenciamentoTarefas'
```

Esta classe terá uma variável de instância `tarefas`, que será uma lista para armazenar todas as tarefas.

Agora, adicionaremos os métodos para manipular as tarefas:

```smalltalk
GerenciadorTarefas>>adicionarTarefa: aTarefa
    tarefas add: aTarefa

GerenciadorTarefas>>removerTarefa: aTarefa
    tarefas remove: aTarefa

GerenciadorTarefas>>listarTarefas
    tarefas do: [ :tarefa | Transcript show: tarefa descricao; cr. ]

GerenciadorTarefas>>concluirTarefa: aTarefa
    aTarefa concluida: true
```

O método `adicionarTarefa:` é usado para adicionar uma tarefa à lista de tarefas. O método `removerTarefa:` remove uma tarefa da lista. O método `listarTarefas` exibe todas as tarefas na janela de transcrição. Por fim, o método `concluirTarefa:` recebe uma tarefa e a marca como concluída.

Por fim, vamos criar uma instância da classe `GerenciadorTarefas` e testar nossa aplicação:

```smalltalk
| gerenciador |
gerenciador := GerenciadorTarefas new.

"Tarefa 1"
| tarefa1 |
tarefa1 := Tarefa new.
tarefa1 descricao: 'Comprar leite'.
gerenciador adicionarTarefa: tarefa1.

"Tarefa 2"
| tarefa2 |
tarefa2 := Tarefa new.
tarefa2 descricao: 'Pagar contas'.
gerenciador adicionarTarefa: tarefa2.

"Listar tarefas"
gerenciador listarTarefas.

"Concluir uma tarefa"
gerenciador concluirTarefa: tarefa1.

"Listar tarefas novamente"
gerenciador listarTarefas.
```

Neste exemplo, criamos duas tarefas e as adicionamos ao gerenciador de tarefas. Em seguida, listamos as tarefas na janela de transcrição. Depois, marcamos a primeira tarefa como concluída e listamos as tarefas novamente para ver a alteração.

Esse é um exemplo básico de uma aplicação de gerenciamento de tarefas em Smalltalk. Claro, existem várias outras funcionalidades que podem ser adicionadas, mas esse código é um bom ponto de partida para entender como criar uma aplicação simples em Smalltalk.