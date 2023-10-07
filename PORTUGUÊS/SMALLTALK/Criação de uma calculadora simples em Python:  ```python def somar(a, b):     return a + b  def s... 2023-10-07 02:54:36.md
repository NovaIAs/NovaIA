Certamente! Aqui está um código em Smalltalk que cria uma aplicação de gerenciamento de tarefas em português:

```smalltalk
"Definindo a classe Tarefa para representar uma tarefa"
Object subclass: #Tarefa
    instanceVariableNames: 'descricao dataConclusao concluida'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'GerenciamentoDeTarefas'

"Definindo métodos para a classe Tarefa"
Tarefa>>descricao: umaDescricao
    descricao := umaDescricao

Tarefa>>dataConclusao: umaData
    dataConclusao := umaData

Tarefa>>concluida: umEstado
    concluida := umEstado

Tarefa>>concluir
    concluida := true

Tarefa>>concluida
    ^concluida

Tarefa>>imprimir
    Transcript show: 'Tarefa: ', descricao.
    Transcript show: ' | Data de Conclusão: ', dataConclusao printString.
    Transcript show: ' | Concluída: ', concluida printString.
    Transcript cr

"Definindo a classe ListaDeTarefas para gerenciar uma lista de tarefas"
Object subclass: #ListaDeTarefas
    instanceVariableNames: 'tarefas'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'GerenciamentoDeTarefas'

"Definindo métodos para a classe ListaDeTarefas"
ListaDeTarefas>>initialize
    super initialize.
    tarefas := OrderedCollection new

ListaDeTarefas>>adicionarTarefa: umaTarefa
    tarefas add: umaTarefa

ListaDeTarefas>>removerTarefa: umaTarefa
    tarefas remove: umaTarefa

ListaDeTarefas>>listarTarefas
    tarefas do: [:tarefa | tarefa imprimir]

"Exemplo de uso da aplicação de gerenciamento de tarefas"
| listaTarefas tarefa1 tarefa2 |
listaTarefas := ListaDeTarefas new.

tarefa1 := Tarefa new.
tarefa1 descricao: 'Comprar leite'.
tarefa1 dataConclusao: Date today.
tarefa1 concluida: false.

tarefa2 := Tarefa new.
tarefa2 descricao: 'Fazer exercícios'.
tarefa2 dataConclusao: (Date today addDays: 3).
tarefa2 concluida: false.

listaTarefas adicionarTarefa: tarefa1.
listaTarefas adicionarTarefa: tarefa2.

tarefa1 concluir.

listaTarefas listarTarefas.
```

Neste código, começamos definindo a classe `Tarefa` que representa uma tarefa em si. Ela tem três variáveis de instância: `descricao` (descrição da tarefa), `dataConclusao` (data em que a tarefa deve ser concluída) e `concluida` (indica se a tarefa foi concluída ou não).

Em seguida, definimos métodos para a classe `Tarefa`, incluindo métodos para definir a descrição, data de conclusão e estado de conclusão da tarefa, além de um método para marcar a tarefa como concluída e um método para imprimir os detalhes da tarefa no console.

Depois disso, criamos a classe `ListaDeTarefas`, que é responsável por gerenciar uma lista de tarefas. Ela possui uma variável de instância `tarefas`, que é uma coleção ordenada de tarefas.

Definimos métodos para a classe `ListaDeTarefas`, incluindo métodos para adicionar e remover tarefas da lista, bem como um método para listar todas as tarefas presentes na lista.

No exemplo de uso da aplicação, criamos uma lista de tarefas (`listaTarefas`) e duas tarefas (`tarefa1` e `tarefa2`). Configuramos as propriedades de cada tarefa, adicionamos ambas à lista de tarefas e, em seguida, marcamos a `tarefa1` como concluída.

Finalmente, chamamos o método `listarTarefas` da lista de tarefas para imprimir os detalhes de todas as tarefas no console.

Esse código pode ser modificado e expandido de várias maneiras para atender a diferentes requisitos de um aplicativo de gerenciamento de tarefas em Smalltalk.