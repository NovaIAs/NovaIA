Claro! Vou gerar um código em Smalltalk que consiste em um sistema de gerenciamento de tarefas pessoais. Este código permitirá que você adicione, visualize e marque tarefas como concluídas. Vamos lá:

```
Object subclass: #GerenciadorDeTarefas
	instanceVariableNames: 'tarefas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GerenciamentoDeTarefas'

GerenciadorDeTarefas class >> new
	^ super new initialize

GerenciadorDeTarefas >> initialize
	tarefas := OrderedCollection new.

GerenciadorDeTarefas >> adicionarTarefa: umaTarefa
	tarefas add: umaTarefa.

GerenciadorDeTarefas >> removerTarefa: umaTarefa
	tarefas remove: umaTarefa.

GerenciadorDeTarefas >> tarefasPendentes
	^ tarefas select: [ :tarefa | tarefa estaConcluida not ].

GerenciadorDeTarefas >> tarefasConcluidas
	^ tarefas select: [ :tarefa | tarefa estaConcluida ].

Object subclass: #Tarefa
	instanceVariableNames: 'descricao estaConcluida'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GerenciamentoDeTarefas'

Tarefa >> initialize: umaDescricao
	descricao := umaDescricao.
	estaConcluida := false.

Tarefa >> marcarComoConcluida
	estaConcluida := true.

Tarefa >> estaConcluida
	^ estaConcluida.

Tarefa >> descricao
	^ descricao.
```

Aqui temos duas classes: `GerenciadorDeTarefas` e `Tarefa`. A classe `GerenciadorDeTarefas` é responsável por gerenciar as tarefas, enquanto a classe `Tarefa` representa cada tarefa individualmente.

O método `new` da classe `GerenciadorDeTarefas` retorna uma instância inicializada do gerenciador. O método `initialize` inicializa a coleção de tarefas como uma `OrderedCollection` vazia.

Os métodos `adicionarTarefa:` e `removerTarefa:` permitem adicionar e remover tarefas da coleção, respectivamente.

Os métodos `tarefasPendentes` e `tarefasConcluidas` retornam as tarefas que estão pendentes (não concluídas) e as tarefas que estão concluídas, respectivamente.

A classe `Tarefa` possui um método `initialize:` que inicializa uma tarefa com uma descrição fornecida e marca a tarefa como não concluída.

O método `marcarComoConcluida` permite marcar a tarefa como concluída.

Os métodos `estaConcluida` e `descricao` retornam se a tarefa está concluída e a descrição da tarefa, respectivamente.

Este código em Smalltalk implementa um sistema básico de gerenciamento de tarefas. Você pode criar instâncias do `GerenciadorDeTarefas`, adicionar tarefas, marcá-las como concluídas e visualizar as tarefas pendentes ou concluídas.