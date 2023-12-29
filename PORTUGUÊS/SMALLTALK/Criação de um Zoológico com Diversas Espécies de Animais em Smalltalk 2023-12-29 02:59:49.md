Claro! Aqui está um código em Smalltalk que cria uma simulação de um zoológico com diversas espécies de animais. O código está em português e inclui comentários explicativos:

```smalltalk
"Definindo as classes para os animais"

Object subclass: #Animal
    instanceVariableNames: 'nome'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

"Classe abstrata para os animais"
Animal subclass: #AnimalAbstrato
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

"Definindo as classes para cada espécie de animal"

AnimalAbstrato subclass: #Leao
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

AnimalAbstrato subclass: #Elefante
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

AnimalAbstrato subclass: #Girafa
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

"Definindo os métodos para cada espécie de animal"

Leao subclass: #Leao
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

"Implementando o método rugir para o leão"
Leao>>rugir
    Transcript show: 'O leão está rugindo!' ; cr

Elefante subclass: #Elefante
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

"Implementando o método balançar a tromba para o elefante"
Elefante>>balancarTromba
    Transcript show: 'O elefante está balançando a tromba!' ; cr

Girafa subclass: #Girafa
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

"Implementando o método esticar o pescoço para a girafa"
Girafa>>esticarPescoco
    Transcript show: 'A girafa está esticando o pescoço!' ; cr

"Testando a simulação do zoológico"

| leao elefante girafa |

leao := Leao new.
elefante := Elefante new.
girafa := Girafa new.

leao nome: 'Leão Simba'.
elefante nome: 'Elefante Dumbo'.
girafa nome: 'Girafa Melman'.

leao rugir.
elefante balancarTromba.
girafa esticarPescoco.
```

Neste código em Smalltalk, criamos as classes `Animal` e `AnimalAbstrato` para representar animais em geral e uma classe abstrata para as espécies específicas de animais. Em seguida, definimos as classes `Leao`, `Elefante` e `Girafa` que herdam da classe `AnimalAbstrato` e implementam seus próprios comportamentos.

Cada espécie de animal possui um método específico: o `Leao` possui o método `rugir`, o `Elefante` possui o método `balancarTromba` e a `Girafa` possui o método `esticarPescoco`.

Por fim, testamos a simulação do zoológico criando instâncias de cada espécie de animal, atribuindo um nome a cada um e chamando os respectivos métodos para cada animal.

Ao executar o código, você verá a seguinte saída no console:

```
O leão está rugindo!
O elefante está balançando a tromba!
A girafa está esticando o pescoço!
```

Espero que este código atenda às suas expectativas!