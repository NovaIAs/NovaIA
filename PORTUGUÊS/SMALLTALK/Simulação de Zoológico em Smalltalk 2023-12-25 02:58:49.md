Claro! Aqui está um exemplo de código em Smalltalk que cria uma simulação de um zoológico com diferentes tipos de animais:

```smalltalk
Object subclass: #Animal
    instanceVariableNames: 'nome idade'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

Animal subclass: #Leao
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

Leao subclass: #Leoa
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

Animal subclass: #Girafa
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

Girafa subclass: #Girafinha
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Zoologico'

Zoologico := OrderedCollection new.

"Criando animais e adicionando ao zoológico"
leao1 := Leao new.
leao1 nome: 'Simba'.
leao1 idade: 5.
Zoologico add: leao1.

leoa1 := Leoa new.
leoa1 nome: 'Nala'.
leoa1 idade: 4.
Zoologico add: leoa1.

girafa1 := Girafa new.
girafa1 nome: 'Melman'.
girafa1 idade: 6.
Zoologico add: girafa1.

girafinha1 := Girafinha new.
girafinha1 nome: 'Gisele'.
girafinha1 idade: 1.
Zoologico add: girafinha1.

"Imprimindo informações dos animais no zoológico"
Zoologico do: [:animal | 
    Transcript
        show: 'Nome: ';
        show: animal nome;
        show: ' | Idade: ';
        show: animal idade;
        cr].
```

Neste código, começamos definindo uma classe base chamada "Animal" que possui as variáveis de instância "nome" e "idade". Em seguida, criamos duas subclasses de "Animal": "Leao" e "Girafa". Por sua vez, "Leao" tem uma subclasse chamada "Leoa", e "Girafa" possui uma subclasse chamada "Girafinha".

Após a definição das classes, criamos uma coleção ordenada chamada "Zoologico" para armazenar os animais. Em seguida, criamos instâncias dos animais com seus respectivos nomes e idades, e adicionamos cada animal ao zoológico.

Por fim, percorremos o zoológico e imprimimos as informações de cada animal no console usando o objeto "Transcript" do Smalltalk.

Espero que esse código atenda às suas expectativas! Caso precise de mais alguma coisa, estou à disposição para ajudar.