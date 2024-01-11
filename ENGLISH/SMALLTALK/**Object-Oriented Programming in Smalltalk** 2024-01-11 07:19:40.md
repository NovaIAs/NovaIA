```smalltalk
Class hierarchy:
Object
  | Animal
    | Cat Dog
    | Bird
      | Canary Robin
  | Person
    | Employee
      | Manager
        | CEO
    | Student
```

```smalltalk
Creating objects:
```

```smalltalk
aCat := Cat new.
aDog := Dog new.
aCanary := Canary new.
aRobin := Robin new.
aManager := Manager new.
aStudent := Student new.
```

```smalltalk
Sending messages:
```

```smalltalk
aCat meow.
aDog bark.
aCanary sing.
aRobin chirp.
aManager promote.
aStudent study.
```

```smalltalk
Accessing instance variables:
```

```smalltalk
aCat name := 'Fluffy'.
aDog name := 'Spot'.
aCanary name := 'Tweety'.
aRobin name := 'Chirpy'.
aManager name := 'John Smith'.
aStudent name := 'Jane Doe'.
```

```smalltalk
Using methods with arguments:
```

```smalltalk
aCat eat: 'fish'.
aDog eat: 'bone'.
aCanary eat: 'seed'.
aRobin eat: 'worm'.
aManager hire: aStudent.
aStudent enrollIn: 'Calculus 101'.
```

```smalltalk
Using blocks:
```

```smalltalk
aCat do: [:cat | cat meow].
aDog do: [:dog | dog bark].
aCanary do: [:canary | canary sing].
aRobin do: [:robin | robin chirp].
aManager do: [:manager | manager promote].
aStudent do: [:student | student study].
```

```smalltalk
Using loops:
```

```smalltalk
1 to: 10 do: [:i | aCat meow i times].
1 to: 10 do: [:i | aDog bark i times].
1 to: 10 do: [:i | aCanary sing i times].
1 to: 10 do: [:i | aRobin chirp i times].
1 to: 10 do: [:i | aManager promote i times].
1 to: 10 do: [:i | aStudent study i times].
```

```smalltalk
Using conditionals:
```

```smalltalk
if aCat isHungry then [aCat eat: 'fish'].
if aDog isHungry then [aDog eat: 'bone'].
if aCanary isHungry then [aCanary eat: 'seed'].
if aRobin isHungry then [aRobin eat: 'worm'].
if aManager isBusy then [aManager delegate].
if aStudent isBored then [aStudent play].
```

```smalltalk
Using exceptions:
```

```smalltalk
try
  [aCat eat: 'poison']
on: CatPoisonedException do: [:e | aCat goToVet].
```

```smalltalk
Using metaclasses:
```

```smalltalk
Cat metaclass extend: [
  :name :age
  ^ self new name: name age: age
].
aCat := Cat named: 'Fluffy' aged: 5.
```

Explanation:
```
1. Class hierarchy:
   Defines a class hierarchy with Object as the root class. Animal, Cat, Dog, Bird, Canary, Robin, Person, Employee, Manager, CEO, and Student are all subclasses of their respective parent classes.

2. Creating objects:
   Creates instances of various classes using the `new` method.

3. Sending messages:
   Sends messages to objects to invoke methods.

4. Accessing instance variables:
   Sets and gets the value of instance variables using the assignment operator `:=`.

5. Using methods with arguments:
   Calls methods with arguments.

6. Using blocks:
   Uses blocks to pass code as arguments to methods.

7. Using loops:
   Uses loops to repeatedly execute code.

8. Using conditionals:
   Uses conditional statements to control the flow of execution.

9. Using exceptions:
   Uses try/on blocks to handle exceptions.

10. Using metaclasses:
    Extends the metaclass of a class to define a new class method.
```