```cool
class Counts {
  var count1: Int;
  var count2: Int;
  var count3: Int;

  constructor(count1: Int, count2: Int, count3: Int) {
    this.count1 = count1;
    this.count2 = count2;
    this.count3 = count3;
  }

  method incrementCount1() {
    count1 := count1 + 1;
  }

  method incrementCount2() {
    count2 := count2 + 1;
  }

  method incrementCount3() {
    count3 := count3 + 1;
  }

  method getCount1(): Int {
    return count1;
  }

  method getCount2(): Int {
    return count2;
  }

  method getCount3(): Int {
    return count3;
  }
}

class Main {
  var counts: Counts;

  constructor() {
    counts := new Counts(0, 0, 0);
  }

  method main() {
    counts.incrementCount1();
    counts.incrementCount2();
    counts.incrementCount3();

    print(counts.getCount1());
    print(counts.getCount2());
    print(counts.getCount3());
  }
}

Main().main();
```

This code defines two classes: `Counts` and `Main`. The `Counts` class has three integer instance variables (`count1`, `count2`, and `count3`) and five methods (`incrementCount1`, `incrementCount2`, `incrementCount3`, `getCount1`, and `getCount2`). The `Main` class has one instance variable (`counts`) and one method (`main`).

The `main` method of the `Main` class creates an instance of the `Counts` class and then calls the `incrementCount1`, `incrementCount2`, and `incrementCount3` methods on that instance. It then prints the values of the `count1`, `count2`, and `count3` instance variables.

The output of the program is:

```
1
1
1
```

This program demonstrates the use of classes and methods in COOL. It also shows how to use the `print` statement to print values to the console.