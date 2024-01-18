```scala
object ComplexScalaCode {

  // Define a sealed trait to represent different types of expressions
  sealed trait Expr
  case class Num(value: Int) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  case class Mul(left: Expr, right: Expr) extends Expr
  case class Div(left: Expr, right: Expr) extends Expr

  // Define a function to evaluate an expression
  def eval(expr: Expr): Int = {
    // Pattern matching on the expression
    expr match {
      case Num(value) => value // Base case: return the value for a Num object
      case Add(left, right) => // Recursive case: evaluate the left and right expressions and add their results
        eval(left) + eval(right)
      case Sub(left, right) => // Recursive case: evaluate the left and right expressions and subtract their results
        eval(left) - eval(right)
      case Mul(left, right) => // Recursive case: evaluate the left and right expressions and multiply their results
        eval(left) * eval(right)
      case Div(left, right) => // Recursive case: evaluate the left and right expressions and divide their results
        eval(left) / eval(right)
    }
  }

  // Define a function to convert an expression to a string
  def toString(expr: Expr): String = {
    // Pattern matching on the expression
    expr match {
      case Num(value) => value.toString // Base case: return the string representation of the value for a Num object
      case Add(left, right) => // Recursive case: convert the left and right expressions to strings and concatenate them with a "+"
        toString(left) + " + " + toString(right)
      case Sub(left, right) => // Recursive case: convert the left and right expressions to strings and concatenate them with a "-"
        toString(left) + " - " + toString(right)
      case Mul(left, right) => // Recursive case: convert the left and right expressions to strings and concatenate them with a "*"
        toString(left) + " * " + toString(right)
      case Div(left, right) => // Recursive case: convert the left and right expressions to strings and concatenate them with a "/"
        toString(left) + " / " + toString(right)
    }
  }

  // Define a main method to test the code
  def main(args: Array[String]): Unit = {
    // Create an expression
    val expr = Add(Mul(Num(3), Num(5)), Sub(Num(10), Num(2)))

    // Evaluate the expression
    val result = eval(expr)

    // Convert the expression to a string
    val str = toString(expr)

    // Print the result and the string representation of the expression
    println(s"Expression: $str")
    println(s"Result: $result")
  }
}
```

Explanation:

1. We define a sealed trait `Expr` to represent different types of expressions. This is a common pattern in Scala to define algebraic data types.

2. We define several case classes that extend `Expr` to represent specific types of expressions, such as `Num` for numeric values, `Add` for addition, `Sub` for subtraction, `Mul` for multiplication, and `Div` for division.

3. We define a function `eval` to evaluate an expression. This function uses pattern matching to determine the type of expression and perform the appropriate operation.

4. We define a function `toString` to convert an expression to a string. This function also uses pattern matching to determine the type of expression and convert it to a string representation.

5. In the `main` method, we create an expression using the `Add`, `Mul`, and `Sub` case classes.

6. We call the `eval` function to evaluate the expression and store the result in the `result` variable.

7. We call the `toString` function to convert the expression to a string and store it in the `str` variable.

8. Finally, we print the result and the string representation of the expression to the console.

This code demonstrates several important concepts in Scala, including:

* Pattern matching
* Algebraic data types
* Recursive functions
* Method overloading