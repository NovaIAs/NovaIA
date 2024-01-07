```scala
object ComplexScalaCode {

  // Define a sealed trait to represent different types of expressions.
  sealed trait Expr

  // Case classes for different types of expressions.
  case class Const(value: Int) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  case class Mul(left: Expr, right: Expr) extends Expr
  case class Div(left: Expr, right: Expr) extends Expr

  // Function to evaluate an expression.
  def eval(expr: Expr): Int = {
    expr match {
      case Const(value) => value
      case Add(left, right) => eval(left) + eval(right)
      case Sub(left, right) => eval(left) - eval(right)
      case Mul(left, right) => eval(left) * eval(right)
      case Div(left, right) => eval(left) / eval(right)
    }
  }

  // Function to generate a random expression.
  def generateExpr(depth: Int): Expr = {
    if (depth == 0) {
      Const(scala.util.Random.nextInt(10))
    } else {
      val op = scala.util.Random.nextInt(4)
      op match {
        case 0 => Add(generateExpr(depth - 1), generateExpr(depth - 1))
        case 1 => Sub(generateExpr(depth - 1), generateExpr(depth - 1))
        case 2 => Mul(generateExpr(depth - 1), generateExpr(depth - 1))
        case 3 => Div(generateExpr(depth - 1), generateExpr(depth - 1))
      }
    }
  }

  // Test the eval and generateExpr functions.
  def main(args: Array[String]): Unit = {
    val expr = generateExpr(5)
    println(s"Expression: $expr")
    println(s"Result: ${eval(expr)}")
  }
}
```

Explanation:

1. We define a sealed trait `Expr` to represent different types of expressions.
2. We define case classes for different types of expressions: `Const`, `Add`, `Sub`, `Mul`, and `Div`.
3. We define a function `eval` to evaluate an expression. It uses pattern matching to handle different types of expressions.
4. We define a function `generateExpr` to generate a random expression with a specified depth.
5. We test the `eval` and `generateExpr` functions in the `main` method.

This code demonstrates various concepts in Scala, including sealed traits, case classes, pattern matching, and recursion. It also shows how to generate and evaluate complex expressions.