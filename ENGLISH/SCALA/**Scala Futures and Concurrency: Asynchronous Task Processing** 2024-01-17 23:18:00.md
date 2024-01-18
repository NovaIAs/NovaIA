```scala
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.{Failure, Success}

object ComplexScalaCode {

  def main(args: Array[String]): Unit = {

    // Define a generic function to convert a list of elements to a list of futures
    def toFutures[A](list: List[A])(f: A => Future[Unit]): List[Future[Unit]] =
      list.map(f)

    // Define a function to print a message after a future completes
    def printMessage(message: String): Future[Unit] =
      Future { println(message) }

    // Create a list of integers
    val numbers = List(1, 2, 3, 4, 5)

    // Create a list of futures by converting each integer to a future
    val futures: List[Future[Unit]] =
      toFutures(numbers)(x => printMessage(s"Number $x is processed"))

    // Wait for all futures to complete
    Await.result(Future.sequence(futures), 5.seconds)

    // Print a message after all futures have completed
    printMessage("All numbers are processed")
  }
}
```

The provided Scala code defines a complex and sophisticated program that demonstrates higher-order functions, Futures, and concurrency in Scala:

1. **Generic Function `toFutures`**: This function takes a list of elements and a function that converts each element to a Future. It then returns a list of Futures, providing a generic way to convert a list of elements into a list of concurrent tasks.

2. **Function `printMessage`**: This function returns a Future that prints a given message after it has completed. It's used to print messages once corresponding Futures are completed.

3. **Creating a List of Integers**: A list of integers `numbers` is defined.

4. **Converting Integers to Futures**: The `toFutures` function is used to convert each integer in the `numbers` list to a future using the `printMessage` function, where each Future is responsible for printing a message indicating that the corresponding integer has been processed.

5. **Waiting for Futures to Complete**: The `Await.result` method is used to wait for all Futures in the `futures` list to complete within a specified timeout period (5 seconds in this case). This ensures that all messages are printed before proceeding further.

6. **Printing a Final Message**: After all Futures have completed, a final message is printed using the `printMessage` function to indicate that all numbers have been processed.

This code demonstrates a complex combination of higher-order functions, Futures, and concurrency in Scala to perform asynchronous tasks in parallel and control the flow of execution.