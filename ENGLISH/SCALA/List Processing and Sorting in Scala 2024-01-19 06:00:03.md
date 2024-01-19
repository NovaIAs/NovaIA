```scala
object ComplexScalaCode {

  // Define a higher-order function that takes a list of integers and a function
  // that takes an integer and returns an integer, and applies the function to each
  // element in the list.
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    list match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs)(f)
    }
  }

  // Define a function that takes an integer and returns its square.
  def square(n: Int): Int = n * n

  // Define a function that takes a list of integers and returns a list of their
  // squares.
  def squares(list: List[Int]): List[Int] = {
    map(list)(square)
  }

  // Define a function that takes a list of integers and returns the sum of their
  // squares.
  def sumOfSquares(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case x :: xs => square(x) + sumOfSquares(xs)
    }
  }

  // Define a function that takes a list of integers and a pivot value, and returns
  // a tuple containing a list of the elements less than the pivot value and a list
  // of the elements greater than or equal to the pivot value.
  def partition(list: List[Int], pivot: Int): (List[Int], List[Int]) = {
    list match {
      case Nil => (Nil, Nil)
      case x :: xs => {
        if (x < pivot) {
          val (lesser, greater) = partition(xs, pivot)
          (x :: lesser, greater)
        } else {
          val (lesser, greater) = partition(xs, pivot)
          (lesser, x :: greater)
        }
      }
    }
  }

  // Define a function that takes a list of integers and returns a sorted list.
  def sort(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case x :: xs => {
        val (lesser, greater) = partition(xs, x)
        sort(lesser) ::: x :: sort(greater)
      }
    }
  }

  // Define a function that takes a list of integers and a value, and returns a
  // list of the elements in the list that are greater than or equal to the value.
  def filter(list: List[Int], value: Int): List[Int] = {
    list match {
      case Nil => Nil
      case x :: xs => {
        if (x >= value) {
          x :: filter(xs, value)
        } else {
          filter(xs, value)
        }
      }
    }
  }

  // Define a function that takes a list of integers and a value, and returns a
  // list of the elements in the list that are less than the value.
  def filterLessThan(list: List[Int], value: Int): List[Int] = {
    list match {
      case Nil => Nil
      case x :: xs => {
        if (x < value) {
          x :: filterLessThan(xs, value)
        } else {
          filterLessThan(xs, value)
        }
      }
    }
  }

  // Define a function that takes a list of integers and returns a list of the
  // unique elements in the list.
  def distinct(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case x :: xs => {
        if (xs.contains(x)) {
          distinct(xs)
        } else {
          x :: distinct(xs)
        }
      }
    }
  }

  // Define a function that takes a list of integers and returns the maximum value in
  // the list.
  def max(list: List[Int]): Int = {
    list match {
      case Nil => throw new IllegalArgumentException("List cannot be empty")
      case x :: xs => {
        val maxTail = max(xs)
        if (x > maxTail) {
          x
        } else {
          maxTail
        }
      }
    }
  }

  // Define a function that takes a list of integers and returns the minimum value in
  // the list.
  def min(list: List[Int]): Int = {
    list match {
      case Nil => throw new IllegalArgumentException("List cannot be empty")
      case x :: xs => {
        val minTail = min(xs)
        if (x < minTail) {
          x
        } else {
          minTail
        }
      }
    }
  }

  // Define a function that takes a list of integers and a