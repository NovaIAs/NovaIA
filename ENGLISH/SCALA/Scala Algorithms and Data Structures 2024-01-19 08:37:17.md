```scala
// 1. Function to calculate the factorial of a number
def factorial(n: Int): BigInt = {
  if (n <= 1) 1
  else n * factorial(n - 1)
}

// 2. Function to check if a number is prime
def isPrime(n: Int): Boolean = {
  if (n <= 1) false
  else if (n == 2) true
  else !(2 to Math.sqrt(n).toInt).exists(i => n % i == 0)
}

// 3. Function to generate the Fibonacci sequence up to a specified number
def fibonacci(n: Int): List[Int] = {
  def fib(n: Int, prev: Int, cur: Int): List[Int] = n match {
    case 0 => List()
    case 1 => List(cur)
    case _ => fib(n - 1, cur, prev + cur) :+ cur
  }
  fib(n, 0, 1)
}

// 4. Function to merge two sorted lists into one sorted list
def mergeSort(list1: List[Int], list2: List[Int]): List[Int] = {
  (list1, list2) match {
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (x :: xs, y :: ys) if x < y => x :: mergeSort(xs, list2)
    case (x :: xs, y :: ys) => y :: mergeSort(list1, ys)
  }
}

// 5. Function to find the longest common substring between two strings
def longestCommonSubstring(str1: String, str2: String): String = {
  val len1 = str1.length
  val len2 = str2.length
  val dp = Array.ofDim[Int](len1 + 1, len2 + 1)
  var maxLen = 0
  var maxEnd = 0
  for (i <- 1 to len1; j <- 1 to len2) {
    if (str1(i - 1) == str2(j - 1)) {
      dp(i)(j) = dp(i - 1)(j - 1) + 1
      if (dp(i)(j) > maxLen) {
        maxLen = dp(i)(j)
        maxEnd = i
      }
    }
  }
  str1.substring(maxEnd - maxLen, maxEnd) // return the longest common substring
}

// 6. Function to find the shortest path between two nodes in a graph using Dijkstra's algorithm
def dijkstra(graph: Map[Int, List[(Int, Int)]], start: Int, end: Int): Int = {
  val distances = mutable.Map[Int, Int]()
  val visited = mutable.Set[Int]()

  distances(start) = 0
  while (visited.size < graph.size) {
    val (node, minDistance) = distances.filterNot { case (k, _) => visited.contains(k) }.minBy(_._2)
    visited += node

    for ((neighbor, weight) <- graph(node)) {
      if (!visited.contains(neighbor)) {
        val newDistance = minDistance + weight
        if (distances.get(neighbor).isEmpty || newDistance < distances(neighbor)) {
          distances(neighbor) = newDistance
        }
      }
    }
  }

  distances(end)
}

// 7. Function to implement a binary search tree data structure
class BinarySearchTree[T <% Ordered[T]] {
  private var root: Node[T] = null

  def insert(value: T): Unit = {
    if (root == null) {
      root = new Node(value)
    } else {
      insertNode(value, root)
    }
  }

  private def insertNode(value: T, node: Node[T]): Unit = {
    if (value < node.value) {
      if (node.left == null) {
        node.left = new Node(value)
      } else {
        insertNode(value, node.left)
      }
    } else {
      if (node.right == null) {
        node.right = new Node(value)
      } else {
        insertNode(value, node.right)
      }
    }
  }

  def search(value: T): Boolean = {
    var current = root
    while (current != null) {
      if (value == current.value) {
        return true
      } else if (value < current.value) {
        current = current.left
      } else {
        current = current.right
      }
    }
    false
  }

  def delete(value: T): Unit = {
    deleteNode(value, root)
  }

  private def deleteNode(value: T, node: Node[T]): Unit = {
    if (node == null) {
      return
    }

    if (value == node.value) {
      if (node.left == null) {
        node = node.right
      } else if (node.right == null) {
        node = node.left
      } else {
        val min = findMin(node.right)
        node.value = min.value
        deleteNode(min.value, node.right)
      }
    } else if (value < node.value) {
      deleteNode(value, node.left)
    } else {
      deleteNode(value, node.right)
    }
  }

  private def findMin(node: Node[T]): Node[T] = {
    if (node.left == null) {
      node
    } else {
      findMin(node.left)
    }
  }
}

// 8. Function to implement a simple neural network
class NeuralNetwork(layers: List[Int], activation: String = "relu") {
  private val weights = Array.ofDim[Double](layers.size - 1, layers(0), layers(1))
  private val biases = Array.ofDim[Double](layers.size - 1, layers(1))

  def forward(input: Array[Double]): Array[Double] = {
    var activations = input
    for (i <- 0 until layers.size - 1) {
      val weightedInputs = activations.zip(weights(i)).map { case (a, w) => a * w }.sum + biases(i)
      activations = activationFunction(weightedInputs, activation)
    }
    activations
  }

  private def activationFunction(x: Double, activation: String): Double = {
    activation match {
      case "relu" => Math.max(0, x)
      case "sigmoid" => 1 / (1 + Math.exp(-x))
      case "tanh" => Math.tanh(x)
      case _ => x
    }
  }

  def train(inputs: List[Array[Double]], labels: List[Array[Double]], epochs: Int, learningRate: Double): Unit = {
    for (epoch <- 0 until epochs) {
      for (i <- inputs.indices) {
        val output = forward(inputs(i))
        val error = labels(i).zip(output).map { case (l, o) => (l - o) * (l - o) }.sum
        updateWeightsAndBiases(inputs(i), labels(i), error, learningRate)
      }
    }
  }

  private def updateWeightsAndBiases(input: Array[Double],