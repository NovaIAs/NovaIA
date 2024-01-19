```kotlin
// This is a Kotlin program that implements a simple calculator.

// Create a class called Calculator with a main() method.
class Calculator {

    // Define the main() method.
    fun main(args: Array<String>) {

        // Print a welcome message.
        println("Welcome to the Kotlin calculator!")

        // Create a loop to repeatedly prompt the user for input.
        while (true) {

            // Prompt the user to enter an expression.
            println("Enter an expression:")

            // Read the user's input.
            val input = readLine() ?: continue

            // Parse the user's input into an expression tree.
            val expressionTree = ExpressionTree.parse(input)

            // Evaluate the expression tree.
            val result = expressionTree.evaluate()

            // Print the result.
            println("Result: $result")
        }
    }
}

// Create a class called ExpressionTree to represent an expression tree.
class ExpressionTree {

    // Define the root node of the expression tree.
    var root: Node? = null

    // Define a Node class to represent a node in the expression tree.
    class Node {

        // Define the data stored in the node.
        var data: Any? = null

        // Define the left and right child nodes.
        var left: Node? = null
        var right: Node? = null
    }

    // Define a method to parse an expression into an expression tree.
    companion object {

        // Define the precedence of operators.
        val precedence = mapOf(
                '+' to 1,
                '-' to 1,
                '*' to 2,
                '/' to 2
        )

        // Define a method to parse an expression into an expression tree.
        fun parse(input: String): ExpressionTree {

            // Create a new expression tree.
            val expressionTree = ExpressionTree()

            // Create a stack to store operators.
            val operatorStack = Stack<Node>()

            // Create a stack to store operands.
            val operandStack = Stack<Node>()

            // Tokenize the input.
            val tokens = input.split(" ")

            // Iterate over the tokens.
            for (token in tokens) {

                // If the token is an operator, push it onto the operator stack.
                if (token in precedence) {

                    // Get the precedence of the operator.
                    val precedence = precedence[token]!!

                    // While the operator stack is not empty and the precedence of the top
                    // operator is greater than or equal to the precedence of the current
                    // operator, pop operators from the operator stack and push them onto
                    // the operand stack.
                    while (operatorStack.isNotEmpty() && precedence[operatorStack.peek()!!.data as String]!! >= precedence) {

                        // Pop the top operator from the operator stack.
                        val operator = operatorStack.pop()

                        // Create a new node for the operator.
                        val node = Node()
                        node.data = operator.data

                        // Push the operands onto the node.
                        node.left = operandStack.pop()
                        node.right = operandStack.pop()

                        // Push the node onto the operand stack.
                        operandStack.push(node)
                    }

                    // Push the current operator onto the operator stack.
                    val node = Node()
                    node.data = token
                    operatorStack.push(node)
                }

                // Otherwise, the token is an operand.
                else {

                    // Create a new node for the operand.
                    val node = Node()
                    node.data = token

                    // Push the node onto the operand stack.
                    operandStack.push(node)
                }
            }

            // While the operator stack is not empty, pop operators from the operator stack
            // and push them onto the operand stack.
            while (operatorStack.isNotEmpty()) {

                // Pop the top operator from the operator stack.
                val operator = operatorStack.pop()

                // Create a new node for the operator.
                val node = Node()
                node.data = operator.data

                // Push the operands onto the node.
                node.left = operandStack.pop()
                node.right = operandStack.pop()

                // Push the node onto the operand stack.
                operandStack.push(node)
            }

            // The root of the expression tree is the only node left on the operand stack.
            expressionTree.root = operandStack.pop()

            // Return the expression tree.
            return expressionTree
        }
    }

    // Define a method to evaluate the expression tree.
    fun evaluate(): Any? {

        // If the root node is null, return null.
        if (root == null) {
            return null
        }

        // Evaluate the left and right subtrees.
        val leftValue = root!!.left!!.evaluate()
        val rightValue = root!!.right!!.evaluate()

        // If either of the subtrees evaluated to null, return null.
        if (leftValue == null || rightValue == null) {
            return null
        }

        // Get the operator.
        val operator = root!!.data as String

        // Evaluate the expression.
        when (operator) {

            "+" -> return leftValue as Double + rightValue as Double
            "-" -> return leftValue as Double - rightValue as Double
            "*" -> return leftValue as Double * rightValue as Double
            "/" -> return leftValue as Double / rightValue as Double
        }

        // If the operator is not recognized, return null.
        return null
    }
}

// Create a class called Stack to implement a stack data structure.
class Stack<T> {

    // Define the top of the stack.
    private var top: Node<T>? = null

    // Define a Node class to represent a node in the stack.
    private class Node<T>(val data: T, var next: Node<T>? = null)

    // Define a method to push an item onto the stack.
    fun push(data: T) {

        // Create a new node for the item.
        val node = Node(data)

        // If the stack is empty, set the top of the stack to the new node.
        if (top == null) {

            top = node
        }

        // Otherwise, set the next node of the new node to the top of the stack and
        // set the top of the stack to the new node.
        else {