// Java program to demonstrate working of
// Function invocation in Java

// Main class
class GFG {

	// Recursive function
	static int fun1(int n)
	{
		if (n <= 1)
			return n;
		return n * fun1(n - 1);
	}

	// Recursive function
	static int fun2(int n)
	{
		if (n <= 1)
			return n;
		return fun2(n - 1) * n;
	}

	// Recursive function
	static int fun3(int n)
	{
		if (n <= 1)
			return n;
		return fun3(n - 1) + n;
	}

	// Driver code
	public static void main(String[] args)
	{
		int n = 5;
		System.out.println(fun1(n));
		System.out.println(fun2(n));
		System.out.println(fun3(n));
	}
}

// This code demonstrates the working of function
// invocation in Java. It defines three recursive
// functions fun1(), fun2() and fun3() that calculate
// different values based on the input value n.

// The first function fun1() returns n! (factorial of
// n) by recursively multiplying n with the result of
// fun1(n-1).

// The second function fun2() returns n! (factorial of
// n) by recursively multiplying the result of fun2(n-1)
// with n.

// The third function fun3() returns the sum of n
// natural numbers by recursively adding n to the result
// of fun3(n-1).

// In the main() method, we call these functions with n
// as 5 and print the results.