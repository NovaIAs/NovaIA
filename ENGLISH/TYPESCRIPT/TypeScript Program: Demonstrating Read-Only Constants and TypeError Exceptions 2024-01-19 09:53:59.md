```typescript
// JavaScript Program to Illustrate that
// SOME_CONSTANT is a read-only property
// Const keyword is used to make constants

// Function to show that
// SOME_CONSTANT is read-only
function showConst() {
	// Changing the value SOME_CONSTANT
	// Will raise TypeError exception
	SOME_CONSTANT = 100;
}

// Declaring SOME_CONSTANT as read-only variable
const SOME_CONSTANT = 50;

// Calling the showConst function
showConst();
```

Explanation:

1. **Const Keyword:** In TypeScript, the const keyword is used to declare constants, which are variables whose values cannot be reassigned after initialization.

2. **SOME_CONSTANT:** SOME_CONSTANT is declared as a constant using the const keyword. It is initialized with the value 50.

3. **showConst Function:** The showConst function is defined to demonstrate that SOME_CONSTANT is read-only. Inside the function, there's an attempt to change the value of SOME_CONSTANT.

4. **TypeError Exception:** When you try to reassign the value of a constant, it will raise a TypeError exception. In this case, when you call the showConst function, it will throw a TypeError because SOME_CONSTANT is read-only.

5. **Calling the Function:** The showConst function is called to demonstrate the read-only behavior of SOME_CONSTANT. When you run the code, you will see the TypeError exception being thrown.

This code illustrates the use of the const keyword to declare constants and shows how constants cannot be reassigned after initialization.