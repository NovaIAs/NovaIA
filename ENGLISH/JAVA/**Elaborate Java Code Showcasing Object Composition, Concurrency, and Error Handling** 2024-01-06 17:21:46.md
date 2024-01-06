```java
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

public class ComplexJavaCode {

    public static void main(String[] args) throws ExecutionException, InterruptedException, TimeoutException {
        // 1. Define a complex object with nested structures
        ComplexObject complexObject = new ComplexObject()
                .setName("Complex Object")
                .setNestedObject(new NestedObject()
                        .setValue1("Nested Value 1")
                        .setValue2("Nested Value 2"));

        // 2. Use Java 8 lambda expressions for concise and clear code
        List<String> stringList = Arrays.asList("A", "B", "C", "D", "E");
        List<String> filteredList = stringList.stream()
                .filter(s -> s.length() > 1)
                .map(String::toUpperCase)
                .collect(Collectors.toList());

        // 3. Leverage Java's concurrency features
        CompletableFuture<Integer> futureResult = CompletableFuture.supplyAsync(() -> {
            // Simulate a long-running task
            try {
                TimeUnit.SECONDS.sleep(5);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            return 10;
        });

        // 4. Handle asynchronous tasks with timeouts
        int result = futureResult.get(3, TimeUnit.SECONDS);
        System.out.println("Asynchronous result: " + result);

        // 5. Use Java's Optional class to handle null values elegantly
        Optional<String> optionalString = Optional.ofNullable("Hello World");
        String value = optionalString.orElse("Default Value");
        System.out.println("Optional value: " + value);

        // 6. Utilize Java's reflection API for dynamic method invocation
        Class<?> clazz = ComplexObject.class;
        Method method = clazz.getMethod("getNestedObject");
        NestedObject nestedObject = (NestedObject) method.invoke(complexObject);
        System.out.println("Nested object value: " + nestedObject.getValue1());

        // 7. Demonstrate Java's exception handling capabilities
        try {
            throw new IllegalArgumentException("Invalid argument");
        } catch (IllegalArgumentException e) {
            System.out.println("Caught IllegalArgumentException: " + e.getMessage());
        } finally {
            System.out.println("Finally block executed");
        }
    }

    private static class ComplexObject {
        private String name;
        private NestedObject nestedObject;

        public String getName() {
            return name;
        }

        public ComplexObject setName(String name) {
            this.name = name;
            return this;
        }

        public NestedObject getNestedObject() {
            return nestedObject;
        }

        public ComplexObject setNestedObject(NestedObject nestedObject) {
            this.nestedObject = nestedObject;
            return this;
        }
    }

    private static class NestedObject {
        private String value1;
        private String value2;

        public String getValue1() {
            return value1;
        }

        public NestedObject setValue1(String value1) {
            this.value1 = value1;
            return this;
        }

        public String getValue2() {
            return value2;
        }

        public NestedObject setValue2(String value2) {
            this.value2 = value2;
            return this;
        }
    }
}
```

Explanation:

1. **Complex Object with Nested Structures:**
   - We define a `ComplexObject` class with a nested `NestedObject` class, demonstrating object composition.

2. **Java 8 Lambda Expressions:**
   - We use lambda expressions to filter and transform a list of strings concisely and clearly.

3. **Java's Concurrency Features:**
   - We utilize `CompletableFuture` to perform an asynchronous task and retrieve its result with a timeout.

4. **Handling Asynchronous Tasks with Timeouts:**
   - We set a timeout for retrieving the result of the asynchronous task to prevent potential deadlocks.

5. **Java's Optional Class:**
   - We use `Optional` to handle nullable values elegantly, avoiding `NullPointerException` errors.

6. **Java's Reflection API:**
   - We dynamically invoke a method on an object using reflection, providing flexibility in method calls.

7. **Java's Exception Handling Capabilities:**
   - We demonstrate Java's exception handling with `try-catch-finally` blocks to handle runtime errors gracefully.

This code combines various Java features and demonstrates its versatility and power in building complex and sophisticated applications.