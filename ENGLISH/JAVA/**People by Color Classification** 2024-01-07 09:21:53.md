```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ComplexCode {

    private static final String[] FIRST_NAMES = {"John", "Mary", "Bob", "Alice", "Tom"};
    private static final String[] LAST_NAMES = {"Smith", "Jones", "Williams", "Brown", "Davis"};
    private static final String[] COLORS = {"Red", "Orange", "Yellow", "Green", "Blue"};

    public static void main(String[] args) {
        // Create a list of people with random names and colors
        List<Person> people = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            String firstName = FIRST_NAMES[(int) (Math.random() * FIRST_NAMES.length)];
            String lastName = LAST_NAMES[(int) (Math.random() * LAST_NAMES.length)];
            String color = COLORS[(int) (Math.random() * COLORS.length)];
            people.add(new Person(firstName, lastName, color));
        }

        // Create a map of colors to lists of people
        Map<String, List<Person>> colorMap = new HashMap<>();
        for (Person person : people) {
            String color = person.getColor();
            List<Person> colorList = colorMap.getOrDefault(color, new ArrayList<>());
            colorList.add(person);
            colorMap.put(color, colorList);
        }

        // Print the map of colors to lists of people
        for (Map.Entry<String, List<Person>> entry : colorMap.entrySet()) {
            String color = entry.getKey();
            List<Person> colorList = entry.getValue();
            System.out.println("Color: " + color);
            for (Person person : colorList) {
                System.out.println("\t" + person);
            }
        }
    }

    private static class Person {

        private String firstName;
        private String lastName;
        private String color;

        public Person(String firstName, String lastName, String color) {
            this.firstName = firstName;
            this.lastName = lastName;
            this.color = color;
        }

        public String getFirstName() {
            return firstName;
        }

        public void setFirstName(String firstName) {
            this.firstName = firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public void setLastName(String lastName) {
            this.lastName = lastName;
        }

        public String getColor() {
            return color;
        }

        public void setColor(String color) {
            this.color = color;
        }

        @Override
        public String toString() {
            return firstName + " " + lastName + " (" + color + ")";
        }
    }
}
```

Explanation:

1. **Data Generation**:
   - The code generates a list of 100 people with random first names, last names, and colors.

2. **Map Creation**:
   - It creates a HashMap where the keys are colors and the values are lists of people with that color.

3. **Populating the Map**:
   - The code iterates through the list of people and adds each person to the corresponding color list in the map.

4. **Printing the Map**:
   - It iterates through the map and prints the color and a list of people associated with that color.

5. **Person Class**:
   - The Person class represents a person with a first name, last name, and color.

6. **Person Methods**:
   - The Person class has getters and setters for the first name, last name, and color.

7. **Person toString() Method**:
   - The toString() method returns a string representation of the person, including their first name, last name, and color.