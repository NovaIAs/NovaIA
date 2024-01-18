```java
import java.util.*;

public class ComplexJavaCode {

    private static final String[] COLORS = {"Red", "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet"};

    public static void main(String[] args) {
        // Create a complex data structure: a map of lists of colors
        Map<String, List<String>> colorMap = new HashMap<>();

        // Fill the map with data
        for (String color : COLORS) {
            List<String> shades = new ArrayList<>();
            for (int i = 0; i < 5; i++) {
                shades.add(color + " shade " + i);
            }
            colorMap.put(color, shades);
        }

        // Print the map to console
        for (Map.Entry<String, List<String>> entry : colorMap.entrySet()) {
            System.out.println("Color: " + entry.getKey());
            for (String shade : entry.getValue()) {
                System.out.println("  Shade: " + shade);
            }
        }

        // Create a complex algorithm to find the most common color in the map
        Map<String, Integer> colorFrequencyMap = new HashMap<>();
        for (List<String> shades : colorMap.values()) {
            for (String shade : shades) {
                int frequency = colorFrequencyMap.getOrDefault(shade, 0);
                colorFrequencyMap.put(shade, frequency + 1);
            }
        }

        String mostCommonColor = null;
        int maxFrequency = 0;
        for (Map.Entry<String, Integer> entry : colorFrequencyMap.entrySet()) {
            if (entry.getValue() > maxFrequency) {
                maxFrequency = entry.getValue();
                mostCommonColor = entry.getKey();
            }
        }

        System.out.println("Most common color: " + mostCommonColor);

        // Create a complex function to generate random colors
        Random random = new Random();
        String randomColor = null;
        while (randomColor == null) {
            int red = random.nextInt(256);
            int green = random.nextInt(256);
            int blue = random.nextInt(256);
            if (red + green + blue > 384) {
                randomColor = String.format("#%02x%02x%02x", red, green, blue);
            }
        }

        System.out.println("Random color: " + randomColor);
    }
}
```

### Explanation:

1. We define an array of colors called `COLORS` and a map of lists of colors called `colorMap`.
2. We fill the map with data by iterating over the colors and adding a list of 5 shades for each color.
3. We print the map to the console to visualize it.
4. We create a complex algorithm to find the most common color in the map.
5. We create a complex function to generate random colors.
6. We generate a random color and print it to the console.

This code demonstrates various programming concepts including complex data structures, algorithms, and functions. It's intentionally complex and may be difficult to understand at first glance, but it showcases a variety of techniques that can be used in Java programming.