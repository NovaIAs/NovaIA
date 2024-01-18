```groovy
//Define a class and assign a name to the class

class GroovyScript {
    //Define a method and assign a name to the method
    def runScript() {

        //Here we define a variable and assign a value to that variable
        def name = "John Doe"

        //Here we print the value of the variable name using println method
        println("Hello ${name}!")

        //Here we define a list of numbers and assign values to the list using []
        def numbers = [1, 2, 3, 4, 5]

        //Here we iterate through the list using each method and print each element of the list
        numbers.each { println(it) }

        //Here we define a map and assign key, value pairs to the map using [:]
        def person = [name: "Jane Doe", age: 30]

        //Here we print the value of the key name using get method
        println("Name: ${person.get('name')}")

        //Here we define a method and assign a name to the method
        def greet(String name) {
            println("Hello ${name}!")
        }

        //Here we call the method greet and pass a value as an argument
        greet("Bob")

        //Here we define a closure and assign values to the closure using ->
        def closure = { name -> println("Hello ${name}!") }

        //Here we call the closure and pass a value as an argument
        closure.call("Alice")
    }

    //Here we define another method and assign a name to the method
    def printArray1(int[] arr) {
        for (int i = 0; i < arr.length; i++) {
            println(arr[i])
        }
    }

    //Here we define another method and assign a name to the method
    def printArray2(int[] arr) {
        arr.each { println(it) }
    }

    //Here we define another method and assign a name to the method
    def findMax(int[] arr) {
        int max = arr[0]
        for (int i = 1; i < arr.length; i++) {
            if (arr[i] > max) {
                max = arr[i]
            }
        }
        return max
    }

    //Here we define another method and assign a name to the method
    def sortArray(int[] arr) {
        Arrays.sort(arr)
    }

    //Here we define another method and assign a name to the method
    def reverseArray(int[] arr) {
        Collections.reverse(arr)
    }

    //Here we define another method and assign a name to the method
    def mergeArrays(int[] arr1, int[] arr2) {
        int[] mergedArray = new int[arr1.length + arr2.length]
        System.arraycopy(arr1, 0, mergedArray, 0, arr1.length)
        System.arraycopy(arr2, 0, mergedArray, arr1.length, arr2.length)
        return mergedArray
    }

    //Here we define another method and assign a name to the method
    def findIntersection(int[] arr1, int[] arr2) {
        Set<Integer> intersection = new HashSet<>()
        for (int i = 0; i < arr1.length; i++) {
            for (int j = 0; j < arr2.length; j++) {
                if (arr1[i] == arr2[j]) {
                    intersection.add(arr1[i])
                }
            }
        }
        return intersection
    }

    //Here we define another method and assign a name to the method
    def findUnion(int[] arr1, int[] arr2) {
        Set<Integer> union = new HashSet<>()
        for (int i = 0; i < arr1.length; i++) {
            union.add(arr1[i])
        }
        for (int i = 0; i < arr2.length; i++) {
            union.add(arr2[i])
        }
        return union
    }

    //Here we define another method and assign a name to the method
    def findDifference(int[] arr1, int[] arr2) {
        Set<Integer> difference = new HashSet<>()
        for (int i = 0; i < arr1.length; i++) {
            boolean found = false
            for (int j = 0; j < arr2.length; j++) {
                if (arr1[i] == arr2[j]) {
                    found = true
                    break
                }
            }
            if (!found) {
                difference.add(arr1[i])
            }
        }
        return difference
    }

    //Here we define another method and assign a name to the method
    def findSymmetricDifference(int[] arr1, int[] arr2) {
        Set<Integer> symmetricDifference = new HashSet<>()
        for (int i = 0; i < arr1.length; i++) {
            boolean found = false
            for (int j = 0; j < arr2.length; j++) {
                if (arr1[i] == arr2[j]) {
                    found = true
                    break
                }
            }
            if (!found) {
                symmetricDifference.add(arr1[i])
            }
        }
        for (int i = 0; i < arr2.length; i++) {
            boolean found = false
            for (int j = 0; j < arr1.length; j++) {
                if (arr2[i] == arr1[j]) {
                    found = true
                    break
                }
            }
            if (!found) {
                symmetricDifference.add(arr2[i])
            }
        }
        return symmetricDifference
    }

    //Here we define another method and assign a name to the method
    def findCommonElements(int[] arr1, int[] arr2, int[] arr3) {
        Set<Integer> commonElements = new HashSet<>()
        for (int i = 0; i < arr1.length; i++) {
            boolean foundInAllArrays = true
            for (int j = 0; j < arr2.length; j++) {
                if (arr1[i] != arr2[j]) {
                    foundInAllArrays = false
                    break
                }
            }
            for (int j = 0; j < arr3.length; j++) {
                if (arr1[i] != arr3[j]) {
                    foundInAllArrays = false
                    break
                }
            }
            if (foundInAllArrays) {
                commonElements.add(arr1[i])
            }
        }
        return commonElements
    }
}

//Here we create an instance of the class GroovyScript and assign it to the variable script
def script = new GroovyScript()

//Here we call the method runScript on the instance script
script.runScript()

//Here we define an array of numbers and assign values to the array using []
def arr1 = [1, 2, 3, 4, 5]

//Here we call the method printArray1 on the instance script and pass the array arr1 as an argument
script.printArray1(arr1)

//Here we call the method printArray2 on the instance script and pass the array arr1 as an argument
script.printArray2(arr1)

//Here we call the method findMax on the instance script and pass the array arr1 as an argument
def max = script.findMax(arr1)

//Here we print the value of the variable max using println method
println("Maximum: ${max}")

//Here we call the method sortArray on the instance script and pass the array arr1 as an argument
script.sortArray(arr1)

//Here we print the sorted array using println method
println("Sorted array: ${arr1}")

//Here we call the method reverseArray on the instance script and pass the array arr1 as an argument
script.reverseArray(arr1)

//Here we print the reversed array using println method
println("Reversed array: ${arr1}")

//Here we define another array of numbers and assign values to the array using []
def arr2 = [6, 7, 8, 9, 10]

//Here we call the method mergeArrays on the instance script and pass the arrays arr1 and arr2 as arguments
def mergedArray = script.mergeArrays(arr1, arr2)

//Here we print the merged array using println method
println("Merged array: ${mergedArray}")

//Here we call the method findIntersection on the instance script and pass the arrays arr1 and arr2 as arguments
def intersection = script.findIntersection(arr1, arr2)

//Here we print the intersection of