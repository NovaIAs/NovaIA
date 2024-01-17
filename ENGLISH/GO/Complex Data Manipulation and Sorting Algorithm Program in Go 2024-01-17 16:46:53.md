```go
// A complex Go code

// Declare a custom type for representing a person
type Person struct {
	Name    string // The person's name
	Age     int    // The person's age
	Country string // The person's country
}

// Declare an interface for defining methods that operate on a slice of people
type PersonInterface interface {
	// Method to get the total age of all people in the slice
	GetTotalAge() int
	// Method to get the average age of all people in the slice
	GetAverageAge() float64
	// Method to get the names of all people in the slice
	GetNames() []string
}

// Implement the PersonInterface interface for a slice of people
type PeopleSlice []Person

// Method to get the total age of all people in the slice
func (p PeopleSlice) GetTotalAge() int {
	totalAge := 0
	for _, person := range p {
		totalAge += person.Age
	}
	return totalAge
}

// Method to get the average age of all people in the slice
func (p PeopleSlice) GetAverageAge() float64 {
	return float64(p.GetTotalAge()) / float64(len(p))
}

// Method to get the names of all people in the slice
func (p PeopleSlice) GetNames() []string {
	var names []string
	for _, person := range p {
		names = append(names, person.Name)
	}
	return names
}

// Function to sort a slice of people by age using the Merge Sort algorithm
func MergeSortPeople(people []Person) []Person {
	if len(people) <= 1 {
		return people
	}

	middle := len(people) / 2
	left := MergeSortPeople(people[:middle])
	right := MergeSortPeople(people[middle:])

	return MergePeople(left, right)
}

// Function to merge two sorted slices of people
func MergePeople(left, right []Person) []Person {
	result := make([]Person, 0, len(left)+len(right))
	i, j := 0, 0

	for i < len(left) && j < len(right) {
		if left[i].Age < right[j].Age {
			result = append(result, left[i])
			i++
		} else {
			result = append(result, right[j])
			j++
		}
	}

	for ; i < len(left); i++ {
		result = append(result, left[i])
	}

	for ; j < len(right); j++ {
		result = append(result, right[j])
	}

	return result
}

// Main function
func main() {
	// Create a slice of people
	// This slice is used to demonstrate the use of interfaces and
	// sorting algorithms defined above
	people := []Person{
		{Name: "John", Age: 25, Country: "USA"},
		{Name: "Mary", Age: 30, Country: "UK"},
		{Name: "Bob", Age: 40, Country: "Canada"},
		{Name: "Alice", Age: 20, Country: "Australia"},
		{Name: "Tom", Age: 35, Country: "India"},
	}

	// Print the original slice of people
	fmt.Println("Original slice of people:")
	for _, person := range people {
		fmt.Printf("Name: %s, Age: %d, Country: %s\n", person.Name, person.Age, person.Country)
	}

	// Implement the PersonInterface interface for the slice of people
	peopleInterface := PeopleSlice(people)

	// Get the total age of all people in the slice
	totalAge := peopleInterface.GetTotalAge()
	fmt.Printf("Total age of all people: %d\n", totalAge)

	// Get the average age of all people in the slice
	averageAge := peopleInterface.GetAverageAge()
	fmt.Printf("Average age of all people: %.2f\n", averageAge)

	// Get the names of all people in the slice
	names := peopleInterface.GetNames()
	fmt.Printf("Names of all people: %s\n", strings.Join(names, ", "))

	// Sort the slice of people by age in ascending order using the Merge Sort algorithm
	sortedPeople := MergeSortPeople(people)

	// Print the sorted slice of people
	fmt.Println("Sorted slice of people:")
	for _, person := range sortedPeople {
		fmt.Printf("Name: %s, Age: %d, Country: %s\n", person.Name, person.Age, person.Country)
	}
}
```