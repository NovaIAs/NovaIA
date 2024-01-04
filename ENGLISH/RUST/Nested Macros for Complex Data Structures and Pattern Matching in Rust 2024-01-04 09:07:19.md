```rust
// Rust code to demonstrate the use of a complex macro with nested structures and pattern matching.

// Define a macro to create a new structure with fields based on a tuple of values.
macro_rules! create_struct {
    ($name:ident, $($field:ident: $type:ty),+) => {
        // Use struct! macro to define the structure.
        struct $name {
            $($field: $type),+
        }
    }
}

// Define a macro to create a new enum with variants based on a tuple of values.
macro_rules! create_enum {
    ($name:ident, $($variant:ident),+) => {
        // Use enum! macro to define the enum.
        enum $name {
            $($variant),+
        }
    }
}

// Define a complex macro that combines the above macros to create a new data structure.
macro_rules! complex_macro {
    ($name:ident, $struct_name:ident, $enum_name:ident, $($field:ident: $type:ty),+) => {
        // Use create_struct! macro to define the structure.
        create_struct!($struct_name, $($field: $type),+);

        // Use create_enum! macro to define the enum.
        create_enum!($enum_name, $($variant: $struct_name),+);

        // Use struct! macro to define the new data structure.
        struct $name {
            data: $enum_name,
        }
    }
}

// Use the complex macro to create a new data structure.
complex_macro!(
    ComplexData,
    PersonStruct,
    PersonEnum,
    name: String,
    age: u32,
    city: String
);

// Create an instance of the new data structure using the PersonStruct variant.
let person_data = ComplexData {
    data: PersonEnum::PersonStruct(PersonStruct {
        name: "John Doe".to_string(),
        age: 30,
        city: "New York".to_string(),
    }),
};

// Match on the data field of the ComplexData instance to access the PersonStruct variant.
match person_data.data {
    PersonEnum::PersonStruct(person) => {
        println!("Name: {}", person.name);
        println!("Age: {}", person.age);
        println!("City: {}", person.city);
    }
}

```

Explanation:

The provided Rust code demonstrates the use of a complex macro that combines the `create_struct!` and `create_enum!` macros to create a new data structure.

1. **Defining Macros**:
   - `create_struct!`: This macro takes a structure name and a list of field names and types as arguments and generates a new structure definition.
   - `create_enum!`: This macro takes an enum name and a list of variant names as arguments and generates a new enum definition.

2. **Complex Macro**:
   - `complex_macro!`: This macro combines the functionality of the `create_struct!` and `create_enum!` macros to create a new data structure. It takes a new data structure name, a structure name, an enum name, and a list of field names and types as arguments. It generates a new data structure definition that contains the enum as a field.

3. **Using the Complex Macro**:
   - The `complex_macro!` is used to create a new data structure named `ComplexData`. It defines a structure named `PersonStruct` with fields `name`, `age`, and `city`, and an enum named `PersonEnum` with a single variant `PersonStruct`. The `ComplexData` structure contains a field named `data` of type `PersonEnum`.

4. **Creating an Instance**:
   - An instance of the `ComplexData` structure is created using the `PersonStruct` variant. The `name`, `age`, and `city` fields are initialized with appropriate values.

5. **Pattern Matching**:
   - The `match` expression is used to match on the `data` field of the `ComplexData` instance. The `PersonStruct` variant is matched, and the fields of the `PersonStruct` are accessed and printed.

This code demonstrates the use of macros to create complex data structures and perform pattern matching on them. It shows how macros can be used to generate code dynamically and simplify the development process.