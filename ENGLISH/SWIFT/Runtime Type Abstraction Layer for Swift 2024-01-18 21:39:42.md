```swift
// Objective-C runtime type detection with protocol abstraction layer
// Inspired by SwiftProtobuf's MessageKit solution

import Foundation

// Storage of runtime types and their metadata
private var runtimeTypeMetadata: [ObjectIdentifier: [String: Any]] = [:]

// Protocol defining a runtime type's factory method
protocol RuntimeTypeRegistry {
    static var typeName: String { get }
}

// Generate a unique identifier for a runtime type
private func typeIdentifier<T: RuntimeTypeRegistry>(forType type: T.Type) -> ObjectIdentifier {
    return ObjectIdentifier(type)
}

// Register a type with its factory method
public func registerRuntimeType<T: RuntimeTypeRegistry>(_ type: T.Type) {
    let identifier = typeIdentifier(forType: type)
    runtimeTypeMetadata[identifier] = ["factory" : type.init]
}

// Protocol to mark that objects can assemble themselves from data
protocol Unpackable {
    init(fromData data: Data) throws
}

// Define a default implementation for creating instances from data based on registered runtime types
public func unpackRuntimeType(fromData data: Data) throws -> Any {
    // Check serialized typeName
    guard let typeName = String(data: data.prefix(64), encoding: .utf8), !typeName.isEmpty else {
        throw UnpackError.invalidTypeName
    }
    
    // Lookup metadata for typeName
    guard let metadata = runtimeTypeMetadata[typeIdentifier(forType: TypeName(typeName).self)] else {
        throw UnpackError.unregisteredType(typeName)
    }
    
    // Retrieve factory method and call it to create an instance
    guard let factory = metadata["factory"] as? (Data) -> Unpackable else {
        throw UnpackError.missingFactoryMethod(typeName)
    }
    
    return factory(data)
}

// Sample type that conforms to RuntimeTypeRegistry and Unpackable
public struct TestType: RuntimeTypeRegistry, Unpackable {
    public static var typeName: String { "TestType" }
    
    public init(fromData data: Data) throws {
        // Assume data contains a single Int32
        guard data.count == 4 else {
            throw UnpackError.invalidData(typeName: typeName)
        }
        self.value = Int32(bigEndian: data.withUnsafeBytes { $0.pointee })
    }
    
    public let value: Int32
}

// Enum defining errors during unpacking
public enum UnpackError: Error {
    case invalidTypeName
    case unregisteredType(String)
    case missingFactoryMethod(String)
    case invalidData(typeName: String)
}

// Sample usage
registerRuntimeType(TestType.self)

// Create a TestType instance
let testType = try! TestType(fromData: Data([3, 0, 0, 0])) // Initializes value with 3

// Retrieve stored value
print(testType.value) // Prints 3
```