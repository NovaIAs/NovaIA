```smalltalk
```

**Class:** `PhoneNumber`

```smalltalk
PhoneNumber := Class new.
```

**Instance Variables:**

* `number`: The phone number as a string.
* `type`: The type of phone number (e.g., "home", "work", "mobile").

**Methods:**

* `initialize`: The constructor. Takes two arguments: the phone number and the type.

```smalltalk
PhoneNumber>>initialize: aNumber type: aType
	number := aNumber.
	type := aType.
```

* `number`: Returns the phone number.

```smalltalk
PhoneNumber>>number
	^number
```

* `type`: Returns the type of phone number.

```smalltalk
PhoneNumber>>type
	^type
```

* `to_s`: Returns a string representation of the phone number.

```smalltalk
PhoneNumber>>to_s
	^String streamContents: [:s | s nextPutAll: number; nextPut: $@; nextPutAll: type].
```

**Usage:**

```smalltalk
phone := PhoneNumber new initialize: '555-1212' type: 'home'.
phone number "-> '555-1212'"
phone type "-> 'home'"
phone to_s "-> '555-1212@home'"
```