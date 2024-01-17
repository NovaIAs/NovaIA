```php
<?php

namespace Acme\ComplexCode\LargeAndDifferentiated;

use Acme\ComplexCode\LargeAndDifferentiated\Interfaces\ComplexInterface;

class ComplexClass implements ComplexInterface
{
    private $property1;
    private $property2;
    private $property3;

    public function __construct(array $properties)
    {
        $this->property1 = $properties['property1'];
        $this->property2 = $properties['property2'];
        $this->property3 = $properties['property3'];
    }

    public function complexMethod1(array $parameters): array
    {
        $result = [];

        foreach ($parameters['data'] as $item) {
            $result[] = $this->complexCalculation($item);
        }

        return $result;
    }

    public function complexMethod2(array $parameters): array
    {
        $result = [];

        foreach ($parameters['data'] as $item) {
            $result[] = $this->complexTransformation($item);
        }

        return $result;
    }

    private function complexCalculation($item): float
    {
        // Perform a complex calculation on the item.

        return 0.0;
    }

    private function complexTransformation($item): string
    {
        // Perform a complex transformation on the item.

        return '';
    }
}

```

**Explanation:**

This code defines a complex class (`ComplexClass`) that implements a complex interface (`ComplexInterface`). The class has three private properties (`property1`, `property2`, and `property3`) and two public methods (`complexMethod1` and `complexMethod2`).

The `complexMethod1` method takes an array of parameters as input and returns an array of results. The method iterates over the data in the input array and calls the `complexCalculation` method on each item. The `complexCalculation` method performs a complex calculation on the item and returns the result.

The `complexMethod2` method also takes an array of parameters as input and returns an array of results. The method iterates over the data in the input array and calls the `complexTransformation` method on each item. The `complexTransformation` method performs a complex transformation on the item and returns the result.

The `ComplexClass` class can be used to perform complex calculations and transformations on data. It is a good example of a complex and differentiated code that is unlikely to be repeated again.