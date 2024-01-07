```typescript
// Import necessary libraries
import { Observable, of, from, range } from 'rxjs';
import { map, filter, switchMap, tap, delay } from 'rxjs/operators';

// Define an interface for our data model
interface Person {
  name: string;
  age: number;
}

// Create an observable of people
const people$: Observable<Person[]> = of([
  { name: 'Alice', age: 20 },
  { name: 'Bob', age: 30 },
  { name: 'Charlie', age: 40 }
]);

// Create an observable of ages
const ages$: Observable<number> = from([20, 30, 40]);

// Create an observable of numbers
const numbers$: Observable<number> = range(1, 10);

// Use the switchMap operator to combine the three observables
const combined$: Observable<any> = people$.pipe(
  switchMap(people => {
    return Observable.combineLatest(
      of(people),
      ages$,
      numbers$
    );
  })
);

// Subscribe to the combined observable
combined$.subscribe(data => {
  console.log(data);
});
```

This code demonstrates the use of the `switchMap` operator to combine three different observables: an observable of people, an observable of ages, and an observable of numbers. The `switchMap` operator allows us to subscribe to the first observable (people$) and then, for each emission from that observable, subscribe to the other two observables (ages$ and numbers$). This allows us to combine the data from the three observables into a single stream of data.

The `combineLatest` operator is used to combine the emissions from the three observables into a single array. The `of` operator is used to convert the array of people into an observable.

The `subscribe` method is used to subscribe to the combined observable and print the data to the console.

The output of this code will be an array of arrays, where each inner array contains the data from the three observables. For example, the first inner array will contain the data for Alice, her age, and the number 1. The second inner array will contain the data for Bob, his age, and the number 2, and so on.