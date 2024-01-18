```cool
class Garra {
  function Garra() {
    lock = new Lock();
  }
  function Lock() {
    barrier = 0;
    holder = nil;
  }
  function acquire() {
    lock.acquire();
    while (lock.holder ~= nil) {
      lock.barrier.wait();
    }
    lock.holder = self;
  }
  function release() {
    lock.holder = nil;
    lock.barrier.wakeAll();
    lock.release();
  }
}

class Account {
  function Account(init: Int) {
    balance = init;
    lock = new Garra();
  }
  function balance: Int;
  function lock: Garra;
  function deposit(amount: Int) {
    lock.acquire();
    balance += amount;
    lock.release();
  }
  function withdraw(amount: Int) {
    lock.acquire();
    balance -= amount;
    lock.release();
  }
}

class Thread {
  function Thread(f: (Void -> Void)) {
    pc = f;
  }
  function pc: (Void -> Void);
  function resume() {
    pc();
  }
}

class Main {
  function Main() {
    a = new Account(100);
    t1 = new Thread(new Thread1(self, a));
    t2 = new Thread(new Thread2(self, a));
    t1.resume();
    t2.resume();
  }
  function a: Account;
  function t1: Thread;
  function t2: Thread;
  function Thread1(m: Main, a: Account) {
    for i in 1 to 10 loop
      a.deposit(10);
    end loop;
    m.t2.join();
    print("Thread1: account balance = ", a.balance);
  }
  function Thread2(m: Main, a: Account) {
    for i in 1 to 10 loop
      a.withdraw(10);
    end loop;
    m.t1.join();
    print("Thread2: account balance = ", a.balance);
  }
}

```

Explanation:

This code implements a simple banking system with two threads that concurrently deposit and withdraw money from an account.

The `Garra` class implements a lock using the `Lock` class, which provides a barrier and a holder field.
The `acquire` method acquires the lock and waits until the holder is `nil`, indicating that the lock is free.
The `release` method releases the lock and wakes up all threads waiting on the barrier.

The `Account` class represents a bank account with a balance and a lock.
The `deposit` and `withdraw` methods use the lock to ensure that only one thread can access the account at a time.

The `Thread` class represents a thread of execution with a `pc` field that points to the function to be executed.
The `resume` method starts the thread by calling the function pointed to by `pc`.

The `Main` class creates an `Account` object and two `Thread` objects, one for each of the two threads that will deposit and withdraw money from the account.
The `Main` class then starts the two threads and waits for them to finish.

Finally, the two threads print the balance of the account after they have finished depositing and withdrawing money.

This code demonstrates how to use locks to synchronize access to shared data in a multithreaded program.