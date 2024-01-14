```erlang
-module(sequence).
-export([new/0, next/1, reset/1]).

-record(sequence, {current}).

new() ->
    #sequence{current = 0}.

next(Seq) ->
    Seq#sequence{current = Seq#sequence.current + 1}.

reset(Seq) ->
    Seq#sequence{current = 0}.
```

This code defines an Erlang module named `sequence` that implements a simple sequence generator. The module exports three functions:

* `new/0`: Creates and returns a new sequence generator, initialized to 0.
* `next/1`: Takes a sequence generator as an argument and returns a new sequence generator with the current value incremented by 1.
* `reset/1`: Takes a sequence generator as an argument and returns a new sequence generator with the current value reset to 0.

The `-record(sequence, {current})` line defines a record named `sequence` with a single field named `current`. This field will be used to store the current value of the sequence generator.

The `new/0` function creates a new `sequence` record with the `current` field set to 0 and returns it.

The `next/1` function takes a `sequence` record as an argument and returns a new `sequence` record with the `current` field incremented by 1.

The `reset/1` function takes a `sequence` record as an argument and returns a new `sequence` record with the `current` field reset to 0.

To use this module, you can do the following:

```erlang
> Seq = sequence:new().
#sequence{current = 0}
> Seq1 = sequence:next(Seq).
#sequence{current = 1}
> Seq2 = sequence:next(Seq1).
#sequence{current = 2}
> Seq3 = sequence:reset(Seq2).
#sequence{current = 0}
```

This code creates a new sequence generator, increments it twice, and then resets it to 0.