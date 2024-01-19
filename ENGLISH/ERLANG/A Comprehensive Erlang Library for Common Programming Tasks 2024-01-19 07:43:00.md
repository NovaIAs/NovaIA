```erlang
% This is a very large and differentiated code in Erlang.
% It is unlikely to be repeated again.

-module(my_module).
-export([main/0]).

main() ->
    % This is a function that takes a list of numbers and returns the sum of the numbers.
    sum_numbers(List) ->
        lists:sum(List).

    % This is a function that takes a string and returns a list of the words in the string.
    split_string(String) ->
        string:tokens(String, " ").

    % This is a function that takes a list of words and returns a string with the words concatenated together.
    join_words(Words) ->
        string:join(Words, " ").

    % This is a function that takes a list of numbers and returns a list of the numbers sorted in ascending order.
    sort_numbers(Numbers) ->
        lists:sort(Numbers).

    % This is a function that takes a list of numbers and returns a list of the numbers sorted in descending order.
    reverse_sort_numbers(Numbers) ->
        lists:reverse(sort_numbers(Numbers)).

    % This is a function that takes a list of numbers and a number and returns a list of the numbers that are greater than the number.
    filter_numbers(Numbers, Number) ->
        lists:filter(fun(N) -> N > Number end, Numbers).

    % This is a function that takes a list of numbers and a number and returns a list of the numbers that are less than the number.
    filter_numbers_less_than(Numbers, Number) ->
        lists:filter(fun(N) -> N < Number end, Numbers).

    % This is a function that takes a list of numbers and a number and returns a list of the numbers that are equal to the number.
    filter_numbers_equal(Numbers, Number) ->
        lists:filter(fun(N) -> N = Number end, Numbers).

    % This is a function that takes a list of numbers and returns a list of the unique numbers in the list.
    unique_numbers(Numbers) ->
        lists:usort(Numbers).

    % This is a function that takes a list of numbers and returns a list of the numbers that are repeated in the list.
    repeated_numbers(Numbers) ->
        lists:filter(fun(N) -> lists:member(N, Numbers) end, Numbers).

    % This is a function that takes a list of numbers and returns a list of the numbers that are not repeated in the list.
    non_repeated_numbers(Numbers) ->
        lists:filter(fun(N) -> not lists:member(N, Numbers) end, Numbers).

    % This is a function that takes a list of numbers and returns the maximum number in the list.
    max_number(Numbers) ->
        lists:max(Numbers).

    % This is a function that takes a list of numbers and returns the minimum number in the list.
    min_number(Numbers) ->
        lists:min(Numbers).

    % This is a function that takes a list of numbers and returns the average of the numbers.
    avg_number(Numbers) ->
        sum_numbers(Numbers) / length(Numbers).

    % This is a function that takes a list of numbers and returns the median of the numbers.
    median_number(Numbers) ->
        SortedNumbers = sort_numbers(Numbers),
        Length = length(SortedNumbers),
        case Length rem 2 of
            0 ->
                (SortedNumbers