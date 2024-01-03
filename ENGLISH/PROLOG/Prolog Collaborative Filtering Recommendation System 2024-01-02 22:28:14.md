Certainly! Here's a complex Prolog code that implements a recommendation system based on collaborative filtering. This system suggests items to users based on their similarity to other users.

```prolog
% Data representation
user(john).
user(anna).
user(peter).
user(mary).
item(book).
item(movie).
item(song).

% User-item rating database
rating(john, book, 4).
rating(john, movie, 5).
rating(anna, book, 3).
rating(anna, movie, 4).
rating(peter, book, 5).
rating(peter, movie, 4).
rating(mary, book, 2).
rating(mary, movie, 3).
rating(mary, song, 5).

% Similarity calculation predicate
similarity(User1, User2, Similarity) :-
    findall(R1-R2, (rating(User1, Item, R1), rating(User2, Item, R2)), Ratings),
    length(Ratings, N),
    sum_ratings(Ratings, SumRatings),
    Avg1 is SumRatings / N,
    sum_sq_diff(Ratings, Avg1, SumSqDiff),
    StdDev1 is sqrt(SumSqDiff / N),
    (StdDev1 > 0 ->
        similarity_aux(Ratings, Avg1, StdDev1, 0, 0, Similarity)
    ;
        Similarity = 0
    ).

% Auxiliary predicates for similarity calculation
sum_ratings([], 0).
sum_ratings([R1-_|Ratings], Total) :-
    sum_ratings(Ratings, SubTotal),
    Total is R1 + SubTotal.

sum_sq_diff([], _, 0).
sum_sq_diff([R1-_|Ratings], Avg, SumSqDiff) :-
    sum_sq_diff(Ratings, Avg, SubSumSqDiff),
    SqDiff is (R1 - Avg) ** 2,
    SumSqDiff is SqDiff + SubSumSqDiff.

similarity_aux([], _, _, Prod, SumSqDiff2, Similarity) :-
    Similarity is Prod / SumSqDiff2.
similarity_aux([R1-R2|Ratings], Avg1, StdDev1, Prod, SumSqDiff2, Similarity) :-
    similarity_aux(Ratings, Avg1, StdDev1, SubProd, SubSumSqDiff2, Similarity),
    SubProd2 is ((R1 - Avg1) / StdDev1) * ((R2 - Avg1) / StdDev1),
    Prod is SubProd + SubProd2,
    SubSumSqDiff2 is SumSqDiff2 + 1.

% Recommendation generation
recommend(User, Item, Rating) :-
    user(User),
    item(Item),
    \+ rating(User, Item, _),
    findall(Similarity-OtherUser, (user(OtherUser), OtherUser \= User, similarity(User, OtherUser, Similarity)), Similarities),
    findall(R-S, (member(Sim-OtherUser, Similarities), rating(OtherUser, Item, R), S > 0), Ratings),
    sum_sim_ratings(Ratings, SumRatings),
    sum_similarities(Similarities, SumSimilarities),
    Rating is SumRatings / SumSimilarities.

% Auxiliary predicates for recommendation generation
sum_sim_ratings([], 0).
sum_sim_ratings([R-S|Ratings], Total) :-
    sum_sim_ratings(Ratings, SubTotal),
    Total is R * S + SubTotal.

sum_similarities([], 0).
sum_similarities([_-_|Similarities], Total) :-
    sum_similarities(Similarities, SubTotal),
    Total is 1 + SubTotal.
```

This code represents a basic collaborative filtering recommendation system. It starts by defining the users, items, and their ratings in the `user/1`, `item/1`, and `rating/3` predicates, respectively.

The `similarity/3` predicate calculates the similarity between two users based on their common ratings. It uses the average and standard deviation of the ratings to compute the similarity score. The `similarity_aux/6` predicate is an auxiliary predicate used for the calculation.

The `recommend/3` predicate generates recommendations for a user. It finds similar users and considers their ratings for unrated items. The recommendations are based on the weighted average of the ratings, where the weights are the similarities between the users. The `sum_sim_ratings/2` and `sum_similarities/2` predicates are auxiliary predicates used for the calculation.

To use this code, you can consult it in a Prolog interpreter and then make queries. For example, to recommend an item to a specific user, you can query:

```prolog
?- recommend(john, Item, Rating).
```

This will give you a recommended `Item` and its associated `Rating` for the user "john".

Note that this is a simplified example, and real-world recommendation systems are more complex and typically involve additional optimizations and techniques.