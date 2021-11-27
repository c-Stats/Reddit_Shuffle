# Reddit_Shuffle

Given an array of size n containing the numbers {1, ... , n}, a sorting algorithm works in the following way:

- Checks if the array is sorted
- If it isn't, then choose two numbers at random and swaps them

What is the expected number of swaps required to sort the array given it is initialized at random?


The problem above can be solved via Markov chains where the set of states is the set of n! possible permutations, with the sole ordered one being terminal. (Computationaly infeasibe for n > 7)
