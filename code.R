set.seed(420)

swap <- function(array){

	to_shift <- sample(c(1:length(array)), 2)
	array[to_shift] <- rev(array[to_shift])
	array

}


swap_sort <- function(n){

	array <- sample(c(1:n), n)
	dummy <- c(1:n)

	n_swaps <- 0

	while(sum((array - dummy)^2) != 0){

		array <- swap(array)
		n_swaps <- n_swaps + 1

	}

	return(n_swaps)

}

array_size <- 5

simulations <- replicate(10000, swap_sort(array_size))
bounds <- range(simulations)

max_var <- diff(bounds)^2 / 4
max_sample_mean_var <- max_var / length(simulations)

CI <- mean(simulations) + c(-1, 1) * qnorm(0.975) * sqrt(max_sample_mean_var)
print(CI)
print(mean(simulations))

p <- 1 / factorial(array_size)
your_answer <- (1 - p) / p
print(your_answer)


#-------
#P solving in 1

p_solve_1 <- length(which(simulations == 1)) / length(simulations)
E_p_var <- p_solve_1 * (1 - p_solve_1) / length(simulations)

CI <- p_solve_1 + c(-1, 1) * qnorm(0.975) * sqrt(E_p_var)
print(CI)
print(p_solve_1)


your_answer <- 1 / factorial(array_size)
print(your_answer)


#-------
#Analytical solution

library(combinat)
library(data.table)
library(dplyr)

n <- 5
n_states <- factorial(n)

indices <- combinat::combn(c(1:n), 2)

states <- combinat::permn(c(1:n))
names(states) <- c(1:length(states))
states <- lapply(states, function(x){as.data.table(t(x))})
states <- bind_rows(states)

transition_mat <- matrix(0, n_states, n_states)
states_compressed = apply(states, 1, function(x){paste(x, collapse = "")})

for(i in 1:nrow(transition_mat)){

	for(j in 1:ncol(indices)){

		next_state <- as.matrix(states[i, ])
		next_state[indices[, j]] <- rev(next_state[indices[, j]])
		next_state <- paste(next_state, collapse = "")

		transition_mat[i, which(states_compressed == next_state)] <- 1 / ncol(indices)

	}

}

transition_mat[1, ] <- 0
transition_mat[1, 1] <- 1


#Use the fact that X is bounded on [0, inf)
#I.e.: use the survival function to compute E[X]
v <- t(rep(1, n_states))

s <- (n_states - v[1]) / n_states

while(TRUE){

	s_last <- s

	v <- v %*% transition_mat
	s <- s + (n_states - v[1]) / n_states

	if(abs(s_last - s) <= 10^(-10)){

		break

	}


}

first_moment <- s
print(first_moment)
