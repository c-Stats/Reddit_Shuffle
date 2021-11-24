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
