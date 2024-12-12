# OPTIMIZATION FOR BUSINESS ANALYTICS II
# Aircraft Boarding Optimization Problem

# This script simulates and optimizes aircraft boarding strategies using various
# optimization algorithms. It compares traditional boarding methods (like front-to-back,
# back-to-front, etc.) with optimized solutions from different algorithms.

# Load required libraries
# dplyr: Data manipulation
# ggplot2: Visualization
# genalg: Genetic algorithms
# DEoptim: Differential evolution
# pso: Particle swarm optimization
# tabuSearch: Tabu search implementation
# tidyr, scales: Data formatting and scaling
library(dplyr)
library(ggplot2)
library(genalg)
library(DEoptim)
library(pso)
library(tabuSearch)
library(tidyr)
library(scales)

# Set seed for reproducibility
set.seed(123)

#--------------------
# Parameters and Constants
#--------------------
# Aircraft configuration
# A typical single-aisle aircraft layout with:
# - 30 rows
# - 6 seats per row (3 on each side)
# - Total of 180 passengers
num_rows <- 30
seats_per_row <- 6
total_passengers <- num_rows * seats_per_row

# Luggage distribution parameters
# Models realistic passenger behavior:
# - 15% carry no bags (quick boarding)
# - 50% have one bag (typical case)
# - 35% have two bags (slower boarding)
luggage_probs <- c(
  no_bags = 0.15,
  one_bag = 0.50,
  two_bags = 0.35
)

# Time required for luggage handling
# Based on empirical observations:
# - No bags: Immediate seating
# - One bag: Brief overhead bin use
# - Two bags: Extended overhead bin use
luggage_times <- c(
  no_bags = 0,
  one_bag = 5,
  two_bags = 10
)

#--------------------
# Optimization Algorithms
#--------------------

# Blind Search Implementation
blind_search <- function(fn, dimensions, max_iter=1000, report_iter=100) {
  best_solution <- sample(1:dimensions)
  best_value <- fn(best_solution)
  
  for(i in 1:max_iter) {
    current <- sample(1:dimensions)
    current_value <- fn(current)
    
    if(current_value < best_value) {
      best_solution <- current
      best_value <- current_value
      if(i %% report_iter == 0) {
        cat("Iteration", i, ": New best =", best_value, "\n")
      }
    }
  }
  
  return(list(
    solution = best_solution,
    value = best_value,
    iterations = max_iter
  ))
}

# Monte Carlo Search Implementation
monte_carlo_search <- function(fn, dimensions, max_iter=1000, num_samples=10, report_iter=100) {
  best_solution <- sample(1:dimensions)
  best_value <- fn(best_solution)
  
  for(i in 1:max_iter) {
    # Generate multiple samples
    samples <- replicate(num_samples, sample(1:dimensions), simplify=FALSE)
    values <- sapply(samples, fn)
    
    # Update if better solution found
    current_best <- min(values)
    if(current_best < best_value) {
      best_value <- current_best
      best_solution <- samples[[which.min(values)]]
      if(i %% report_iter == 0) {
        cat("Iteration", i, ": New best =", best_value, "\n")
      }
    }
  }
  
  return(list(
    solution = best_solution,
    value = best_value,
    iterations = max_iter
  ))
}

# Hill Climbing Implementation
#
# A local search algorithm that:
# 1. Starts with an initial solution
# 2. Explores neighboring solutions by swapping passenger positions
# 3. Accepts improvements until no better solution is found
# 4. Includes early stopping if no improvements are found
#
# Parameters:
# - initial_solution: Starting boarding sequence
# - fn: Objective function (simulate_boarding)
# - max_iter: Maximum iterations
# - report_iter: Report progress every N iterations
# - max_no_improve: Stop after N iterations without improvement
hill_climbing <- function(initial_solution, fn, max_iter=100, report_iter=10, max_no_improve=20) {
  current <- initial_solution
  current_value <- fn(current)
  best_solution <- current
  best_value <- current_value
  no_improve_count <- 0
  
  for(i in 1:max_iter) {
    # Generate and evaluate multiple neighbors to reduce local optima risk
    num_neighbors <- 5
    best_neighbor <- current
    best_neighbor_value <- current_value
    
    for(j in 1:num_neighbors) {
      # Create neighbor by swapping two random passenger positions
      # This maintains a valid permutation
      neighbor <- current
      idx <- sample(1:length(neighbor), 2)
      neighbor[idx[1]] <- current[idx[2]]
      neighbor[idx[2]] <- current[idx[1]]
      
      # Evaluate neighbor
      neighbor_value <- fn(neighbor)
      
      # Keep track of best neighbor
      if(neighbor_value < best_neighbor_value) {
        best_neighbor <- neighbor
        best_neighbor_value <- neighbor_value
      }
    }
    
    # Accept if better
    if(best_neighbor_value < current_value) {
      current <- best_neighbor
      current_value <- best_neighbor_value
      no_improve_count <- 0
      
      if(current_value < best_value) {
        best_solution <- current
        best_value <- current_value
        if(i %% report_iter == 0) {
          cat("Iteration", i, ": New best =", current_value, "\n")
        }
      }
    } else {
      no_improve_count <- no_improve_count + 1
      if(no_improve_count >= max_no_improve) {
        cat("Stopping early: No improvement for", max_no_improve, "iterations\n")
        break
      }
    }
  }
  
  return(list(
    solution = best_solution,
    value = best_value,
    iterations = i
  ))
}

# Simulated Annealing Implementation
simulated_annealing <- function(initial_solution, fn, max_iter=1000, initial_temp=100, cooling_rate=0.95, report_iter=100) {
  current <- initial_solution
  current_value <- fn(current)
  best_solution <- current
  best_value <- current_value
  temp <- initial_temp
  
  for(i in 1:max_iter) {
    # Generate neighbor by swapping two positions
    neighbor <- current
    idx <- sample(1:length(neighbor), 2)
    neighbor[idx[1]] <- current[idx[2]]
    neighbor[idx[2]] <- current[idx[1]]
    
    neighbor_value <- fn(neighbor)
    delta <- neighbor_value - current_value
    
    # Accept based on temperature probability
    if(delta < 0 || runif(1) < exp(-delta/temp)) {
      current <- neighbor
      current_value <- neighbor_value
      
      if(current_value < best_value) {
        best_solution <- current
        best_value <- current_value
        if(i %% report_iter == 0) {
          cat("Iteration", i, ": New best =", best_value, "\n")
        }
      }
    }
    
    temp <- temp * cooling_rate
  }
  
  return(list(
    solution = best_solution,
    value = best_value,
    iterations = max_iter
  ))
}

# Tabu Search Implementation
tabu_search <- function(initial_solution, fn, max_iter=1000, tabu_size=20, report_iter=100) {
  current <- initial_solution
  current_value <- fn(current)
  best_solution <- current
  best_value <- current_value
  tabu_list <- list()
  
  for(i in 1:max_iter) {
    # Generate neighbors
    neighbors <- list()
    neighbor_values <- c()
    
    # Create multiple neighbors by swapping
    for(j in 1:5) {
      neighbor <- current
      idx <- sample(1:length(neighbor), 2)
      move_key <- paste(sort(idx), collapse=",")
      
      if(!move_key %in% tabu_list) {
        neighbor[idx[1]] <- current[idx[2]]
        neighbor[idx[2]] <- current[idx[1]]
        neighbors[[length(neighbors) + 1]] <- neighbor
        neighbor_values <- c(neighbor_values, fn(neighbor))
      }
    }
    
    if(length(neighbors) > 0) {
      best_idx <- which.min(neighbor_values)
      current <- neighbors[[best_idx]]
      current_value <- neighbor_values[best_idx]
      
      if(current_value < best_value) {
        best_solution <- current
        best_value <- current_value
        if(i %% report_iter == 0) {
          cat("Iteration", i, ": New best =", best_value, "\n")
        }
      }
      
      # Update tabu list
      tabu_list[[length(tabu_list) + 1]] <- move_key
      if(length(tabu_list) > tabu_size) {
        tabu_list <- tabu_list[-1]
      }
    }
  }
  
  return(list(
    solution = best_solution,
    value = best_value,
    iterations = max_iter
  ))
}

# Differential Evolution Implementation
differential_evolution <- function(fn, dimensions, max_gen=100, pop_size=50, F=0.8, CR=0.9, report_iter=10) {
  # Initialize population
  population <- lapply(1:pop_size, function(x) sample(1:dimensions))
  fitness <- sapply(population, fn)
  best_solution <- population[[which.min(fitness)]]
  best_value <- min(fitness)
  
  for(gen in 1:max_gen) {
    for(i in 1:pop_size) {
      # Select three random individuals
      idx <- sample((1:pop_size)[-i], 3)
      a <- population[[idx[1]]]
      b <- population[[idx[2]]]
      c <- population[[idx[3]]]
      
      # Create trial vector
      trial <- a
      if(runif(1) < CR) {
        pos <- sample(1:dimensions, 2)
        trial[pos[1]] <- trial[pos[2]]
        trial[pos[2]] <- trial[pos[1]]
      }
      
      # Selection
      trial_fitness <- fn(trial)
      if(trial_fitness < fitness[i]) {
        population[[i]] <- trial
        fitness[i] <- trial_fitness
        
        if(trial_fitness < best_value) {
          best_value <- trial_fitness
          best_solution <- trial
          if(gen %% report_iter == 0) {
            cat("Generation", gen, ": New best =", best_value, "\n")
          }
        }
      }
    }
  }
  
  return(list(
    solution = best_solution,
    value = best_value,
    iterations = max_gen
  ))
}

# Particle Swarm Implementation
particle_swarm <- function(fn, dimensions, max_iter=100, swarm_size=30, w=0.7, c1=1.5, c2=1.5, report_iter=10) {
  # Initialize swarm
  particles <- lapply(1:swarm_size, function(x) sample(1:dimensions))
  velocities <- lapply(1:swarm_size, function(x) runif(dimensions, -1, 1))
  pbest <- particles
  pbest_values <- sapply(particles, fn)
  gbest <- particles[[which.min(pbest_values)]]
  gbest_value <- min(pbest_values)
  
  for(iter in 1:max_iter) {
    for(i in 1:swarm_size) {
      # Update velocity and position
      r1 <- runif(dimensions)
      r2 <- runif(dimensions)
      
      new_vel <- velocities[[i]]
      new_vel <- w * new_vel + 
        c1 * r1 * (as.numeric(pbest[[i]] != particles[[i]])) +
        c2 * r2 * (as.numeric(gbest != particles[[i]]))
      
      particle <- particles[[i]]
      for(j in 1:dimensions) {
        if(runif(1) < abs(new_vel[j])) {
          swap_idx <- sample(1:dimensions, 1)
          temp <- particle[j]
          particle[j] <- particle[swap_idx]
          particle[swap_idx] <- temp
        }
      }
      
      # Evaluate new position
      new_value <- fn(particle)
      
      # Update personal best
      if(new_value < pbest_values[i]) {
        pbest[[i]] <- particle
        pbest_values[i] <- new_value
        
        # Update global best
        if(new_value < gbest_value) {
          gbest <- particle
          gbest_value <- new_value
          if(iter %% report_iter == 0) {
            cat("Iteration", iter, ": New best =", gbest_value, "\n")
          }
        }
      }
      
      # Update particle and velocity
      particles[[i]] <- particle
      velocities[[i]] <- new_vel
    }
  }
  
  return(list(
    solution = gbest,
    value = gbest_value,
    iterations = max_iter
  ))
}

# Genetic Algorithm Implementation
genetic_algorithm <- function(fn, dimensions, max_gen=100, pop_size=50,
                              mutation_rate=0.1, crossover_rate=0.8,
                              tournament_size=3, report_iter=10) {
  
  # Initialize population with valid permutations
  population <- lapply(1:pop_size, function(x) sample(1:dimensions))
  fitness <- sapply(population, fn)
  best_solution <- population[[which.min(fitness)]]
  best_value <- min(fitness)
  
  # Order Crossover (OX) for permutations
  order_crossover <- function(p1, p2) {
    n <- length(p1)
    # Select two random crossover points
    points <- sort(sample(1:(n-1), 2))
    start <- points[1]
    end <- points[2]
    
    # Create children
    child1 <- rep(NA, n)
    child2 <- rep(NA, n)
    
    # Copy the segments between crossover points
    child1[start:end] <- p1[start:end]
    child2[start:end] <- p2[start:end]
    
    # Fill remaining positions
    fill1 <- p2[!(p2 %in% child1[start:end])]
    fill2 <- p1[!(p1 %in% child2[start:end])]
    
    child1[c(1:(start-1), (end+1):n)] <- fill1
    child2[c(1:(start-1), (end+1):n)] <- fill2
    
    return(list(child1=child1, child2=child2))
  }
  
  # Swap mutation for permutations
  swap_mutation <- function(solution) {
    pos <- sample(1:length(solution), 2)
    solution[c(pos[1], pos[2])] <- solution[c(pos[2], pos[1])]
    return(solution)
  }
  
  # Tournament selection
  tournament_select <- function(fitness, size) {
    candidates <- sample(1:length(fitness), size)
    winner <- candidates[which.min(fitness[candidates])]
    return(winner)
  }
  
  for(gen in 1:max_gen) {
    new_population <- list()
    
    # Elitism - keep best solution
    elite_idx <- which.min(fitness)
    new_population[[1]] <- population[[elite_idx]]
    
    while(length(new_population) < pop_size) {
      # Tournament selection
      parent1 <- population[[tournament_select(fitness, tournament_size)]]
      parent2 <- population[[tournament_select(fitness, tournament_size)]]
      
      # Crossover
      if(runif(1) < crossover_rate) {
        children <- order_crossover(parent1, parent2)
        child1 <- children$child1
        child2 <- children$child2
      } else {
        child1 <- parent1
        child2 <- parent2
      }
      
      # Mutation
      if(runif(1) < mutation_rate) child1 <- swap_mutation(child1)
      if(runif(1) < mutation_rate) child2 <- swap_mutation(child2)
      
      new_population[[length(new_population) + 1]] <- child1
      if(length(new_population) < pop_size) {
        new_population[[length(new_population) + 1]] <- child2
      }
    }
    
    # Update population and evaluate
    population <- new_population
    fitness <- sapply(population, fn)
    
    # Update best solution
    gen_best <- min(fitness)
    if(gen_best < best_value) {
      best_value <- gen_best
      best_solution <- population[[which.min(fitness)]]
      if(gen %% report_iter == 0) {
        cat("Generation", gen, ": New best =", best_value, "\n")
      }
    }
  }
  
  return(list(
    solution = best_solution,
    value = best_value,
    iterations = max_gen
  ))
}

#--------------------
# Problem Definition
#--------------------

# Load source functions 
setwd("/Users/danteschrantz/desktop/UNAV/2024-2025/Optimización 2/")
source("hill.R") 
source("blind.R") 
source("montecarlo.R") 

#--------------------
# Problem Definition
#--------------------
# Objective: Minimize total boarding time for aircraft passengers
# Decision variables: Sequence of passengers (permutation of 1:total_passengers)
# Constraints: 
# 1. Each passenger must be assigned exactly once
# 2. All passengers must be assigned
# Additional penalty for violations of social distancing (optional)

# Parameters
num_rows <- 30
seats_per_row <- 6
total_passengers <- num_rows * seats_per_row

# Luggage probabilities and times
luggage_probs <- c(
  no_bags = 0.15,    # 15% chance of no bags
  one_bag = 0.50,    # 50% chance of one bag
  two_bags = 0.35    # 35% chance of two bags
)

luggage_times <- c(
  no_bags = 0,     # 0 seconds for no bags
  one_bag = 5,     # 5 seconds for one bag
  two_bags = 10    # 15 seconds for two bags
)

# Add social distancing penalty
simulate_boarding <- function(boarding_sequence, 
                              aisle_time = 3,     # Time to move one row
                              seating_time = 15) { # Time to sit down
  
  # This function simulates the aircraft boarding process
  # It tracks:
  # 1. Passenger movement through the aisle
  # 2. Luggage storage time based on number of bags
  # 3. Seating time including delays from blocked seats
  # Returns: Total boarding time in seconds
  
  # Create a matrix to represent the airplane
  plane <- matrix(0, nrow = num_rows, ncol = seats_per_row)
  
  # Track passenger status and activities
  aisle_status <- matrix(0, nrow = num_rows, ncol = 1)  # 0=empty, 1=occupied
  passenger_status <- data.frame(
    passenger = integer(),      # Passenger ID
    row = integer(),           # Target row
    seat = integer(),          # Target seat
    current_row = integer(),   # Current position
    activity = character(),    # Current activity (moving/luggage/seating)
    time_remaining = numeric() # Time remaining for current activity
  )
  
  total_time <- 0
  
  # Pre-generate luggage scenarios for all passengers
  luggage_scenarios <- sample(
    c("no_bags", "one_bag", "two_bags"),
    size = length(boarding_sequence),
    prob = c(luggage_probs["no_bags"], 
             luggage_probs["one_bag"], 
             luggage_probs["two_bags"]),
    replace = TRUE
  )
  
  # Process time steps until all passengers are seated
  active_passengers <- 0
  passenger_index <- 1
  
  while(passenger_index <= length(boarding_sequence) || nrow(passenger_status) > 0) {
    # Add new passenger if possible (no one blocking first row)
    if(passenger_index <= length(boarding_sequence) && aisle_status[1] == 0) {
      passenger <- boarding_sequence[passenger_index]
      row <- ceiling(passenger / seats_per_row)
      seat <- passenger %% seats_per_row
      if(seat == 0) seat <- seats_per_row
      
      passenger_status <- rbind(passenger_status, data.frame(
        passenger = passenger,
        row = row,
        seat = seat,
        current_row = 1,
        activity = "moving",
        time_remaining = aisle_time,
        stringsAsFactors = FALSE
      ))
      
      aisle_status[1] <- 1
      passenger_index <- passenger_index + 1
    }
    
    # Process each active passenger
    if(nrow(passenger_status) > 0) {
      for(i in 1:nrow(passenger_status)) {
        if(passenger_status$time_remaining[i] > 0) {
          passenger_status$time_remaining[i] <- passenger_status$time_remaining[i] - 1
        } else {
          # Transition to next activity
          if(passenger_status$activity[i] == "moving") {
            if(passenger_status$current_row[i] < passenger_status$row[i]) {
              # Move to next row if possible
              next_row <- passenger_status$current_row[i] + 1
              if(aisle_status[next_row] == 0) {
                aisle_status[passenger_status$current_row[i]] <- 0
                passenger_status$current_row[i] <- next_row
                aisle_status[next_row] <- 1
                passenger_status$time_remaining[i] <- aisle_time
              }
            } else {
              # Reached destination row, start luggage handling
              passenger_status$activity[i] <- "luggage"
              passenger_status$time_remaining[i] <- luggage_times[luggage_scenarios[passenger_status$passenger[i]]]
            }
          } else if(passenger_status$activity[i] == "luggage") {
            # Start seating process
            passenger_status$activity[i] <- "seating"
            blocked_seats <- sum(plane[passenger_status$row[i], 1:passenger_status$seat[i]] != 0)
            passenger_status$time_remaining[i] <- seating_time + (blocked_seats * seating_time)
          } else if(passenger_status$activity[i] == "seating") {
            # Passenger is seated
            row <- passenger_status$row[i]
            seat <- passenger_status$seat[i]
            plane[row, seat] <- 1
            aisle_status[row] <- 0
            passenger_status <- passenger_status[-i,]
            break
          }
        }
      }
    }
    
    total_time <- total_time + 1
  }
  
  return(total_time)
}

#--------------------
# Grid Search Implementation
#--------------------
grid_search <- function(D, levels, FUN, type="min") {
  # Create grid points for each dimension
  grid_points <- seq(1, D, length.out = levels)
  
  # Create all combinations
  search <- expand.grid(replicate(D, grid_points, simplify = FALSE))
  
  # Convert to matrix
  search <- as.matrix(search)
  
  # Evaluate all points
  results <- apply(search, 1, FUN)
  
  # Find best solution
  if(type == "min") {
    best_idx <- which.min(results)
  } else {
    best_idx <- which.max(results)
  }
  
  return(list(
    solution = search[best_idx,],
    value = results[best_idx]
  ))
}

#--------------------
# Parameter Tuning
#--------------------
tune_parameters <- function(initial_solution, num_trials = 5) {
  # Parameters to test
  parameter_combinations <- expand.grid(
    temperature = c(500, 1000, 2000),
    population_size = c(30, 50, 100),
    mutation_rate = c(0.05, 0.1, 0.2)
  )
  
  # Store results
  results <- matrix(nrow = nrow(parameter_combinations), ncol = num_trials)
  
  # Test each combination
  for(i in 1:nrow(parameter_combinations)) {
    params <- parameter_combinations[i,]
    
    for(trial in 1:num_trials) {
      # Test SA parameters
      sa_control <- list(
        maxit = MAXFN, 
        temp = params$temperature,
        trace = FALSE
      )
      sa_result <- optim(
        par = initial_solution,
        fn = evaluate_boarding,
        method = "SANN",
        control = sa_control
      )
      results[i, trial] <- -sa_result$value
      
      # Test GA parameters
      ga_result <- rbga(
        stringMin = lower,
        stringMax = upper,
        evalFunc = evaluate_boarding,
        popSize = params$population_size,
        iters = MAXFN/params$population_size,
        mutationChance = params$mutation_rate,
        monitorFunc = NULL
      )
      results[i, trial] <- min(c(results[i, trial], -min(ga_result$evaluations)))
    }
  }
  
  # Find best parameter combination
  mean_results <- rowMeans(results)
  best_idx <- which.min(mean_results)
  
  best_parameters <- list(
    temperature = parameter_combinations$temperature[best_idx],
    population_size = parameter_combinations$population_size[best_idx],
    mutation_rate = parameter_combinations$mutation_rate[best_idx],
    best_value = mean_results[best_idx]
  )
  
  return(best_parameters)
}

#--------------------
# Algorithm Comparison
#--------------------
compare_algorithms <- function(methods = c("hillclimbing", "blind", "montecarlo", "genetic", 
                                           "simulated_annealing", "tabu", "differential_evolution", "pso"), 
                               runs=1, max_iter=3) {
  # Define parameters for each algorithm
  params <- list(
    montecarlo = list(
      num_samples = 5     # Reduced from 20 to 5 samples per iteration
    ),
    genetic = list(
      pop_size = 20,     # Reduced from 100 to 20 individuals
      mutation_rate = 0.1,
      crossover_rate = 0.8,
      tournament_size = 3
    ),
    simulated_annealing = list(
      initial_temp = 1000,
      cooling_rate = 0.70  # Faster cooling for fewer iterations
    ),
    tabu = list(
      tabu_size = 5      # Smaller tabu list for faster convergence
    ),
    differential_evolution = list(
      pop_size = 20,     # Reduced population size
      F = 0.8,
      CR = 0.9
    ),
    pso = list(
      swarm_size = 15,   # Reduced swarm size
      w = 0.7,
      c1 = 1.5,
      c2 = 1.5
    )
  )
  
  # Get all boarding strategies
  strategies <- list(
    front_to_back = order(1:total_passengers),
    back_to_front = order(1:total_passengers, decreasing = TRUE),
    window_middle_aisle = {
      seats <- rep(1:seats_per_row, num_rows)
      rows <- rep(1:num_rows, each = seats_per_row)
      order(seats, rows)
    },
    random = sample(1:total_passengers),
    steffen = {
      seats <- rep(1:seats_per_row, num_rows)
      rows <- rep(1:num_rows, each = seats_per_row)
      # Order: Window seats back-to-front alternating sides, then middle seats, then aisle
      seat_priority <- c(1, 6, 2, 5, 3, 4)
      # Create alternating row priorities for each seat type
      row_priority <- rep(1:num_rows, each = seats_per_row)
      seat_map <- match(seats, seat_priority)
      # Adjust row priority based on seat type
      row_priority <- row_priority + (seat_map - 1) * num_rows
      order(seat_map, -row_priority)
    }
  )
  
  results <- matrix(NA, nrow=runs, ncol=length(methods))
  colnames(results) <- methods
  times <- matrix(NA, nrow=runs, ncol=length(methods))
  colnames(times) <- methods
  best_solutions <- list()
  
  for(run in 1:runs) {
    cat("\nRun", run, "of", runs, "\n")
    # For each method and strategy combination
    for(method in methods) {
      for(strategy_name in names(strategies)) {
        initial_solution <- strategies[[strategy_name]]
        
        start_time <- Sys.time()
        
        tryCatch({
          result <- switch(method,
                           "hillclimbing" = {
                             hill_climbing(
                               initial_solution = initial_solution,
                               fn = simulate_boarding,
                               max_iter = max_iter,
                               report_iter = max(1, max_iter/10)
                             )
                           },
                           "blind" = {
                             blind_search(
                               fn = simulate_boarding,
                               dimensions = total_passengers,
                               max_iter = max_iter
                             )
                           },
                           "montecarlo" = {
                             monte_carlo_search(
                               fn = simulate_boarding,
                               dimensions = total_passengers,
                               max_iter = max_iter,
                               num_samples = params$montecarlo$num_samples
                             )
                           },
                           "genetic" = {
                             genetic_algorithm(
                               fn = simulate_boarding,
                               dimensions = total_passengers,
                               max_gen = max_iter,
                               pop_size = params$genetic$pop_size,
                               mutation_rate = params$genetic$mutation_rate,
                               crossover_rate = params$genetic$crossover_rate,
                               tournament_size = params$genetic$tournament_size
                             )
                           },
                           "simulated_annealing" = {
                             simulated_annealing(
                               initial_solution = initial_solution,
                               fn = simulate_boarding,
                               max_iter = max_iter,
                               initial_temp = params$simulated_annealing$initial_temp,
                               cooling_rate = params$simulated_annealing$cooling_rate
                             )
                           },
                           "tabu" = {
                             tabu_search(
                               initial_solution = initial_solution,
                               fn = simulate_boarding,
                               max_iter = max_iter,
                               tabu_size = params$tabu$tabu_size
                             )
                           },
                           "differential_evolution" = {
                             differential_evolution(
                               fn = simulate_boarding,
                               dimensions = total_passengers,
                               max_gen = max_iter,
                               pop_size = params$differential_evolution$pop_size,
                               F = params$differential_evolution$F,
                               CR = params$differential_evolution$CR
                             )
                           },
                           "pso" = {
                             particle_swarm(
                               fn = simulate_boarding,
                               dimensions = total_passengers,
                               max_iter = max_iter,
                               swarm_size = params$pso$swarm_size,
                               w = params$pso$w,
                               c1 = params$pso$c1,
                               c2 = params$pso$c2
                             )
                           }
          )
          
          # Store results with strategy name
          results[run, method] <- result$value
          best_solutions[[length(best_solutions) + 1]] <- list(
            run = run,
            method = method,
            strategy = strategy_name,
            solution = result$solution,
            value = result$value,
            iterations = result$iterations
          )
          
          cat(method, "-", strategy_name, "result:", result$value, "\n")
          
        }, error = function(e) {
          cat("Error in", method, ":", e$message, "\n")
          results[run, method] <- NA
        })
        
        times[run, method] <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
      }
    }
  }
  
  # Analysis of results
  cat("\n=== Algorithm Comparison Results ===\n")
  for(method in methods) {
    method_results <- results[, method]
    valid_results <- method_results[!is.na(method_results)]
    
    if(length(valid_results) > 0) {
      cat("\n", method, ":\n")
      cat("Mean result:", mean(valid_results), "\n")
      cat("Best result:", min(valid_results), "\n")
      cat("Worst result:", max(valid_results), "\n")
      cat("Std dev:", sd(valid_results), "\n")
      cat("Average time:", mean(times[, method], na.rm=TRUE), "seconds\n")
    }
  }
  
  # Find overall best
  best_idx <- which.min(sapply(best_solutions, function(x) x$value))
  cat("\nOverall Best Solution:\n")
  cat("Method:", best_solutions[[best_idx]]$method, "\n")
  cat("Run:", best_solutions[[best_idx]]$run, "\n")
  cat("Value:", best_solutions[[best_idx]]$value, "\n")
  cat("Iterations:", best_solutions[[best_idx]]$iterations, "\n")
  
  return(list(
    results = results,
    times = times,
    best_solutions = best_solutions,
    parameters = params
  ))
}

# Run comparison with optimal parameters
results <- compare_algorithms(
  methods = c("hillclimbing", "blind", "montecarlo", "genetic", 
              "simulated_annealing", "tabu", "differential_evolution", "pso"),
  runs = 1,            # Single run for each combination
  max_iter = 5         # 5 iterations for balance of speed and quality
)

#--------------------
# Boarding Strategies
#--------------------
analyze_boarding_strategies <- function() {
  # Create different boarding sequences
  strategies <- list(
    front_to_back = order(1:total_passengers),  # Front to back
    back_to_front = order(1:total_passengers, decreasing = TRUE),  # Back to front
    reverse_pyramid = {  # Back-to-front in a pyramid pattern
      rows <- rep(1:num_rows, each = seats_per_row)
      seats <- rep(1:seats_per_row, num_rows)
      # Create pyramid priority
      row_priority <- abs(rows - mean(rows))
      order(row_priority, seats)
    },
    alternating_rows = {  # Alternate between even and odd rows
      rows <- rep(1:num_rows, each = seats_per_row)
      seats <- rep(1:seats_per_row, num_rows)
      row_groups <- rows %% 2  # 0 for even, 1 for odd
      order(row_groups, -rows, seats)
    },
    window_middle_aisle = {  # Window seats first, then middle, then aisle
      seats <- rep(1:seats_per_row, num_rows)
      rows <- rep(1:num_rows, each = seats_per_row)
      order(seats, rows)
    },
    outside_in = {  # Window seats from both sides, then middle, then aisle
      seats <- rep(1:seats_per_row, num_rows)
      rows <- rep(1:num_rows, each = seats_per_row)
      seat_priority <- c(1, 6, 2, 5, 3, 4)  # Window->Window->Middle->Middle->Aisle->Aisle
      seat_map <- match(seats, seat_priority)
      order(seat_map, rows)
    },
    random = sample(1:total_passengers),  # Random boarding
    steffen = {  # Steffen perfect method
      seats <- rep(1:seats_per_row, num_rows)
      rows <- rep(1:num_rows, each = seats_per_row)
      # Order: Window seats back-to-front alternating sides, then middle seats, then aisle
      seat_priority <- c(1, 6, 2, 5, 3, 4)
      # Create alternating row priorities for each seat type
      row_priority <- rep(1:num_rows, each = seats_per_row)
      seat_map <- match(seats, seat_priority)
      # Adjust row priority based on seat type
      row_priority <- row_priority + (seat_map - 1) * num_rows
      order(seat_map, -row_priority)
    },
    block_boarding = {  # Board in blocks of rows
      block_size <- 5
      blocks <- ceiling(seq(1:total_passengers) / (block_size * seats_per_row))
      order(blocks, sample(1:total_passengers))
    }
  )
  
  # Test each strategy
  results <- list()
  for(name in names(strategies)) {
    sequence <- strategies[[name]]
    time <- simulate_boarding(sequence)
    results[[name]] <- list(
      sequence = sequence,
      time = time
    )
    cat(sprintf("%s: %d time units\n", name, time))
  }
  
  # Visualize results
  times <- sapply(results, function(x) x$time)
  barplot(times, 
          main="Boarding Time by Strategy",
          las=2,
          ylab="Time Units")
  
  # Find best strategy
  best_strategy <- names(which.min(times))
  cat("\nBest strategy:", best_strategy, 
      "\nTime:", min(times), "units\n")
  
  return(results)
}

# Run the analysis
boarding_analysis <- analyze_boarding_strategies()

#--------------------
# Results Analysis and Export
#--------------------
generate_results_summary <- function(algorithm_results, boarding_results) {
  # Create data frame for results
  boarding_times <- sapply(boarding_results, function(x) x$time)
  best_boarding_time <- min(boarding_times)
  
  # Get algorithm results
  algo_times <- tapply(
    sapply(algorithm_results$best_solutions, function(x) x$value),
    list(
      sapply(algorithm_results$best_solutions, function(x) x$method),
      sapply(algorithm_results$best_solutions, function(x) x$strategy)
    ),
    min
  )
  best_algo_time <- min(algo_times, na.rm = TRUE)
  
  # Combine all results
  results_df <- data.frame(
    Algorithm = character(),
    Boarding_Method = character(),
    Total_Time_Seconds = numeric(),
    Total_Time_Minutes = numeric(),
    Time_Format = character(),
    Is_Best = character(),
    stringsAsFactors = FALSE
  )
  
  # Add algorithm results
  for(sol in algorithm_results$best_solutions) {
    minutes <- sol$value / 60
    formatted_time <- sprintf("%d:%02d", floor(minutes), round((minutes %% 1) * 60))
    results_df <- rbind(results_df, data.frame(
      Algorithm = sol$method,
      Boarding_Method = sol$strategy,
      Total_Time_Seconds = sol$value,
      Total_Time_Minutes = round(minutes, 2),
      Time_Format = formatted_time,
      Is_Best = ifelse(sol$value == best_algo_time, "Yes", "No"),
      stringsAsFactors = FALSE
    ))
  }
  
  # Add boarding strategy results
  for(method in names(boarding_times)) {
    minutes <- boarding_times[[method]] / 60
    formatted_time <- sprintf("%d:%02d", floor(minutes), round((minutes %% 1) * 60))
    results_df <- rbind(results_df, data.frame(
      Algorithm = "Fixed Strategy",
      Boarding_Method = method,
      Total_Time_Seconds = boarding_times[[method]],
      Total_Time_Minutes = round(minutes, 2),
      Time_Format = formatted_time,
      Is_Best = ifelse(boarding_times[[method]] == best_boarding_time, "Yes", "No"),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by total time in seconds
  results_df <- results_df[order(results_df$Total_Time_Seconds),]
  
  # Write to CSV
  write.csv(results_df, "boarding_results.csv", row.names = FALSE)
  
  # Print summary with minutes
  cat("\nResults Summary (sorted by boarding time):\n")
  cat("----------------------------------------\n")
  for(i in 1:nrow(results_df)) {
    cat(sprintf("%s - %s: %s (%0.2f minutes)%s\n",
                results_df$Algorithm[i],
                results_df$Boarding_Method[i],
                results_df$Time_Format[i],
                results_df$Total_Time_Minutes[i],
                ifelse(results_df$Is_Best[i] == "Yes", " [BEST]", "")))
  }
  
  return(results_df)
}

# Generate and save results
summary_results <- generate_results_summary(results, boarding_analysis)

# Convert time to minutes for all results
summary_results$Total_Time_Minutes <- summary_results$Total_Time_Seconds / 60

# Generate the visualization
comparison_plot <- plot_boarding_times(summary_results)

# Force the plot to display
print(comparison_plot)

# Display summary of results
print(summary_results)

#--------------------
# Analysis Functions
#--------------------
analyze_boarding_patterns <- function(solution) {
  # Convert solution to row/seat format
  rows <- ceiling(solution / seats_per_row)
  seats <- solution %% seats_per_row
  seats[seats == 0] <- seats_per_row
  
  # Analyze patterns
  patterns <- list(
    row_order = cor(1:length(rows), rows),  # Positive = front-to-back, Negative = back-to-front
    seat_order = cor(1:length(seats), seats),  # Pattern in seat assignments
    window_first = mean(which(seats %in% c(1, seats_per_row))) < total_passengers/2,  # TRUE if windows tend to board first
    block_pattern = sd(diff(rows))  # Low = block boarding, High = scattered
  )
  
  # Print analysis
  cat("\nBoarding Pattern Analysis:\n")
  cat("Row ordering:", ifelse(patterns$row_order > 0.3, "Front-to-back", 
                              ifelse(patterns$row_order < -0.3, "Back-to-front", "Mixed")), "\n")
  cat("Window preference:", ifelse(patterns$window_first, "Windows first", "No window preference"), "\n")
  cat("Boarding style:", ifelse(patterns$block_pattern < 2, "Block boarding", 
                                ifelse(patterns$block_pattern < 4, "Semi-structured", "Scattered")), "\n")
  
  return(patterns)
}

#--------------------
# Visualization
#--------------------
plot_boarding_times <- function(results_df) {
  # Prepare data for plotting
  plot_data <- results_df
  
  # Create color palette for algorithms
  colors <- c("Fixed Strategy" = "#E69F00", 
              "hillclimbing" = "#56B4E9",
              "blind" = "#009E73",
              "montecarlo" = "#F0E442",
              "simulated_annealing" = "#0072B2",
              "tabu" = "#D55E00",
              "differential_evolution" = "#CC79A7",
              "pso" = "#999999")
  
  # Create combined method name for sorting
  plot_data$Method_Full <- paste(plot_data$Algorithm, plot_data$Boarding_Method)
  
  # Create the plot
  p <- ggplot(plot_data, 
              aes(x = reorder(Method_Full, -Total_Time_Minutes),
                  y = Total_Time_Minutes,
                  fill = Algorithm)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_fill_manual(values = colors) +
    labs(title = "Aircraft Boarding Time Comparison",
         subtitle = "All Methods Compared",
         x = "Method",
         y = "Time (minutes)",
         fill = "Algorithm") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom") +
    geom_text(aes(label = sprintf("%.1f", Total_Time_Minutes)),
              vjust = -0.5,
              size = 3)
  
  # Add single best time reference line
  best_time <- min(plot_data$Total_Time_Minutes)
  p <- p + geom_hline(yintercept = best_time,
                      linetype = "dashed",
                      color = "red",
                      alpha = 0.5) +
    annotate("text", 
             x = 1, 
             y = best_time, 
             label = sprintf("Best Time: %.1f min", best_time),
             vjust = -0.5,
             color = "red",
             fontface = "bold")
  
  # Save the plot
  ggsave("boarding_times_comparison.png", p, width = 15, height = 8, dpi = 300)
  
  return(p)
}

# Alternative plotting approach
comparison_plot <- plot_boarding_times(summary_results)
print(comparison_plot)

# Or save directly to file and confirm
ggsave("/Users/danteschrantz/desktop/UNAV/2024-2025/Optimización 2/boarding_comparison.png", comparison_plot, width = 15, height = 8, dpi = 300)