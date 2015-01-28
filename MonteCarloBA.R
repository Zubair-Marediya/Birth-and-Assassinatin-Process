# Now you are ready to study the process in greater detail.

exptOne = function(l, k, mG = 10, mT = 5 * l){
# This little helper function calls familyTree with
# the specified values for lambda, kappa, maxGen, and maxTime in
# l, k, mG, and mT, respectively
	
# It returns the summary statistics needed for the analysis,
# i.e., the number of generations and the number of offspring
ft = familyTree(l,k,mG, mT)
num_gen = length(ft) - 1
os_count = 0
for (i in 1:num_gen){
  os_count = os_count + length(ft[[i]]$gen_id)
}

died_out = ft[[length(ft)]]
  
return(c(num_gen, os_count, died_out))	
}

MCBA = function(params, repeats = 100){
# Here you will simulate the stochastic process for different values of lambda and kappa
# And you will run the experiment for each lambda and kappa several timess
	
# params is a matrix with two columns.
# The first column holds lambda values
# The second column holds kappa values
# For each row in the matrix, i.e., each lambda, kappa pair, call exptOne repeats times.
	
# For each parameter setting, summarize the experiments by providing:
# numEnd: number of times the family died out
# OS50: median number of offspring for each family
# OS25: lower quartile of the number of offspring for each family
# OS75: upper quartile of the number of offspring for each family
# Gen50: median number of generations for each family
# Gen25: lower quartile of the number of generations for each family
# Gen75: upper quartile of the number of generations for each family
  
 
 
# This information should be returned in a data frame with one row for each parameter setting
# Use the helper function you wrote to collect the summary statistics from teach family

final_data = matrix(0,nrow = nrow(params), ncol = 7)
for (r in 1:nrow(params)){
  generations_data = replicate(10, exptOne(params[r,1],params[r,2]))
  numEnd = sum(generations_data[3,])
  offsprings = generations_data[2,]
  osquants = unname(quantile(offsprings, probs = c(0.25, 0.50, 0.75)))
  os50 = osquants[2]
  os25 = osquants[1]
  os75 = osquants[3]
  genquants = unname(quantile(generations_data[1,], probs = c(0.25, 0.50, 0.75)))
  Gen50 = genquants[2]
  Gen25 = genquants[1]
  Gen75 = genquants[3]
  
  final_data[r,] = c(numEnd, os50, os25, os75, Gen50, Gen25, Gen75) 
}
colnames(final_data) =  c("numEnd" , "os50" , "os25" , "os75" , "Gen50", "Gen25", "Gen75" )
return(as.data.frame(final_data))
}


lambdas = c(0.5, 0.5, 0.5, 1, 1, 1, 2, 2, 3, 4, 5)
kappas  = c(1, 2, 3, 1, 2, 3, 1, 2, 1, 1, 1)

mcOutput = MCBA(params = matrix(c(lambdas, kappas), ncol = 2, byrow = FALSE))
mcOutput
