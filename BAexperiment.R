# In this file, you will write your code to perform one experiment, 
# i.e. simulate a birth and assassination process

# You are provided the shell for several functions.
# Do Not change the function names or definitions

genKids = function(id, bTime, aTime, lambda = 1, kappa = 1) {
# This function generates the birth times and assassination times
# for the children.
# The function is vectorized in that it takes a vector of ids, a vector of bTimes, etc.
# id is the id number of the parent
# bTime is a vector of birth times for the parents
# aTime is a vector of assassination times for the parents
# This is done in a three step proces
  
# STEP 1. Determine how many children he has
# The number of children is a Poisson random count
# The rate of the Possion is lambda * the age
  
if (length(id) > 0){
    
  child_lambda = lambda * (aTime - bTime)
  child_count = rep(0, length(id))
  for (i in 1:length(id)){
    child_count[i] = rpois(1, child_lambda[i])
  }
  
  
  # STEP 2. Determine the birth times of the children
  # The children are born at uniform times between the parents birth
  # and assassination.
  # Be careful to take care of the case when a parent has no children.
  
  child_births = rep(list(NA),length(id))
  for (i in 1:length(id)){
    if (child_count[i] != 0){
      child_births[i] = list(runif(child_count[i], min = bTime[i], max = aTime[i]))
    }
  }
  
  # STEP 3. Determine the death times of the children.
  # The children are assassinated after the parent is assassinated.
  # The amount of time after the parent is assassinated follows an
  # exponential distribution with rate kappa.
  
  child_deaths = rep(list(NA), length(id))
  for (i in 1:length(id)){
    if (child_count[i] != 0){
      child_deaths[i] = list(aTime[i] + rexp(child_count[i], kappa))
    }
  }
  
  # Return a data frame with four columns called parentID, id, birth, death
  # This data frame will have one row for each child
  # If a parent had no children, there will be no rows in the data frame for his children
  # The id is simply a number from 1 to the total number of children born in this generation
  
  parentID = integer(0)
  for (i in 1:length(id)){
    current_parentID = rep(id[i], child_count[i])
    parentID = c(parentID, current_parentID)
  }
  
  if (sum(child_count) == 0) {
    
    gen_id = integer(0)
    } else {
    gen_id = 1:sum(child_count)
  }
  
  birth = integer(0)
  for (i in 1:length(id)){
    birth = c(birth, sort(child_births[[i]]))
  }
  
  death = integer(0)
  for (i in 1:length(id)){
      death = c(death, na.omit(child_deaths[[i]]))
  }
  return(data.frame(parentID, gen_id, birth, death))
   

}
}

		   
familyTree = function(lambda = 1, kappa = 1, maxGen = 10, maxTime = 5*lambda) {
# In this function you will create the generations
# lambda is the birth rate for each person
# kappa is the death rate for each person
# maxGen is the maximum number of generations that you generate
# maxTime is the maximum length of time that you observe the family

# The return value from this function is a list with one element for each generation.
# Create this list, and call it allGens
	allGens = list()
		   
# Each element in allGens is a data frame. 
# See the function genKids for a description of this data frame
	
# At the root of the family tree is one individual, born at time 0 and dies at a 
# time determined by the exponential distribution with rate kappa
# Create the data frame for this first generation
# It will have one row.  Use 0 for the parentID


parentID = c(0)
gen_id = c(1)
birth = c(0)
death = rexp(1)

allGens = c(allGens,list(data.frame(parentID, gen_id, birth, death)))

# Generate the future generations, one at a time.
# Use genKids() to do this.

indicator = FALSE
f = genKids(parentID, birth, death)

if (maxGen > 1) {
  
  gencount = 1
  while(gencount < maxGen){
    if (any(f$birth > maxTime)) {
      ##print(c("BEGINNING OF CASE 1 REACHED:",gencount))
      locs = which(f$birth > maxTime)
      f = f[-locs,]
        
        if (length(f$gen_id) == 0) {
          ##print(c("BEGINNING OF CASE 1 SUBCASE 1 REACHED", gencount))
          
          gencount = maxGen
          ##print(c("END OF CASE 1 SUBCASE 1 REACHED", gencount))
        } else {
          ##print(c("BEGINNING OF CASE 1 SUBCASE 2 REACHED", gencount))
          allGens = c(allGens, list(f))
          f = genKids(f$gen_id, f$birth, f$death)
          gencount = gencount + 1
          ##print(c("END OF CASE 1 SUBCASE 2 REACHED", gencount))
        }
      #print(c("END OF CASE 1 REACHED:", gencount))
    } else {
      #print(c("BEGINNING OF CASE 2 REACHED:",gencount))
        if (length(f$gen_id) == 0) {
          indicator = TRUE
          gencount = maxGen
        } else {
        allGens = c(allGens, list(f))
        
        f = genKids(f$gen_id, f$birth, f$death)
        gencount = gencount + 1
        #print(c("END OF CASE 2 REACHED:",gencount))
        }
      
    }
  }
}


# The simulation stops when 
# 1. you have created children in the maxGen generation OR
# 2. the birth dates all exceed the maxTime time OR
# 3. no offspring have been produced in a generation.
	allGens = c(allGens,list(indicator))
	return(allGens)
}		   

### Extra Credit
### PROFILE genKids and familyTree
### Identify where your code is taking most of the time
### Improve the efficiency of your code


test.Gen = function(){
# This function is provided to you to check your code
# With the seed set at 222222222,
# You should produce a family tree that looks as follows

#	[[1]]
#	parentID id birth    death
#	1        0  1     0 1.465804
	
#	[[2]]
#	parentID id     birth    death
#	1        1  1 0.3507661 2.483865
	
#	[[3]]
#	parentID id     birth    death
#	1        1  1 0.5760519 2.755471
#	2        1  2 1.9161281 5.377054
	
#	[[4]]
#	parentID id    birth    death
#	1        1  1 1.726824 2.968156
#	2        1  2 2.438052 3.308342
#	3        1  3 2.723489 3.720874
#	4        2  4 2.566992 6.202776
#	5        2  5 3.029503 6.735076
	
#	[[5]]
#	parentID id    birth     death
#	1        4  1 3.909691 12.416104
#	2        4  2 4.211997  6.840689
#	6        5  6 4.810271  8.878621
#	7        5  7 4.925031  6.917579
	
#	[[6]]
#	parentID id    birth   death
#	1        1  1 4.058793 12.8465
#	2        1  2 4.737611 12.5524
	
#	[[7]]
#	parentID id    birth    death
#	1        1  1 4.837545 14.37369
	
#	[[8]]
#	parentID id   birth    death
#	1        1  1 4.84967 16.05662
	
 set.seed(222222222)
 xx = familyTree()
	if (length(xx) == 8) {
		cat("Correct number of generations\n")
	} else cat("Incorrect number of generations\n")
 if (xx[[1]]$death > 1.4658 & xx[[1]]$death < 1.46581) {
    cat("Gen 1 assassinated at correct time\n")
 } else cat("Incorrect assassination time for first person\n")
 if (all(sapply(xx, nrow) == c(1,1,2,5,4,2,1,1))) {
	cat("Correct number of offspring in each generation\n")
  } else cat("Incorrect number of offspring in each generation\n")
  if (xx[[8]]$death > 16.0566 & xx[[8]]$death < 16.05663) {
		cat("Gen 8 assassinated at correct time\n")
	} else cat("Incorrect assassination time for last person\n")
}


