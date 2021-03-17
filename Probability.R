# Emran Sabbagh Probability assignment

# delete all
rm(list= ls())

forename = "Emran"
surname = "Sabbagh"
studentID = 20088634
studentProgramme = "Computer forensics and security"



q1 = function(replications = 50000)
{

MTBC = 214
CRIME = 8

# (a) probability

# probability = ppois(7,6.72,F)
calcProb = 0.3596447

results  = c()


# (b) simulation

for(rep in 1:20)
{
	simulation = rpois(replications,6.72)
	# print(length(simulation[simulation > 7]) / replications)
	results = c(results, length(simulation[simulation > 7]) / replications)
}
simProb =mean(results)
sd = sd(results)
c(calcProb, simProb, sd)
}




















/////////////

q2 = function(replications = 37000)
{
AVG = 2428
SD = 238
LOWER = 2145
UPPER = 2596


# (a) Probability
results  = c()

# probability = pnorm(2596,2428,238) - pnorm(2145,2428,238) = 0.6426644
calcProb = 0.6426644

# (b) simulation

for(rep in 1:20)
{
	simulation = rnorm(replications,2428,238)
	#print(length(simulation[simulation < 2596 & simulation > 2145]) / replications)
	results = c(results, length(simulation[simulation < UPPER & simulation > LOWER]) / replications)
}
simProb = mean(results)
sd = sd(results)
c(calcProb, simProb, sd)
}

///////////////



















q3 = function(replications = 25000)
{
# (a) Probability

# probability = choose(11,3)*choose(20,5)/choose(31,8)
calcProb = 0.3242805

results  = c()
choices = c("Tech", "Tech", "Tech", "Tech", "Tech", "Tech", "Tech", "Tech", "Tech", "Tech", "Tech", "Aca", "Aca", "Aca", "Aca", "Aca", "Aca", "Aca", "Aca", "Aca", "Aca", "Aca", "Aca", "Pos", "Pos", "Pos", "Pos", "Pos", "Pos", "Pos", "Pos")
# This is a list of choices to pick from:(11 Tech, 12 Aca, 8 Post)

COMMSIZE = 8
TECH = 11
ACAD = 12
POST = 8
MINITECH = 3

# (b) simulation

for(rep in 1:20)
{
	matches = 0	
	for(rep in 1:replications)
	{
		simulation = sample(choices, 8, replace = F)
		#print(length(simulation[simulation == "Tech"]) / replications)
		#print(length(simulation[simulation == "Tech"]))
	
	if(length(simulation[simulation == "Tech"]) == 3)
	{
		matches = matches + 1
	}

}

	results = c(results, matches / replications)
}

simProb = mean(results)
sd = sd(results)
c(calcProb, simProb, sd)
}















q4 = function(replications = 30525)
{

# (a) Probability

results  = c()
# probability = (10/10 * 9/10 * 2/10) + (10/10 * 1/10 * 9/10) + (10/10 * 1/10 * 1/10)
calcProb = 0.28

DRAWN = 3
TOTAL = 10

# (b) simulation

choices = c(1,2,3,4,5,6,7,8,9,10)

for(rep in 1:20)
{
	matches = 0	
	for(rep in 1:replications)
	{
		simulation = sample(choices, 3, replace = T)
		bool = duplicated(simulation)
	

		if(length(bool[bool==T]) == 1 | length(bool[bool==T]) == 2)
		{
			matches = matches + 1
		}
	

}

	results = c(results, matches / replications)
}

simProb = mean(results)
sd = sd(results)
c(calcProb, simProb, sd)
}














q5 = function(replications = 20000)
{

# IMPORTANT NOTE: THIS FUNCTION WILL TAKE ABOUT 30 MINUTES TO PRODUCE AN OUTPUT

results  = c()
MEMBERS = 110
WINNERS = 2

# (a) Probability
# probability = 1 - choose(110,0)*((1/120)^0)*((119/120)^110) - (choose(110,1)*((1/120)^1)*((119/120)^109))
calcProb = 0.2335

# (b) simulation
choices = c(1,2,3,4,5,6,7,8,9,10)
for(rep1 in 1:20)
{
	# balls chosen in the draw
	draw = sample(choices, 3, replace = F)
	two_winners_in_week = 0	

	for(rep2 in 1:replications)
	{
		
		winners_in_week = 0

		
			
		for(mem in 1:110)
		{

			# choice of the player
			simulation = sample(choices, 3, replace = F)
			
			if(length(setdiff(draw, simulation)) == 0)
			{
				winners_in_week = winners_in_week + 1
			}

		}

		if(winners_in_week >= 2)
		{
			two_winners_in_week = two_winners_in_week + 1
		}
	}
	results = c(results, two_winners_in_week / replications)
}
simProb = mean(results)
sd = sd(results)
c(calcProb, simProb, sd)
}

