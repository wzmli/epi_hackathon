library(tidyverse)
library(macpan2)

model = Model(ModelFiles("SEIR/"))

params = ("SEIR/default.csv"
	%>% read_csv
	%>% with(setNames(Default, Variable))
)


mod_simulator = model$simulators$tmb(time_steps = 100
	, state = c(S = params[["S"]], E = params[["E"]]
							, I = params[["I"]], H = params[["H"]]
							, R = params[["R"]], D = params[["D"]])
	, flow = c(infection = params[["infection"]], progression = params[["progression"]]
					 , hospitalization = params[["hospitalization"]]
					 , discharge = params[["discharge"]], recovery = params[["recovery"]]
					 , deathH = params[["deathH"]], deathI = params[["deathI"]]
					 )
	, transmission = params[["transmission"]]
	, N = sum(as.numeric(params[c("S","E","I","H","R","D")])) # explained below
	# , state = c(S = 998, E = 1, I = 1, H = 0, R = 0)
	# , flow = c(infection = 0.2
	# 					 , progression = 0.5, recovery = 0.2
	# 					 , hospitalization = 0.5, discharge = 0.5
	# )
	# , transmission = 1
	# , N = 1000 # explained below
	
)

sim_results = mod_simulator$report()
head(sim_results)
sim_results

(ggplot(sim_results)
	+ geom_line(aes(time, value, colour = row))
	+ labs(colour = "state")
	+ theme_bw()
)
