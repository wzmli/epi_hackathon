library(tidyverse)
library(macpan2)

model = Model(ModelFiles("SEIR/"))

params = ("SEIR/default.csv"
	%>% read_csv
	%>% with(setNames(Default, Variable))
)

state_name <- c("S","E","I","H","R","D")
state <- params[state_name]

mod_simulator = model$simulators$tmb(time_steps = 100
  , state = state
	, flow = c(infection = params[["infection"]], progression = params[["progression"]]
					 , hospitalization = params[["hospitalization"]]
					 , discharge = params[["discharge"]], recovery = params[["recovery"]]
					 , deathH = params[["deathH"]], deathI = params[["deathI"]]
					 )
	, transmission = params[["transmission"]]
	, N = sum(as.numeric(params[c("S","E","I","H","R","D")])) # explained below
	, .mats_to_return = c("state", "total_inflow")
	, .dimnames = list(total_inflow = list(names(state), ""))
)

sim_results = mod_simulator$report()
head(sim_results)
sim_results

(sim_results %>% filter(matrix == "total_inflow") %>% filter(row == "I")
%>% ggplot()
	+ geom_line(aes(time, value, colour = row))
	+ labs(colour = "state")
	+ theme_bw()
)


