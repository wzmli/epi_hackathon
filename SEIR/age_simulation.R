library(tidyverse)
library(macpan2)

model = Model(ModelFiles("SEIR/age"))

params = ("SEIR/age/default.csv"
	%>% read_csv
	%>% with(setNames(Default, Variable))
)

state_name <- c("S_y","E_y","I_y","H_y","R_y","D_y"
	, "S_o","E_o","I_o","H_o","R_o","D_o")
state <- params[state_name]

mod_simulator = model$simulators$tmb(time_steps = 100
  , state = state
	, flow = c(infection_y = params[["infection_y"]], progression = params[["progression_y"]]
					 , hospitalization = params[["hospitalization_y"]]
					 , discharge_y = params[["discharge_y"]], recovery = params[["recovery_y"]]
					 , deathH_y = params[["deathH_y"]], deathI_y = params[["deathI_y"]],
					 infection_o = params[["infection_o"]], progression_o = params[["progression_o"]]
					 , hospitalization_o = params[["hospitalization_o"]]
					 , discharge_o = params[["discharge_o"]], recovery_y = params[["recovery_o"]]
					 , deathH_o = params[["deathH_o"]], deathI_o = params[["deathI_o"]]
					 )
	, transmission_y = params[["transmission_y"]]
  , transmission_o = params[["transmission_o"]]
	, N_y = sum(as.numeric(params[c("S_y","E_y","I_y","H_y","R_y","D_y")]))
  , N_o = sum(as.numeric(params[c("S_o","E_o","I_o","H_o","R_o","D_o")]))
	, .mats_to_return = c("state", "total_inflow")
	, .dimnames = list(total_inflow = list(names(state), ""))
)

sim_results = mod_simulator$report()
head(sim_results)
sim_results <- (sim_results 
	%>% mutate(state = gsub("_y","",row)
				, state = gsub("_o","",state)
				, age = ifelse(grepl("_y",row),"young","old")
	)
)

(sim_results %>% filter(matrix == "state") %>% filter(state == "I")
%>% ggplot()
	+ geom_line(aes(time, value, colour = age))
	+ labs(colour = "state")
	+ theme_bw()
)

