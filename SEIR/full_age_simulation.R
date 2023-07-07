library(tidyverse)
library(macpan2)

source("SEIR/clean_cmat.R")

model = Model(ModelFiles("SEIR/full_age"))

mergeagedat <- (agepop80
	%>% ungroup()
	%>% transmute(Variable = paste0("S_",from)
		, newvalue = pop
	)								
	# %>% select(-from)
)

mergecmlong <- (cmlong5
	%>% ungroup()
	%>% transmute(Variable = paste0("c_",age_to,"_",age_from)
		, newvalue = stdvalue
	)
)

# mergecmlong <- data.frame()

mergedat <- bind_rows(mergeagedat,mergecmlong)



pp = ("SEIR/full_age/default.csv"
	%>% read_csv
	%>% left_join(mergedat)
	%>% mutate(Default = ifelse(!is.na(newvalue),newvalue,Default))
	%>% with(setNames(Default, Variable))
)

age <- seq(0,80,by=5)
sname <- c("S","E","I","H","R","D")

state_name <- c()
for(j in age){
	for(i in sname){
		temp <- paste(i,j,sep="_")
		state_name <- c(state_name,temp)
	}
}
state <- pp[state_name]

mod_simulator = model$simulators$tmb(time_steps = 100
  , state = state
	, flow = c(infection_0 = pp[["infection_0"]], progression_0 = pp[["progression_0"]]
					 , hospitalization_0 = pp[["hospitalization_0"]]
					 , discharge_0 = pp[["discharge_0"]], recovery_0 = pp[["recovery_0"]]
					 , deathH_0 = pp[["deathH_0"]], deathI_0 = pp[["deathI_0"]],
					 
					 infection_5 = pp[["infection_5"]], progression_5 = pp[["progression_5"]]
					 , hospitalization_5 = pp[["hospitalization_5"]]
					 , discharge_5 = pp[["discharge_5"]], recovery_5 = pp[["recovery_5"]]
					 , deathH_5 = pp[["deathH_5"]], deathI_5 = pp[["deathI_5"]],
					 
					 infection_10 = pp[["infection_10"]], progression_10 = pp[["progression_10"]]
					 , hospitalization_10 = pp[["hospitalization_10"]]
					 , discharge_10 = pp[["discharge_10"]], recovery_10 = pp[["recovery_10"]]
					 , deathH_10 = pp[["deathH_10"]], deathI_10 = pp[["deathI_10"]],
					 
					 infection_15 = pp[["infection_15"]], progression_15 = pp[["progression_15"]]
					 , hospitalization_15 = pp[["hospitalization_15"]]
					 , discharge_15 = pp[["discharge_15"]], recovery_15 = pp[["recovery_15"]]
					 , deathH_15 = pp[["deathH_15"]], deathI_15 = pp[["deathI_15"]],
					 
					 infection_20 = pp[["infection_20"]], progression_20 = pp[["progression_20"]]
					 , hospitalization_20 = pp[["hospitalization_20"]]
					 , discharge_20 = pp[["discharge_20"]], recovery_20 = pp[["recovery_20"]]
					 , deathH_20 = pp[["deathH_20"]], deathI_20 = pp[["deathI_20"]],
					 
					 infection_25 = pp[["infection_25"]], progression_25 = pp[["progression_25"]]
					 , hospitalization_25 = pp[["hospitalization_25"]]
					 , discharge_25 = pp[["discharge_25"]], recovery_25 = pp[["recovery_25"]]
					 , deathH_25 = pp[["deathH_25"]], deathI_25 = pp[["deathI_25"]],
					 
					 infection_30 = pp[["infection_30"]], progression_30 = pp[["progression_30"]]
					 , hospitalization_30 = pp[["hospitalization_30"]]
					 , discharge_30 = pp[["discharge_30"]], recovery_30 = pp[["recovery_30"]]
					 , deathH_30 = pp[["deathH_30"]], deathI_30 = pp[["deathI_30"]],
					 
					 infection_35 = pp[["infection_35"]], progression_35 = pp[["progression_35"]]
					 , hospitalization_35 = pp[["hospitalization_35"]]
					 , discharge_35 = pp[["discharge_35"]], recovery_35 = pp[["recovery_35"]]
					 , deathH_35 = pp[["deathH_35"]], deathI_35 = pp[["deathI_35"]],
					 
					 infection_40 = pp[["infection_40"]], progression_40 = pp[["progression_40"]]
					 , hospitalization_40 = pp[["hospitalization_40"]]
					 , discharge_40 = pp[["discharge_40"]], recovery_40 = pp[["recovery_40"]]
					 , deathH_40 = pp[["deathH_40"]], deathI_40 = pp[["deathI_40"]],
					 
					 infection_45 = pp[["infection_45"]], progression_45 = pp[["progression_45"]]
					 , hospitalization_45 = pp[["hospitalization_45"]]
					 , discharge_45 = pp[["discharge_45"]], recovery_45 = pp[["recovery_45"]]
					 , deathH_45 = pp[["deathH_45"]], deathI_45 = pp[["deathI_45"]],
					 
					 infection_50 = pp[["infection_50"]], progression_50 = pp[["progression_50"]]
					 , hospitalization_50 = pp[["hospitalization_50"]]
					 , discharge_50 = pp[["discharge_50"]], recovery_50 = pp[["recovery_50"]]
					 , deathH_50 = pp[["deathH_50"]], deathI_50 = pp[["deathI_50"]],
					 
					 infection_55 = pp[["infection_55"]], progression_55 = pp[["progression_55"]]
					 , hospitalization_55 = pp[["hospitalization_55"]]
					 , discharge_55 = pp[["discharge_55"]], recovery_55 = pp[["recovery_55"]]
					 , deathH_55 = pp[["deathH_55"]], deathI_55 = pp[["deathI_55"]],
					 
					 infection_60 = pp[["infection_60"]], progression_60 = pp[["progression_60"]]
					 , hospitalization_60 = pp[["hospitalization_60"]]
					 , discharge_60 = pp[["discharge_60"]], recovery_60 = pp[["recovery_60"]]
					 , deathH_60 = pp[["deathH_60"]], deathI_60 = pp[["deathI_60"]],
					 
					 infection_65 = pp[["infection_65"]], progression_65 = pp[["progression_65"]]
					 , hospitalization_65 = pp[["hospitalization_65"]]
					 , discharge_65 = pp[["discharge_65"]], recovery_65 = pp[["recovery_65"]]
					 , deathH_65 = pp[["deathH_65"]], deathI_65 = pp[["deathI_65"]],
					 
					 infection_70 = pp[["infection_70"]], progression_70 = pp[["progression_70"]]
					 , hospitalization_70 = pp[["hospitalization_70"]]
					 , discharge_70 = pp[["discharge_70"]], recovery_70 = pp[["recovery_70"]]
					 , deathH_70 = pp[["deathH_70"]], deathI_70 = pp[["deathI_70"]],
					 
					 infection_75 = pp[["infection_75"]], progression_75 = pp[["progression_75"]]
					 , hospitalization_75 = pp[["hospitalization_75"]]
					 , discharge_75 = pp[["discharge_75"]], recovery_75 = pp[["recovery_75"]]
					 , deathH_75 = pp[["deathH_75"]], deathI_75 = pp[["deathI_75"]],
					 
					 infection_80 = pp[["infection_80"]], progression_80 = pp[["progression_80"]]
					 , hospitalization_80 = pp[["hospitalization_80"]]
					 , discharge_80 = pp[["discharge_80"]], recovery_80 = pp[["recovery_80"]]
					 , deathH_80 = pp[["deathH_80"]], deathI_80 = pp[["deathI_80"]]
					 )
	, transmission_0 = pp[["transmission_0"]]
  , transmission_5 = pp[["transmission_5"]]
  , transmission_10 = pp[["transmission_10"]]
  , transmission_15 = pp[["transmission_15"]]
  , transmission_20 = pp[["transmission_20"]]
  , transmission_25 = pp[["transmission_25"]]
  , transmission_30 = pp[["transmission_30"]]
  , transmission_35 = pp[["transmission_35"]]
  , transmission_40 = pp[["transmission_40"]]
  , transmission_45 = pp[["transmission_45"]]
  , transmission_50 = pp[["transmission_50"]]
  , transmission_55 = pp[["transmission_55"]]
  , transmission_60 = pp[["transmission_60"]]
  , transmission_65 = pp[["transmission_65"]]
  , transmission_70 = pp[["transmission_70"]]
  , transmission_75 = pp[["transmission_75"]]
  , transmission_80 = pp[["transmission_80"]]
  , c_0_0 = pp["c_0_0"], c_0_5 = pp[["c_0_5"]], c_0_10 = pp[["c_0_10"]], c_0_15 = pp[["c_0_15"]], c_0_20 = pp[["c_0_20"]], c_0_25 = pp[["c_0_25"]]
  , c_0_30 = pp[["c_0_30"]], c_0_35 = pp[["c_0_35"]], c_0_40 = pp[["c_0_40"]], c_0_45 = pp[["c_0_45"]], c_0_50 = pp[["c_0_50"]], c_0_55 = pp[["c_0_55"]]
  , c_0_60 = pp[["c_0_60"]], c_0_65 = pp[["c_0_65"]], c_0_70 = pp[["c_0_70"]], c_0_75 = pp[["c_0_75"]], c_0_80 = pp[["c_0_80"]]
  
  , c_5_0 = pp["c_5_0"], c_5_5 = pp[["c_5_5"]], c_5_10 = pp[["c_5_10"]], c_5_15 = pp[["c_5_15"]], c_5_20 = pp[["c_5_20"]], c_5_25 = pp[["c_5_25"]]
  , c_5_30 = pp[["c_5_30"]], c_5_35 = pp[["c_5_35"]], c_5_40 = pp[["c_5_40"]], c_5_45 = pp[["c_5_45"]], c_5_50 = pp[["c_5_50"]], c_5_55 = pp[["c_5_55"]]
  , c_5_60 = pp[["c_5_60"]], c_5_65 = pp[["c_5_65"]], c_5_70 = pp[["c_5_70"]], c_5_75 = pp[["c_5_75"]], c_5_80 = pp[["c_5_80"]]
  
  , c_10_0 = pp["c_10_0"], c_10_5 = pp[["c_10_5"]], c_10_10 = pp[["c_10_10"]], c_10_15 = pp[["c_10_15"]], c_10_20 = pp[["c_10_20"]], c_10_25 = pp[["c_10_25"]]
  , c_10_30 = pp[["c_10_30"]], c_10_35 = pp[["c_10_35"]], c_10_40 = pp[["c_10_40"]], c_10_45 = pp[["c_10_45"]], c_10_50 = pp[["c_10_50"]], c_10_55 = pp[["c_10_55"]]
  , c_10_60 = pp[["c_10_60"]], c_10_65 = pp[["c_10_65"]], c_10_70 = pp[["c_10_70"]], c_10_75 = pp[["c_10_75"]], c_10_80 = pp[["c_10_80"]]
  
  , c_15_0 = pp["c_15_0"], c_15_5 = pp[["c_15_5"]], c_15_10 = pp[["c_15_10"]], c_15_15 = pp[["c_15_15"]], c_15_20 = pp[["c_15_20"]], c_15_25 = pp[["c_15_25"]]
  , c_15_30 = pp[["c_15_30"]], c_15_35 = pp[["c_15_35"]], c_15_40 = pp[["c_15_40"]], c_15_45 = pp[["c_15_45"]], c_15_50 = pp[["c_15_50"]], c_15_55 = pp[["c_15_55"]]
  , c_15_60 = pp[["c_15_60"]], c_15_65 = pp[["c_15_65"]], c_15_70 = pp[["c_15_70"]], c_15_75 = pp[["c_15_75"]], c_15_80 = pp[["c_15_80"]]
  
  , c_20_0 = pp["c_20_0"], c_20_5 = pp[["c_20_5"]], c_20_10 = pp[["c_20_10"]], c_20_15 = pp[["c_20_15"]], c_20_20 = pp[["c_20_20"]], c_20_25 = pp[["c_20_25"]]
  , c_20_30 = pp[["c_20_30"]], c_20_35 = pp[["c_20_35"]], c_20_40 = pp[["c_20_40"]], c_20_45 = pp[["c_20_45"]], c_20_50 = pp[["c_20_50"]], c_20_55 = pp[["c_20_55"]]
  , c_20_60 = pp[["c_20_60"]], c_20_65 = pp[["c_20_65"]], c_20_70 = pp[["c_20_70"]], c_20_75 = pp[["c_20_75"]], c_20_80 = pp[["c_20_80"]]
  
  , c_25_0 = pp["c_25_0"], c_25_5 = pp[["c_25_5"]], c_25_10 = pp[["c_25_10"]], c_25_15 = pp[["c_25_15"]], c_25_20 = pp[["c_25_20"]], c_25_25 = pp[["c_25_25"]]
  , c_25_30 = pp[["c_25_30"]], c_25_35 = pp[["c_25_35"]], c_25_40 = pp[["c_25_40"]], c_25_45 = pp[["c_25_45"]], c_25_50 = pp[["c_25_50"]], c_25_55 = pp[["c_25_55"]]
  , c_25_60 = pp[["c_25_60"]], c_25_65 = pp[["c_25_65"]], c_25_70 = pp[["c_25_70"]], c_25_75 = pp[["c_25_75"]], c_25_80 = pp[["c_25_80"]]
  
  , c_30_0 = pp["c_30_0"], c_30_5 = pp[["c_30_5"]], c_30_10 = pp[["c_30_10"]], c_30_15 = pp[["c_30_15"]], c_30_20 = pp[["c_30_20"]], c_30_25 = pp[["c_30_25"]]
  , c_30_30 = pp[["c_30_30"]], c_30_35 = pp[["c_30_35"]], c_30_40 = pp[["c_30_40"]], c_30_45 = pp[["c_30_45"]], c_30_50 = pp[["c_30_50"]], c_30_55 = pp[["c_30_55"]]
  , c_30_60 = pp[["c_30_60"]], c_30_65 = pp[["c_30_65"]], c_30_70 = pp[["c_30_70"]], c_30_75 = pp[["c_30_75"]], c_30_80 = pp[["c_30_80"]]
  
  , c_35_0 = pp["c_35_0"], c_35_5 = pp[["c_35_5"]], c_35_10 = pp[["c_35_10"]], c_35_15 = pp[["c_35_15"]], c_35_20 = pp[["c_35_20"]], c_35_25 = pp[["c_35_25"]]
  , c_35_30 = pp[["c_35_30"]], c_35_35 = pp[["c_35_35"]], c_35_40 = pp[["c_35_40"]], c_35_45 = pp[["c_35_45"]], c_35_50 = pp[["c_35_50"]], c_35_55 = pp[["c_35_55"]]
  , c_35_60 = pp[["c_35_60"]], c_35_65 = pp[["c_35_65"]], c_35_70 = pp[["c_35_70"]], c_35_75 = pp[["c_35_75"]], c_35_80 = pp[["c_35_80"]]
  
  , c_40_0 = pp["c_40_0"], c_40_5 = pp[["c_40_5"]], c_40_10 = pp[["c_40_10"]], c_40_15 = pp[["c_40_15"]], c_40_20 = pp[["c_40_20"]], c_40_25 = pp[["c_40_25"]]
  , c_40_30 = pp[["c_40_30"]], c_40_35 = pp[["c_40_35"]], c_40_40 = pp[["c_40_40"]], c_40_45 = pp[["c_40_45"]], c_40_50 = pp[["c_40_50"]], c_40_55 = pp[["c_40_55"]]
  , c_40_60 = pp[["c_40_60"]], c_40_65 = pp[["c_40_65"]], c_40_70 = pp[["c_40_70"]], c_40_75 = pp[["c_40_75"]], c_40_80 = pp[["c_40_80"]]
  
  , c_45_0 = pp["c_45_0"], c_45_5 = pp[["c_45_5"]], c_45_10 = pp[["c_45_10"]], c_45_15 = pp[["c_45_15"]], c_45_20 = pp[["c_45_20"]], c_45_25 = pp[["c_45_25"]]
  , c_45_30 = pp[["c_45_30"]], c_45_35 = pp[["c_45_35"]], c_45_40 = pp[["c_45_40"]], c_45_45 = pp[["c_45_45"]], c_45_50 = pp[["c_45_50"]], c_45_55 = pp[["c_45_55"]]
  , c_45_60 = pp[["c_45_60"]], c_45_65 = pp[["c_45_65"]], c_45_70 = pp[["c_45_70"]], c_45_75 = pp[["c_45_75"]], c_45_80 = pp[["c_45_80"]]
  
  , c_50_0 = pp["c_50_0"], c_50_5 = pp[["c_50_5"]], c_50_10 = pp[["c_50_10"]], c_50_15 = pp[["c_50_15"]], c_50_20 = pp[["c_50_20"]], c_50_25 = pp[["c_50_25"]]
  , c_50_30 = pp[["c_50_30"]], c_50_35 = pp[["c_50_35"]], c_50_40 = pp[["c_50_40"]], c_50_45 = pp[["c_50_45"]], c_50_50 = pp[["c_50_50"]], c_50_55 = pp[["c_50_55"]]
  , c_50_60 = pp[["c_50_60"]], c_50_65 = pp[["c_50_65"]], c_50_70 = pp[["c_50_70"]], c_50_75 = pp[["c_50_75"]], c_50_80 = pp[["c_50_80"]]
  
  , c_55_0 = pp["c_55_0"], c_55_5 = pp[["c_55_5"]], c_55_10 = pp[["c_55_10"]], c_55_15 = pp[["c_55_15"]], c_55_20 = pp[["c_55_20"]], c_55_25 = pp[["c_55_25"]]
  , c_55_30 = pp[["c_55_30"]], c_55_35 = pp[["c_55_35"]], c_55_40 = pp[["c_55_40"]], c_55_45 = pp[["c_55_45"]], c_55_50 = pp[["c_55_50"]], c_55_55 = pp[["c_55_55"]]
  , c_55_60 = pp[["c_55_60"]], c_55_65 = pp[["c_55_65"]], c_55_70 = pp[["c_55_70"]], c_55_75 = pp[["c_55_75"]], c_55_80 = pp[["c_55_80"]]
  
  , c_60_0 = pp["c_60_0"], c_60_5 = pp[["c_60_5"]], c_60_10 = pp[["c_60_10"]], c_60_15 = pp[["c_60_15"]], c_60_20 = pp[["c_60_20"]], c_60_25 = pp[["c_60_25"]]
  , c_60_30 = pp[["c_60_30"]], c_60_35 = pp[["c_60_35"]], c_60_40 = pp[["c_60_40"]], c_60_45 = pp[["c_60_45"]], c_60_50 = pp[["c_60_50"]], c_60_55 = pp[["c_60_55"]]
  , c_60_60 = pp[["c_60_60"]], c_60_65 = pp[["c_60_65"]], c_60_70 = pp[["c_60_70"]], c_60_75 = pp[["c_60_75"]], c_60_80 = pp[["c_60_80"]]
  
  , c_65_0 = pp["c_65_0"], c_65_5 = pp[["c_65_5"]], c_65_10 = pp[["c_65_10"]], c_65_15 = pp[["c_65_15"]], c_65_20 = pp[["c_65_20"]], c_65_25 = pp[["c_65_25"]]
  , c_65_30 = pp[["c_65_30"]], c_65_35 = pp[["c_65_35"]], c_65_40 = pp[["c_65_40"]], c_65_45 = pp[["c_65_45"]], c_65_50 = pp[["c_65_50"]], c_65_55 = pp[["c_65_55"]]
  , c_65_60 = pp[["c_65_60"]], c_65_65 = pp[["c_65_65"]], c_65_70 = pp[["c_65_70"]], c_65_75 = pp[["c_65_75"]], c_65_80 = pp[["c_65_80"]]
  
  , c_70_0 = pp["c_70_0"], c_70_5 = pp[["c_70_5"]], c_70_10 = pp[["c_70_10"]], c_70_15 = pp[["c_70_15"]], c_70_20 = pp[["c_70_20"]], c_70_25 = pp[["c_70_25"]]
  , c_70_30 = pp[["c_70_30"]], c_70_35 = pp[["c_70_35"]], c_70_40 = pp[["c_70_40"]], c_70_45 = pp[["c_70_45"]], c_70_50 = pp[["c_70_50"]], c_70_55 = pp[["c_70_55"]]
  , c_70_60 = pp[["c_70_60"]], c_70_65 = pp[["c_70_65"]], c_70_70 = pp[["c_70_70"]], c_70_75 = pp[["c_70_75"]], c_70_80 = pp[["c_70_80"]]
  
  , c_75_0 = pp["c_75_0"], c_75_5 = pp[["c_75_5"]], c_75_10 = pp[["c_75_10"]], c_75_15 = pp[["c_75_15"]], c_75_20 = pp[["c_75_20"]], c_75_25 = pp[["c_75_25"]]
  , c_75_30 = pp[["c_75_30"]], c_75_35 = pp[["c_75_35"]], c_75_40 = pp[["c_75_40"]], c_75_45 = pp[["c_75_45"]], c_75_50 = pp[["c_75_50"]], c_75_55 = pp[["c_75_55"]]
  , c_75_60 = pp[["c_75_60"]], c_75_65 = pp[["c_75_65"]], c_75_70 = pp[["c_75_70"]], c_75_75 = pp[["c_75_75"]], c_75_80 = pp[["c_75_80"]]
  
  , c_80_0 = pp["c_80_0"], c_80_5 = pp[["c_80_5"]], c_80_10 = pp[["c_80_10"]], c_80_15 = pp[["c_80_15"]], c_80_20 = pp[["c_80_20"]], c_80_25 = pp[["c_80_25"]]
  , c_80_30 = pp[["c_80_30"]], c_80_35 = pp[["c_80_35"]], c_80_40 = pp[["c_80_40"]], c_80_45 = pp[["c_80_45"]], c_80_50 = pp[["c_80_50"]], c_80_55 = pp[["c_80_55"]]
  , c_80_60 = pp[["c_80_60"]], c_80_65 = pp[["c_80_65"]], c_80_70 = pp[["c_80_70"]], c_80_75 = pp[["c_80_75"]], c_80_80 = pp[["c_80_80"]]
  
	, N_0 = sum(as.numeric(pp[c("S_0","E_0","I_0","H_0","R_0","D_0")]))
  , N_5 = sum(as.numeric(pp[c("S_5","E_5","I_5","H_5","R_5","D_5")]))
  , N_10 = sum(as.numeric(pp[c("S_10","E_10","I_10","H_10","R_10","D_10")]))
  , N_15 = sum(as.numeric(pp[c("S_15","E_15","I_15","H_15","R_15","D_15")]))
  , N_20 = sum(as.numeric(pp[c("S_20","E_20","I_20","H_20","R_20","D_20")]))
  , N_25 = sum(as.numeric(pp[c("S_25","E_25","I_25","H_25","R_25","D_25")]))
  , N_30 = sum(as.numeric(pp[c("S_30","E_30","I_30","H_30","R_30","D_30")]))
  , N_35 = sum(as.numeric(pp[c("S_35","E_35","I_35","H_35","R_35","D_35")]))
  , N_40 = sum(as.numeric(pp[c("S_40","E_40","I_40","H_40","R_40","D_40")]))
  , N_45 = sum(as.numeric(pp[c("S_45","E_45","I_45","H_45","R_45","D_45")]))
  , N_50 = sum(as.numeric(pp[c("S_50","E_50","I_50","H_50","R_50","D_50")]))
  , N_55 = sum(as.numeric(pp[c("S_55","E_55","I_55","H_55","R_55","D_55")]))
  , N_60 = sum(as.numeric(pp[c("S_60","E_60","I_60","H_60","R_60","D_60")]))
  , N_65 = sum(as.numeric(pp[c("S_65","E_65","I_65","H_65","R_65","D_65")]))
  , N_70 = sum(as.numeric(pp[c("S_70","E_70","I_70","H_70","R_70","D_70")]))
  , N_75 = sum(as.numeric(pp[c("S_75","E_75","I_75","H_75","R_75","D_75")]))
  , N_80 = sum(as.numeric(pp[c("S_80","E_80","I_80","H_80","R_80","D_80")]))
	, .mats_to_return = c("state", "total_inflow")
	, .dimnames = list(total_inflow = list(names(state), ""))
)

sim_results = mod_simulator$report()
sim_results
head(sim_results)
sim_results <- (sim_results 
	%>% separate(row,c("state","age"),"_")
)

(gg <- (sim_results %>% filter(matrix == "state") %>% filter(state == "I")
%>% ggplot()
	+ geom_line(aes(time, value, colour = age))
	+ labs(colour = "age group")
	# + scale_color_manual(values=c("red","black"))
	+ theme_bw()
	# + theme(legend.position = "none")
	+ ylab("Incidence")
	# + scale_y_log10()
	+ ylim(c(0,1000000))
	+ geom_hline(aes(yintercept=375000))
)
)

gg
ggsave("full_base.png",width=5,height=5)


