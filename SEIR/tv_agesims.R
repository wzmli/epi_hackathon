source("SEIR/age_simulation.R")

trans_times <- c(0,40,50)
transval_o <- params[["transmission_o"]]
transfactors_o <- c(1,0.5,0.3)
# transfactors_o <- c(1,1,1)

transvec_o <- transval_o*transfactors_o

transval_y <- params[["transmission_y"]]
transfactors_y <- c(1,0.5,0.3)
# transfactors_y <- c(1,1,1)

transvec_y <- transval_y*transfactors_y

mod_simulator$add$matrices(transmission_o_changepoints = trans_times)
mod_simulator$add$matrices(transmission_y_changepoints = trans_times)

mod_simulator$add$matrices(transmission_y_values = transvec_y)
mod_simulator$add$matrices(transmission_o_values = transvec_o)

mod_simulator$add$matrices(transmission_y_pointer = 0)
mod_simulator$add$matrices(transmission_o_pointer = 0)

mod_simulator$insert$expressions(
	transmission_y_pointer ~ time_group(transmission_y_pointer, transmission_y_changepoints)
	, transmission_o_pointer ~ time_group(transmission_o_pointer, transmission_o_changepoints)
	, .phase = "during"
)

mod_simulator$insert$expressions(
	transmission_y ~ transmission_y_values[transmission_y_pointer]
	, transmission_o ~ transmission_o_values[transmission_o_pointer]
	, .phase = "during"
)

tvsims <- mod_simulator$report(.phases = "during")

tvsims <- (tvsims 
	%>% mutate(state = gsub("_y","",row)
		, state = gsub("_o","",state)
		, age = ifelse(grepl("_y",row),"young","old")
		)
)


(gg <- (tvsims %>% filter(matrix == "total_inflow") %>% filter(state == "I")
	%>% ggplot()
				+ geom_line(aes(time, value, colour = age))
				+ labs(colour = "state")
				+ scale_color_manual(values=c("red","black"))
				+ theme_bw()
				+ theme(legend.position = "none")
				+ ylab("Incidence")
	+ ylim(c(0,2500000))
	
	+ geom_vline(
		aes(xintercept = x),
		linetype = "dashed",
		alpha = 0.5,
		data = data.frame(x = mod_simulator$get$initial("transmission_y_changepoints"))
	)
)
)

gg
ggsave("tv.png",width=5,height=3)
