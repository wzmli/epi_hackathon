source("SEIR/full_age_simulation.R")

trans_times <- c(0,30,50)
transval_75 <- pp[["transmission_75"]]
transfactors_75 <- c(0,0,0)
# transfactors_75 <- c(1,1,1)

transvec_75 <- transval_75*transfactors_75

mod_simulator$add$matrices(transmission_75_changepoints = trans_times)

mod_simulator$add$matrices(transmission_75_values = transvec_75)

mod_simulator$add$matrices(transmission_75_pointer = 0)

mod_simulator$insert$expressions(
	transmission_75_pointer ~ time_group(transmission_75_pointer, transmission_75_changepoints)
	, .phase = "during"
)

mod_simulator$insert$expressions(
	transmission_75 ~ transmission_75_values[transmission_75_pointer]
	, .phase = "during"
)

tvsims <- mod_simulator$report(.phases = "during")

tvsims <- (tvsims 
	%>% separate(row,c("state","age"),"_")
)


(gg <- (tvsims %>% filter(matrix == "state") %>% filter(state == "I")
	%>% ggplot()
				+ geom_line(aes(time, value, colour = age))
				+ labs(colour = "age")
				# + scale_color_manual(values=c("red","black"))
				+ theme_bw()
				# + theme(legend.position = "none")
				+ ylab("Incidence")
	+ geom_vline(
		aes(xintercept = x),
		linetype = "dashed",
		alpha = 0.5,
		data = data.frame(x = mod_simulator$get$initial("transmission_75_changepoints"))
	)
	+ ylim(c(0,1000000))
	+ geom_hline(aes(yintercept=3750000))
	
	
)
)

gg
ggsave("full_tv.png",width=5,height=5)
