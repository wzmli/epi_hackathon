source("SEIR/simulation.R")

trans_times <- c(0,50,60)
transval <- params[["transmission"]]
transfactors <- c(1,0.5,0.8)
transvec <- transval*transfactors

mod_simulator$add$matrices(transmission_changepoints = trans_times)
mod_simulator$add$matrices(transmission_values = transvec)
mod_simulator$add$matrices(transmission_pointer = 0)

mod_simulator$insert$expressions(
	transmission_pointer ~ time_group(transmission_pointer, transmission_changepoints),
	.phase = "during"
)

mod_simulator$insert$expressions(
	transmission ~ transmission_values[transmission_pointer],
	.phase = "during"
)

tvsims <- mod_simulator$report(.phases = "during")
(tvsims %>% filter(matrix == "total_inflow") %>% filter(row == "I")
	%>% ggplot()
	+ geom_line(aes(time, value, colour = row))
	+ geom_vline(
		aes(xintercept = x),
		linetype = "dashed",
		alpha = 0.5,
		data = data.frame(x = mod_simulator$get$initial("transmission_changepoints"))
	)
)
