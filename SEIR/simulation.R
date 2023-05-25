library(macpan2)

model = Model(ModelFiles("SEIR/"))

mod_simulator = model$simulators$tmb(
	time_steps = 100,
	state = c(S = 9998, E=1, I = 1, R = 0),
	flow = c(foi = 0.1, alpha=0.2,gamma = 0.1),
	beta = 1,
	N = 10000 # explained below
)

sim_results = mod_simulator$report()
head(sim_results)
sim_results

(ggplot(sim_results)
	+ geom_line(aes(time, value, colour = row))
	+ labs(colour = "state")
	+ theme_bw()
)
