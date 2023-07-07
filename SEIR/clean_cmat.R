## clean contact matrix and pop

library(tidyverse)
library(macpan2)

cm <- read_csv("https://raw.githubusercontent.com/mobs-lab/mixing-patterns/main/data/contact_matrices/Canada_country_level_M_overall_contact_matrix_85.csv", col_names = FALSE)
cmlong <- (cm
	%>% mutate(age_from=as.numeric(rownames(.)))	
	%>% pivot_longer(names_to="age_to", values_to="value",-age_from)
	%>% mutate(age_to = as.numeric(gsub("X",replacement = "",age_to)))
	%>% group_by(age_to)
	%>% mutate(stdvalue = value/sum(value))
	%>% ungroup()
)

gg <- (ggplot(cmlong,aes(x=age_from,y=age_to,fill=stdvalue))
	+ geom_tile()
	+ scale_fill_gradient2(low = "black"
			, mid = "orange"
			, high = "yellow"
			, na.value = NA
			, midpoint = 0.1
	)
	+ theme_bw()
	+ theme(legend.position = "bottom")
	+ xlim(c(1,80))
	+ ylim(c(1,80))
)

gg
ggsave("cmat_full.png",width=4,height=5)

cmlong5 <- data.frame()
bin_5 <- seq(0,80,by=5)
for(i in bin_5){
	for(j in bin_5){
		tempdat <- (cmlong
			%>% filter(between(age_from,i,i+4.9))
			%>% filter(between(age_to,j,j+4.9))
			%>% transmute(age_to=i
					, age_from = j
					, value = mean(value,na.rm=TRUE))
			)
		cmlong5 <- bind_rows(cmlong5,tempdat)
	}
}

cmlong5 <- (cmlong5
	%>% distinct()
	%>% group_by(age_to)
	%>% mutate(stdvalue = value/sum(value))
)

(cmat_5bin <- gg %+% cmlong5
	+ scale_fill_gradient2(low = "black"
												, mid = "orange"
												, high = "yellow"
												, na.value = NA
												, midpoint = 0.4
	)
)

cmat_5bin

ggsave("cmat_5bin.png",width=4,height=5)


## 85 is problematic, so lets remove it


agepop <- read_csv("SEIR/full_age/age_pop.csv")
agepop <- (agepop
	%>% mutate(age = gsub("years","",age)
		, age = gsub(" ","",age)
	)					 
	%>% separate(age,c("from","to"),sep="to")
	%>% mutate(from = as.numeric(from)
						 , to = as.numeric(to)
	)
)
agepop80 <- (agepop
	%>% mutate(from = ifelse(from>80,80,from)
				, to = ifelse(to>80,80,to))
	%>% group_by(from)
	%>% mutate(pop = sum(pop))
	%>% distinct()
)

