##Visualize sensitivity analysis

sens <- read_excel("SensitivityResults.xlsx")
# Heatmap 
library(magma)
library(ggthemes)
library(paletteer)
library(scico)

# Heatmap 
sens$CaseAvert[sens$InSchool == 0 & sens$OutSchool == 0] <- NA
p1 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= CaseAvert)) + 
  geom_tile() + scale_fill_viridis(option = "mako")  + 
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "Cases Averted") + theme(text = element_text(size=17))

sens$DeathAvert[sens$InSchool == 0 & sens$OutSchool == 0] <- NA
p2 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= DeathAvert)) + 
  geom_tile() + scale_fill_viridis(option = "mako")  +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "Deaths Averted") + theme(text = element_text(size=17))

sens$CHEAvert[sens$InSchool == 0 & sens$OutSchool == 0] <- NA
p3 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= CHEAvert)) + 
  geom_tile() + scale_fill_viridis(option = "mako")  +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "CHE Averted") + theme(text = element_text(size=17))

#sens$CaseEquity[sens$InSchool == 0 & sens$OutSchool == 0] <- NA
sens$CaseEquity_new <- 1/sens$CaseEquity
p4 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= CaseEquity_new)) + 
  geom_tile() + scale_fill_viridis(option = "mako")  +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "Cases Averted: \nQ1/Q5") + theme(text = element_text(size=17))

sens$CHEEquity_new <- 1/sens$CHEEquity
p5 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= CHEEquity_new)) + 
  geom_tile() + scale_fill_viridis(option = "mako")  +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "CHE Averted: \nQ1/Q5") + theme(text = element_text(size=17))

sens$Cost[sens$InSchool == 0 & sens$OutSchool == 0] <- NA
p6 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= Cost)) + 
  geom_tile() + scale_fill_viridis(option = "mako")  +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "Vaccination \nProgram Cost") + theme(text = element_text(size=17))

p7 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= CostperCase)) + 
  geom_tile() + scale_fill_viridis(option = "mako") +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "Cost per \nCase Averted") + theme(text = element_text(size=17))

p8 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= CostperDeath)) + 
  geom_tile() + scale_fill_viridis(option = "mako")  +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "Cost per \nDeath Averted") + theme(text = element_text(size=17))

p9 <- ggplot(sens, aes(as.character(InSchool), as.character(OutSchool), fill= CostperCHE)) + 
  geom_tile() +
  xlab("In-School Girls Vaccinated") + 
  ylab("Out-of-School Girls Vaccinated") + 
  labs(fill = "Cost per \nCHE Averted") + 
  scale_fill_viridis(option = "mako") + theme(text = element_text(size=17))

jpeg(file="Figure3.jpg", width=1300, height=1000)
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3, widths = c(1,1,1))
dev.off()






