#### Visualize ####

f_df %>% 
  filter(sim_pop_beta == 7) %>% 
  filter(sim_pop_beta_sd == .1) %>% 
ggplot(aes(x= sim_pop_mu,
           y = mod_pop_mu, 
           group = sim_pop_mu_sd,
           color = as.factor(sim_pop_mu_sd)
           ),alpha =.3)+
  geom_point() +
  geom_line() +
  #facet_wrap(~sim_pop_mu_sd)+
  xlab("Set Mu") +
  ylab("Recovered Mu")+
  scale_color_manual("Set Mu sd",
                     values = c("darkred","steelblue","orange"))

write_csv(f_df,"extract_1.csv")

