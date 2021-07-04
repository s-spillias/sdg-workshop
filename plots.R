## Make Plots

names(new_data) <- c(#"model_sT_aS",
                     "model_SDG_aS",
                    # "model_T_aS",
                     #"model_ET_aS",
                     "model_EsT_aS", 
                     "model_ALLT_aS")

f.plot <- function(i){
  num_obs = df_2 %>% filter(!!as.symbol(names(new_data[[i]])[1]) %in% pull(unique(new_data[[i]][names(new_data[[i]])[1]]))) %>% 
    select(!!as.symbol(names(new_data[[i]])[1]), Polarity, Group, Pathway) %>% 
    distinct() %>% 
    group_by(!!as.symbol(names(new_data[[i]])[1]), Polarity) %>% 
    tally %>% 
    mutate(total = sum(n)) %>% 
    arrange(n)
  
  ggplot() +
    geom_pointinterval(data = new_data[[i]] %>% filter(Group %in% #if(i<3){
                                                         "All"
                                                       #}else{c("Academia", "Practitioners", "Government","All")}
                                                       ),
                       aes(x = .value, 
                           y = reorder(get(names(new_data[[i]])[1]),.value), 
                           col = Group, 
                           alpha = .6, 
                           xmin = .lower, 
                           xmax = .upper, 
                           size = 3),
                       position = position_dodge(width = .5)) +
    geom_bar(data = num_obs,
             aes(y = get(names(new_data[[i]])[1]), 
                 x = -as.numeric(n)*max(new_data[[i]]$.upper)/max(num_obs$total), 
                 fill = Polarity), 
             stat = 'identity') +
    scale_fill_manual(values = SDGpalette[18:20], drop = FALSE, 
                      labels = case_when(i == 3 ~ c("Strengths/Opportunities","Unknown", "Weaknesses/Threats"),
                                         i == 2 ~ c("Opportunities","Unknown", "Threats"),
                                         i == 1 ~ c("Strengths","Unknown", "Weaknesses"))
                                         
     #                 breaks = c("Positive", "Uncertain", "Negative")
                      ) +
    scale_size_continuous(guide = FALSE) +
    scale_color_manual(values = c(brewer.pal(3, "Dark2"),'black'), breaks = c("Academia", "Practitioners", "Government", 
                                                                              "All")  ) +
    coord_cartesian(expand = FALSE) +
    guides(col = FALSE#guide_legend(order = 2)
             ,fill = guide_legend(order = 1), alpha = FALSE) +
    theme_pubr() +
    theme(legend.position="right",
          plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(breaks = c(-0.6, -0.3, 0, 0.3, 0.6), labels = c("50", "25", "0", "0.3", "0.6" )) +
    ggtitle(case_when(i == 1 ~ "",
                      i == 2 ~ "",
                      i == 3 ~ ""#"Importance of Themes"
                      )) +
    xlab("Frequency of Mentions | Probability of High Importance Rating") +
    ylab(NULL) +
    expand_limits(x = 0) +
    labs(y = names(new_data[[i]])[1])
}

for (i in 1:length(models)){
  (f.plot(i) + ggsave(paste0("Figures/output_",i,".png"), dpi=600, device = "png", width = 24, height = 16, units = "cm")  ) }

diag.plot <- function(x) {
  mcmc_trace(models[[x]]) +
    theme(plot.title = element_text(size=4)) +
    ggsave(paste0("Figures/trace_",x,".png"), width = 24, height = 16, units = "cm")}
lapply(1:3, function(x) {diag.plot(x)})

bayesplot::mcmc_acf(models[[1]])
acf.plot <- function(x) {
  mcmc_acf(models[[x]]) +
    theme(plot.title = element_text(size=4),
          panel.spacing = unit(0.01, "lines"),
          strip.text.x = element_text(angle = 90,
                                      hjust = 0,
                                      size = 6),
          axis.text.x = element_blank()) +
    ggsave(paste0("Figures/acf_",x,".png"), width = 24, height = 16, units = "cm")}
lapply(1:3, function(x) {acf.plot(x)})

hist.plot <- function(x) {
  mcmc_hist(models[[x]]) +
    theme(plot.title = element_text(size=4)) +
    ggsave(paste0("Figures/hist_",x,".png"), width = 24, height = 16, units = "cm")}
lapply(1:3, function(x) {hist.plot(x)})
