#Utilities

 
#plot prediction results.  each plot will be dumped to a pdf 
#Arguments:
# passed_results: dataframe from calling singlearity to get prediction results
# plot_types: vector different types of expected results (e.g. c('woba', 'ba', 'hr'))
# state: Singlearity "State" object.  Used to print text on graph header
# atmosphere: Singlearity "Atmosphere" object.   Used to print text on graph header
plot_pa_pred_results <- function(passed_results, plot_types, state, atmosphere) {
      library(ggplot2)
      results <- passed_results  #copy and round to 3 decimals
      numeric_columns <- sapply(results, mode) == 'numeric'
      results[numeric_columns] <- round(results[numeric_columns], 4)
      
      for (plot_type in plot_types) {
        plot_type <- trimws(plot_type)
        print(sprintf("Plotting %s", plot_type))
        exp_string <- sprintf("%s_exp", plot_type)
        ggplot(results, aes_string("pitcher_name", "batter_name", fill = exp_string)) + geom_tile() + scale_fill_distiller(palette = "Spectral")  + geom_text(aes_string(label = exp_string), size=2) + theme(legend.position = "none", axis.text=element_text(size=5)) + labs(title=sprintf("Predicted %s by Batter vs. Pitcher", plot_type), subtitle=sprintf("%s Inning:%d  Pitcher pitch_count:%d  outs:%d.", venue$name, state$inning, state$pitch_number, state$outs)) + theme(axis.text.x  = element_text(angle=90), plot.title = element_text(size=10, face="bold"), plot.subtitle= element_text(size=8))
        ggsave(sprintf("%s.pdf", plot_type))
      }
}

