tbl_orders <- tibble(Kurs=c(119:126),
                      Nachfrage=c(NA,77,62,57,54,38,32,29),
                      Angebot=c(26,28,34,50,54,61,71,71))

tbl_plot <- tbl_orders %>%
                      pivot_longer(-Kurs,names_to = "Variable",
                                   values_to = "Stück")


p <- tbl_plot %>%
               ggplot(aes(x=Stück,y=Kurs,color=Variable)) +
               geom_step()
p <- p + ylim(c(118, 127)) + xlim(c(0,90))
p <- p + scale_y_continuous(breaks = seq(118,127,1)) +
         scale_x_continuous(breaks=seq(0,90,10))
p <- p + theme_light() +
         theme(legend.position="bottom",
               legend.title=element_blank())
