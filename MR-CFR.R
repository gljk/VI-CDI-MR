require(ggplot2)
require(tidyverse)
require(ggthemes)
require(plotly)

cohortFertility<- read.csv("MRfert.csv")
cohortFertility<- cohortFertility[, 1:3]
cohortFertility <- cohortFertility %>% gather(TFR..15.49., TFR..15.40.,value="Stopa Kumulativnog Fertiliteta", key="Kohorta")
cohortFertility$Kohorta <- as.factor(cohortFertility$Kohorta)

levels(cohortFertility$Kohorta) <- c("15-40","15-49")

CFRgg <-
  ggplot(data = cohortFertility,
         aes(
           x = Cohorts,
           y = `Stopa Kumulativnog Fertiliteta`,
           group=Kohorta,
           col=Kohorta,
           text = sprintf(
             "Žene rođene <b>%s</b>. su od svoje %s god. u proseku rodile <b>%s</b> dece",
             Cohorts,
             Kohorta,
             format(`Stopa Kumulativnog Fertiliteta`,decimal.mark = ",")
           )
           )) +
  geom_line(aes(linetype = Kohorta)) + theme_minimal() + ylab(NULL) +
  scale_y_continuous(
    breaks = seq(1.5, 2, by = 0.1),
    labels = format(seq(1.5, 2, by = 0.1),decimal.mark = ","),
    limits = c(1.5, 1.9)
  ) +
  scale_x_continuous(
    breaks = seq(1930, 1975, by = 5),
    labels = seq(1930, 1975, by = 5),
    limits = c(1930, 1975)
  ) + scale_color_manual(values = c("#ef3b2c", "#cb181d"))+ scale_linetype_manual(values = c(2,1))+
  guides(linetype = guide_legend(title = "Starost"), color= guide_legend(title = NULL)) + xlab("Godina rođenja") +
  ylab("Prosečan broj dece")

ggplotly(CFRgg, tooltip="text")

