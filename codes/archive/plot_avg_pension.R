library(ggplot2)
library(dplyr)
library(tidyr)

#Régionkénti átlagnyugdíj 2000-2023
avg_pension_region <- readxl::read_excel("C:/Nandi/BPM/Szakdoga/adatok/atlagnyugdij_regio.xlsx", sheet = "oregsegi")

matplot(avg_pension_region$Év, avg_pension_region[,-1], type = "l",
        xlab = "év", ylab = "Átlagos nyugdíj",
        main = "A régiónkénti átlagos öregségi nyugdíj alakulása",
        legend.text = colnames(avg_pension_region[,-1]), lty = 1)
legend("topleft", legend = colnames(avg_pension_region[,-1]), lty = 1, cex = 0.8)
# az látszik, hogy a Budapesti átlagnyugdíj jobban nő, mint a többi régióhoz tartozó

oszlopok <- ncol(avg_pension_region)
elteresek <- data.frame('ev' = 2000:2023)
# országos átlagtól vett eltérések kiszámítása
for (i in 2:9) {
  elteresek[,i] <- avg_pension_region[,i]/avg_pension_region[,10] - 1
}

# országos átlagtól vett eltérések ábrázolása

col1 = "#013C58"
col2 = "#E69C38"
col3 = "#800000"
col4 = "#808000"
col5 = "#104C00"
col6 = "#7195C1"
col7 = "#996633"
col8 = "#E6E122"
szinek <- c(col1, col2, col3, col4, col5, col6, col7, col8)

elteresek %>% pivot_longer(cols = -ev, names_to = "var", values_to = "val") %>%
  ggplot() +
  geom_hline(yintercept = c(seq(-0.15, 0.25, 0.05)), col="lightgrey", linetype="dashed")+
  geom_hline(yintercept=0, color="black", alpha=0.8)+
  geom_line(aes(x=ev,y=val,linetype=var, color = var),linewidth=1) + 
  theme(legend.title = element_blank(), text=element_text(size=12, family="serif"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_line(),
        legend.key = element_rect(fill = "white") )+
  scale_color_manual(values = szinek) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title="A régiónkénti átlagos öregségi nyugdíjak országos átlagtól vett eltérése",
       subtitle = "Magyarország (2000 - 2023)",
       y = "Átlagtól vett eltérés (%)", legend = "Régió")


