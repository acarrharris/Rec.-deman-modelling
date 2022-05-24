# Calculate proportions in each subarea
# Code excerpt created for Lou & Geret 5/6/22
library(dplyr)
library(ggplot2)


load("df_biomass.rdat")
df_biomass1 <- data.frame(df_biomass)
write_xlsx(df_biomass1,"df_biomass1.xlsx")


df_biomass1 %>% 
  group_by(year) %>% summarise(total = sum(total_mt))

df_biomass1 <- aggregate(total_mt ~ year + season + area, data = df_biomass1, FUN=sum)

# plot total biomass
ggplot(data = df_biomass %>% ungroup(), aes(x = year, y = total_mt, color = area)) +
  geom_line() +
  scale_color_manual(values=c("black", "red", "blue", "dark green")) +
  facet_grid(spp~season, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  ylab("Biomass (MT)") +
  xlab("Year") #+
#ggtitle("Total biomass in each area")


# plot proportion
df2plot <-
  df_biomass %>%
  ungroup() %>%
  tidyr::spread(area, total_mt) %>%
  dplyr::mutate(North = North / `Whole shelf`,
                Middle = Middle / `Whole shelf`,
                South = South / `Whole shelf`) %>%
  tidyr::gather(variable, value, -year, -spp, -season) %>%
  dplyr::filter(variable %in% c("North",
                                "Middle",
                                "South")) %>%
  dplyr::mutate(variable = factor(variable, levels = c("North", "Middle", "South")))

ggplot(data = df2plot,
       aes(x = year, y = value, group = variable, color = variable)) +
  geom_line() +
  scale_color_manual(values=c("red", "blue", "dark green")) +
  facet_grid(spp~season) +
  theme_bw() +
  theme(strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        #plot.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  ylab("Proportion of biomass in each area") +
  xlab("Year")