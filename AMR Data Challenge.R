## --------------------------------------------------------------------------##

#Vivli AMR Data Challenge 2023#
#Changes in Fluoroquinolone susceptibility for Enterobacteriaceae among adult population in Asia#
#Team lead: Mrs. Suchitra Thapa, Team member: Dr. Merlin Veronika#

## --------------------------------------------------------------------------##
                               ## BEGINNING OF CODE ##

library(dplyr)
library(ggplot2)
library(tidyr)
library(widyr)
library(maps)
library(patchwork)
library(RColorBrewer)
library(viridis)
library(ggparliament)
library(readr)

## --------------------------------------------------------------------------##

X2023_06_15_atlas_antibiotics <- read_csv("2023_06_15 atlas_antibiotics.csv")

asia <- c("China","India", "Hong Kong","Israel","Japan","Jordan","Korea, South",
          "Kuwait", "Malaysia","Philippines","Qatar","Saudi Arabia",
          "Singapore","Taiwan","Thailand","Turkey")
specie <- c("Citrobacter freundii",
           "Citrobacter koseri",
           "Enterobacter cloacae",
           "Escherichia coli",
           "Klebsiella pneumoniae",
           "Klebsiella oxytoca",
           "Morganella morganii",
           "Proteus mirabilis",
           "Proteus vulgaris",
           "Serratia marcescens")
gender <- c("Male","Female")
age.gr <- c("19 to 64 Years")
special <- c("General Unspecified ICU",
             "Surgery ICU",
             "Medicine ICU")

Filtered_AMR_data <- subset(X2023_06_15_atlas_antibiotics,
               Study == "Atlas" &
               Family == "Enterobacteriaceae" &
               Country %in% asia &
                 Species %in% specie &
                 Gender %in% gender &
                 `Age Group` %in% age.gr &
               Speciality %in% special)
rm(asia, specie, gender, age.gr, special)

Final.data <- select(Filtered_AMR_data, `Isolate Id`, Study, Species,	Family,	Country,
               Gender,	`Age Group`,	Speciality,	Source,	`In / Out Patient`,
               Year,	
               Levofloxacin,	`Levofloxacin_I`,
               Ciprofloxacin,	`Ciprofloxacin_I`,
               Gatifloxacin,	`Gatifloxacin_I`,	
               Moxifloxacin,	`Moxifloxacin_I`)

rm(Filtered_AMR_data)

## --------------------------------------------------------------------------##

# Converting to absolute numbers

Final.data$Levofloxacin[Final.data$Levofloxacin == "<=0.25"] <- "0.249"
Final.data$Levofloxacin[Final.data$Levofloxacin == ">8"] <- "8.001"
Final.data$Levofloxacin <- as.numeric(Final.data$Levofloxacin)

Final.data$Ciprofloxacin[Final.data$Ciprofloxacin == "<=0.12"] <- "0.119"
Final.data$Ciprofloxacin[Final.data$Ciprofloxacin == "<=0.06"] <- "0.059"
Final.data$Ciprofloxacin[Final.data$Ciprofloxacin == ">4"] <- "4.001"
Final.data$Ciprofloxacin <- as.numeric(Final.data$Ciprofloxacin)

# write_excel_csv(Final.data, "Final Data.xls")

## --------------------------------------------------------------------------##

## Susceptible data extraction

# Ciprofloxacin

tb_c <- table(Final.data$Species,
              Final.data$Year,
              Final.data$Ciprofloxacin_I); tb_c

cipro<- as.data.frame(tb_c)
colnames(cipro) <- c("Species","Year","Type","Frequency")
cipro
cipro.s <- subset(cipro, Type == "Susceptible")
# write_excel_csv(cipro.s, "Cipro Susceptible.xls")

rm(tb_c, cipro)

# Levofloxacin 

tb_l <- table(Final.data$Species,
      Final.data$Year,
      Final.data$Levofloxacin_I); tb_l
levo<- as.data.frame(tb_l)
colnames(levo) <- c("Species","Year","Type","Frequency")
levo
levo.s <- subset(levo, Type == "Susceptible")
# write_excel_csv(levo.s, "Levo Susceptible.xls")

rm(tb_l,levo)

## --------------------------------------------------------------------------##

## Susceptibility plots

# Ciprofloxacin 
cipro.plot <- ggplot(cipro.s, aes(x = Year, y = Frequency, 
                                  colour = Species, linetype = Species, 
                                  group = Species)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, shape = 21)+
  theme_bw() +
  theme(text = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Ciprofloxacin - Susceptible",
    x = "Year",
    y = "Isolate frequency")

jpeg('Cipro.Susceptible.jpeg', width = 10, height = 5, units = 'in', res = 300)
cipro.plot
dev.off()
rm(cipro.s, cipro.plot)

# Levofloxacin

levo.plot <- ggplot(levo.s, aes(x = Year, y = Frequency, colour = Species, linetype = Species, 
                    group = Species)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, shape = 21)+
  theme_bw() +
  theme(text = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Levofloxacin - Susceptible",
    x = "Year",
    y = "Isolate frequency")

jpeg('Levo.Susceptible.jpeg', width = 10, height = 5, units = 'in', res = 300)
levo.plot
dev.off()
rm(levo.s,levo.plot)

## --------------------------------------------------------------------------##

## Plots by gender 

# Ciprofloxacin

tb_c <- table(Final.data$Species,
              Final.data$Year,
              Final.data$Ciprofloxacin_I, 
              Final.data$Gender); tb_c
cipro<- as.data.frame(tb_c)
colnames(cipro) <- c("Species","Year","Type","Gender","Frequency")
cipro

# cipro.temp <- spread(cipro, key = Type, value = Frequency)
# write_excel_csv(cipro.temp, "Cipro.stat.xls")

cipro.s <- subset(cipro, Type == "Susceptible")

p1 <- ggplot(subset(cipro.s, Gender == "Male"), aes(x = Year, y = Frequency, colour = Species, linetype = Species, 
                                                    group = Species)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, shape = 21)+
  theme_bw() +
  theme(text = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  labs(
    title = "Male",
    x = "Year",
    y = "Isolate frequency (Absolute numbers)")

p2 <- ggplot(subset(cipro.s, Gender == "Female"), aes(x = Year, y = Frequency, colour = Species, linetype = Species, 
                                                      group = Species)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, shape = 21)+
  theme_bw() +
  theme(text = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Female",
    x = "Year",
    y = "Isolate frequency (Absolute numbers)")

p3 <- p1+p2

jpeg('Cipro.Sus.Gender.jpeg', width = 10, height = 5, units = 'in', res = 300)

p3 + plot_annotation(
  title = "Ciprofloxacin - Susceptible"
) &
  theme(plot.title = element_text(hjust = 0.5)) 

dev.off()
rm (tb_c, cipro, cipro.s, p1,p2,p3)

# Levofloxacin

tb_l <- table(Final.data$Species,
              Final.data$Year,
              Final.data$Levofloxacin_I,
              Final.data$Gender); tb_l
levo<- as.data.frame(tb_l)
colnames(levo) <- c("Species","Year","Type","Gender","Frequency")

# levo.temp <- spread(levo, key = Type, value = Frequency)
# write_excel_csv(levo.temp, "levo.stat.xls")

levo.s <- subset(levo, Type == "Susceptible")

p1 <- ggplot(subset(levo.s, Gender == "Male"), aes(x = Year, y = Frequency, colour = Species, linetype = Species, 
                                                   group = Species)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, shape = 21)+
  theme_bw() +
  theme(text = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  labs(
    title = "Male",
    x = "Year",
    y = "Isolate frequency (Absolute numbers)")

p2 <- ggplot(subset(levo.s, Gender == "Female"), aes(x = Year, y = Frequency, colour = Species, linetype = Species, 
                                                     group = Species)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, shape = 21)+
  theme_bw() +
  theme(text = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Female",
    x = "Year",
    y = "Isolate frequency (Absolute numbers)")

p3 <- p1+p2

jpeg('Levo.Sus.Gender.jpeg', width = 10, height = 5, units = 'in', res = 300)

p3 + plot_annotation(
  title = "Levofloxacin - Susceptible"
) &
  theme(plot.title = element_text(hjust = 0.5)) 

dev.off()
rm (tb_l, levo, levo.s, p1,p2,p3)

## --------------------------------------------------------------------------##

## Scatter plot

# Levo vs Cipro MICs

tb_combo <- table(Final.data$Levofloxacin, Final.data$Ciprofloxacin)
tb_combo

# write.csv(tb_combo, file = "tb_combo.csv")

# Correlation test
cr <- cor.test(Final.data$Levofloxacin, Final.data$Ciprofloxacin,
               method = "kendall")
cr

tb.plot <- ggplot(Final.data, aes(x = Ciprofloxacin, y = Levofloxacin))+ 
  geom_point(size = 2) +
  theme_bw() +
  theme(text = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "MIC Cut-Offs",
    x = "Ciprofloxacin",
    y = "Levofloxacin") +
    annotate(geom="text", x=3, y=8, 
           label=paste("r =",round(cr$estimate,2),",","P value =",cr$p.value),
           color="black", 
           fontface = 2) +
    annotate(geom="text", x=0.6, y=7.5, 
           label=("Sensitive"),
           color="Green", 
           fontface = 2) +
    annotate(geom="text", x=1.4, y=7.5, 
           label=("Resistant"),
           color="Red", 
           fontface = 2)
  

# Cipro breakpoint: Sensitive - 0.25, Resistant - 1
# Levo breakpoint: Sensitive - 0.5, Resistant - 1

jpeg('MIC.jpeg', width = 10, height = 5, units = 'in', res = 300)
tb.plot + geom_smooth(method = "lm") +
  geom_vline(xintercept = 0.25, col = "green", lwd = 2, lty = 4) +
  geom_hline(yintercept = 0.5, col = "green", lwd = 2, lty = 4) +
  geom_vline(xintercept = 1, col = "red", lwd = 2, lty = 2) +
  geom_hline(yintercept = 1, col = "red", lwd = 2, lty = 2)
dev.off()
rm(tb_combo,tb.plot,cr)

## --------------------------------------------------------------------------##

## Geospatial maps

world <- map_data("world")

asia <- c("China","India", "Hong Kong","Israel","Japan","Jordan","South Korea",
          "Kuwait", "Malaysia","Philippines","Qatar","Saudi Arabia",
          "Singapore","Taiwan","Thailand","Turkey")

I.count <- table(Final.data$Country,Final.data$Ciprofloxacin_I, 
                 Final.data$Year)
I.count<- as.data.frame(I.count)
colnames(I.count) <- c("region","type","year","freq")
levels(I.count$region)[levels(I.count$region) == "Korea, South"] <- "South Korea"
I.count <- spread(I.count, key = type, value = freq)

# write.csv(I.count, "Country wise count.csv")

# Subset as per year
I.count.y<- subset(I.count, year == "2018")

world.asia <- subset(world, region %in% asia)

worldSubset <- inner_join(world.asia, I.count.y, by = "region")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

suscep <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, 
                                                   group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Susceptible)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction = 1
  ggtitle("2018") +
  plain

jpeg('Cipro.2018.jpeg', width = 10, height = 5, units = 'in', res = 300)
suscep
dev.off()

rm(I.count.y, suscep, worldSubset)
rm(I.count, asia, plain, world, world.asia) # remove after plotting for all years

# rm(Final.data, X2023_06_15_atlas_antibiotics)

                               ## END OF CODE ##

## --------------------------------------------------------------------------##
