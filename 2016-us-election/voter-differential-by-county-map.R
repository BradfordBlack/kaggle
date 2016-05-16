library(readr)
library(plyr)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(scales)

# set working directory
# setwd("...")

# read data
data1 <- read_csv("../input/primary_results.csv")

# filter the data to include only counties where both parties voted
temp1 <- ddply(data1, .(state, county, party), summarize, min = min(1))
temp2 <- ddply(temp1, .(state, county), summarize, cnt = sum(min))
temp3 <- temp2[temp2$cnt == 1,]
data2 <- anti_join(data1, temp3, by = c("state", "county"))

# what's the differential between the number of Republican voters and number of Democrat voters
sdata1 <- ddply(data2, .(state, county, fips), summarize, dif = sum(votes * ifelse(party == "Democrat", -1, 1)))
sdata1$party <- ifelse(sdata1$dif > 0, "Republican", "Democrat")

# delete counties that potentially didn't vote
sdata1 <- sdata1[sdata1$dif != 0,]

# select data for mapping party voter majority/winner by county
sdata11 <- select(sdata1, region = fips, value = dif)
sdata11$value <- sign(sdata11$value)

# plot party voter majority/winner by county
choro11 = CountyChoropleth$new(sdata11)
choro11$title = "Part Voter Majority by County"
choro11$ggplot_scale = scale_fill_manual(values = c("#0000BF", "#BF0000"),
                                        na.value = "grey90",
                                        name = element_blank(),
                                        labels = c("Democrat", "Republican"),
                                        drop = FALSE)
choro11$ggplot_polygon = geom_polygon(aes(fill = value), 
                                     color = NA)
choro11$render()

# save party voter majority/winner by county plot
ggsave(paste0("Party Voter Majority by County.png"), choro11$render(), height = 7.5, width = 10, units="in")

# select data for mapping party voter differential by county
sdata12 <- select(sdata1, region = fips, value = dif)

# identify upper (and lower) bounds for continuous scale
bound = max(abs(sdata12$value))

# plot party voter differential by county
choro12 = CountyChoropleth$new(sdata12)
choro12$set_num_colors(1)
choro12$title = "Party Voter Differential by County"
choro12$ggplot_scale = scale_fill_gradientn(limits = c(-bound, bound), 
                                            colors = c("#0000bf", "#f2e6f2", "#bf0000"), 
                                            na.value = "grey90",
                                            name = "Differential",
                                            labels = scales::comma)
choro12$ggplot_polygon = geom_polygon(aes(fill = value), 
                                    color = NA)
choro12$render()

# save party voter differential by county plot
ggsave(paste0("Party Voter Differential by County.png"), choro12$render(), height = 7.5, width = 10, units="in")
