library(readr)
library(plyr)
library(ggplot2)
library(scales)

# set working directory
setwd("..")

# read data
data <- read_csv("../input/primary_results.csv")

# filter the data to include only states where both parties voted
temp1 <- ddply(data, .(state, party), summarize, min = min(1))
temp2 <- ddply(temp1, .(state), summarize, cnt = sum(min))
temp3 <- temp2[temp2$cnt == 1,]
data2 <- data[is.na(match(data$state,temp3$state)),]

# what's the differential between the number of Republican voters and number of Democrat voters
sdata <- ddply(data2, .(state), summarize, dif = sum(votes * ifelse(party == "Democrat", -1, 1)))
sdata$party <- ifelse(sdata$dif > 0, "Republican", "Democrat")

# plot the results
p <- ggplot(data=sdata, aes(x=state, y=dif, fill=party)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("Republican" = "#BF0000", "Democrat" = "#0000BF")) +
  coord_flip() +
  scale_x_discrete(limits = rev(sdata$state)) +
  scale_y_continuous(labels = scales::comma) +
  ylab("# Republican Voters - # Democrat Voters") +
  ggtitle("Difference between Number of Republican and Democrat Primary Voters by State") +
  theme_light() +
  theme(legend.position="top", legend.title=element_blank(), axis.title.y=element_blank())

# view the results
p

# save plot
ggsave(paste0("Voter Differential by State.png"), p, height=7.5, width=10, units="in")
