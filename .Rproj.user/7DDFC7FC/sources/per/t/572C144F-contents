
FF_dat <- read.csv("FantasyFootball2019csv.csv", stringsAsFactors = FALSE)
FF_dat <- na.omit(FF_dat)
names(FF_dat)[1] <- "Final_rank"
#James will never read this message, but I really had to write a line of code to capitalize his name.
FF_dat$Team.Name[FF_dat$Team.Name == "james kepler"] <- "James Kepler"
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(ggrepel)
library(transformr)
library(kableExtra)
library(knitr)

#Decided to add 2016 afterwards, even though the scoring uncomparable without knowing the number of receptions. 
FF_dat2016 <- read.csv("FF2016.csv")
names(FF_dat2016)[1] <- "Final_rank"
FF_dat <- rbind(FF_dat, FF_dat2016)
FF_dat <- FF_dat %>% separate(Record, c("Wins","Losses"))
#Separating Wins and Losses creates two character vectors.
W_numeric <- as.numeric(FF_dat[,3])
L_numeric <- as.numeric(FF_dat[,4])
WLnumeric <- cbind(L_numeric, W_numeric)
FF_datn <- cbind(WLnumeric, FF_dat)

#orignal pythagorean expectation
FF_datn$PythagWR <- FF_datn$PF^2/(FF_datn$PF^2 + FF_datn$PA^2)
FF_datn$PythagEW <- FF_datn$PythagWR * 14
#Pythagenpat update
pythag_exp <- ((FF_datn$PF + FF_datn$PA)/14)^0.287
FF_datn$pythagenpatWR <- FF_datn$PF^pythag_exp / (FF_datn$PF^pythag_exp + FF_datn$PA^pythag_exp)
FF_datn$pythagenpatEW <- FF_datn$pythagenpat * 14
FF_datn$DIFF_pythagenpat_wins <- FF_datn$W_numeric - FF_datn$pythagenpatEW

#Problem with the first form of pythagenpat is that 2019 has only 10 games so far, but exponent assumes 14
FF_datn2019 <- subset(FF_datn, Year == "2019")
pythag_exp2019 <- ((FF_datn2019$PF+FF_datn2019$PA)/10)^0.287
FF_datn2019$pythagenpat <- FF_datn2019$PF^pythag_exp2019 / (FF_datn2019$PF^pythag_exp2019 + FF_datn2019$PA^pythag_exp2019)
FF_datn2019$pythagenpat_exp_wins <- FF_datn2019$pythagenpat * 10
FF_datn2019$PP_DIFF <- FF_datn2019$W_numeric - FF_datn2019$pythagenpat_exp_wins

#uncategorized stuff
roundtest <- round(FF_datn2019[,15:17], 3)
FF_datn$PAG <- ifelse(FF_datn$Year == "2019", FF_datn$PA / 10, FF_datn$PA / 14)

#HTML Table outputs
FF_datn$PFG <- ifelse(FF_datn$Year == "2019", FF_datn$PF / 10, FF_datn$PF / 14) 

options(kableextra.html.bsTable = TRUE)
FF_not2019 <- subset(FF_datn, Year != "2019")

FF_kable <- FF_datn[,c(3,4,5,11,12,13,14,15,16,17)]
FF_kablenot2019 <- FF_not2019[,c(3,4,5,11,12,13,14,15,16,17)]

FF_kable <- FF_kable[order(-FF_kable$pythagenpatEW),]
FF_kablenot2019 <- FF_kablenot2019[order(-FF_kablenot2019$pythagenpatEW),]

FF_kable %>% kable(row.names = FALSE, col.names = c("Final Rank", "Manager", "Wins","Year","PythagoreanWR", "PythagoreanEW", "PythagenpatWR", "PythagenpatEW", "Difference B/W", "Points/G"), digits = 2) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% scroll_box(width = "1000px", height = "400px")
FF_kablenot2019 %>% kable(row.names = FALSE, col.names = c("Final Rank", "Manager", "Wins", "Year","PythagoreanWR", "PythagoreanEW", "PythagenpatWR", "PythagenpatEW", "Difference B/W", "Points/G"), digits = 2) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% scroll_box(width = "1200px", height = "400px")

##Working time-series ggplot
ggplot(data = FF_datn_not2019, aes(x = Year, y = Final_rank, group = Team.Name, color = Team.Name)) + geom_line() + geom_point() + theme(axis.ticks = element_blank()) + scale_y_reverse(breaks = seq(1,10,1)) + scale_x_continuous(breaks = seq(2016,2018,1)) + theme_fivethirtyeight()
anim_save("FinalRankGIF.gif", animation = last_animation())
##gganimate jitter nightmare
ggplot(data = FF_datn_not2019, aes(x = Year, y = Final_rank, group = Team.Name, color = Team.Name)) + geom_line() + geom_point() + theme(axis.ticks = element_blank()) + scale_y_reverse(breaks = seq(1,10,1)) + scale_x_continuous(breaks = seq(2016,2018,1)) + theme_fivethirtyeight() + labs(title = "Final Positional Standings by Year: {frame_time}", ylab = "Final Rank") + geom_label_repel(aes(label = Team.Name)) + transition_time(Year) + ease_aes('linear')


#none of this stuff works as intended
ggplot(FF_datn, aes(x = Final_rank, y = Year)) + geom_line()

library(reshape2)
FF_datn_melt <- melt(FF_datn, id = "Year")
FF_finalrank <- subset(FF_datn_melt, variable == "Final_rank")

FF_meltRank <- subset(FF_finalrank, variable == "Final_rank")
FF_meltYear <- subset(FF_finalrank, variable == "Year")
FF_melt_bind <- cbind(FF_meltYear, FF_meltRank)

FF_order <- FF_datn[,order("Final_rank")]
#