FF_dat <- read.csv("FantasyFootball2019csv.csv")
FF_dat <- na.omit(FF_dat)
names(FF_dat)[1] <- "Final_rank"
#Decided to add 2016 afterwards, even though the scoring is a much different format. 
FF_dat2016 <- read.csv("FF2016.csv")
names(FF_dat2016)[1] <- "Final_rank"
FF_dat <- rbind(FF_dat, FF_dat2016)
FF_dat <- FF_dat %>% separate(Record, c("Wins","Losses"))
#Separating Wins and Losses creates two character vectors.
W_numeric <- as.numeric(FF_dat[,3])
L_numeric <- as.numeric(FF_dat[,4])
WLnumeric <- cbind(L_numeric, W_numeric)
FF_datn <- cbind(WLnumeric, FF_dat)

#none of this stuff works as intended
> library(reshape2)
> FF_datn_melt <- melt(FF_datn, id = "Year")
> ggplot(FF_datn, aes(x = Final_rank, y = Year)) + geom_line()
> FF_finalrank <- subset(FF_datn_melt, variable == "Final_rank")
> FF_order <- FF_datn[,order("Final_rank")]
#pythagorean expectation
FF_datn$Pythag <- FF_datn$PF^2/(FF_datn$PF^2 + FF_datn$PA^2)