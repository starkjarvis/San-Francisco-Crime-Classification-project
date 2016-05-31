# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(ggmap)
trainDataset <- read.csv("../input/train.csv")
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
system("ls ../input")
# Any results you write to the current directory are saved as output.
freqTop7 <- sort(table(trainDataset$Category), decreasing=TRUE)[1:7]
freqTop7 <- as.data.frame(freqTop7)
names(freqTop7)[1] <- "Category"
freqTop8 <- sort(table(trainDataset$Category), decreasing=TRUE)[1:8]
freqTop8 <- as.data.frame(freqTop8)
names(freqTop8)[1] <- "Category"
Top1st = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(1)],]
Top2nd = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(3)],]
Top3rd = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(4)],]
Top4th = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(4)],]
Top5th = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(5)],]
Top6th = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(6)],]
Top7th = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(7)],]
Top7 = trainDataset[ trainDataset$Category %in% freqTop7$Category[c(1, 3:8)],]
png(file="San_Francisco_Top8_Crime_Pie_Chart.png")
pie(table(freqTop8$Category), main="Top 8 Crimes pie chart")
dev.off()
map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")
obj1 <- ggmap(map) + geom_point(data=Top1st, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("LARCENY/THEFT(Top Crime) Crime in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
obj2 <- ggmap(map) + geom_point(data=Top2nd, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("NON-CRIMINAL(Top 2nd) Crime in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
obj3 <- ggmap(map) + geom_point(data=Top3rd, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("ASSAULT (Top 3rd) Crime in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
obj4 <- ggmap(map) + geom_point(data=Top4th, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("DRUG/NARCOTIC (Top 4th) Crime in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
obj5 <- ggmap(map) + geom_point(data=Top5th, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("VEHICLE THEFT (Top 5th) Crime in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
obj6 <- ggmap(map) + geom_point(data=Top6th, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("VANDALISM (Top 6th) Crime in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
obj7 <- ggmap(map) + geom_point(data=Top7th, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("WARRANTS(Top 7th) Crime in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
objAll7 <- ggmap(map) + geom_point(data=Top7, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) + guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) + scale_colour_brewer(type="qual",palette="Paired") + ggtitle("Top 7 Crimes in San Francisco") + theme_light(base_size=20) + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
ggsave("San_Francisco_Top1st_Crimes_Classification1.png", obj1, width=9, height=7, units="in")
ggsave("San_Francisco_Top2nd_Crimes_Classification1.png", obj2, width=9, height=7, units="in")
ggsave("San_Francisco_Top3rd_Crimes_Classification1.png", obj3, width=9, height=7, units="in")
ggsave("San_Francisco_Top4th_Crimes_Classification1.png", obj4, width=9, height=7, units="in")
ggsave("San_Francisco_Top5th_Crimes_Classification1.png", obj5, width=9, height=7, units="in")
ggsave("San_Francisco_Top6th_Crimes_Classification1.png", obj6, width=9, height=7, units="in")
ggsave("San_Francisco_Top7th_Crimes_Classification1.png", obj7, width=9, height=7, units="in")
ggsave("San_Francisco_TopAll7_Crimes_Classification1.png", objAll7, width=9, height=7, units="in")