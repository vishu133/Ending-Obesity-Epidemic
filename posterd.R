#IST 719 Poster Code
# Vishwanath Hegde


mappath <- file.choose()
worldmap <- read.csv(mappath,stringsAsFactors = F)


EnsurePackage <- function(x) {
  x <- as.character(x)
  
  if(!require(x,character.only = T)){
    install.packages(pkgs = x,repos = "http://cran.r-project.org")
    require(x,character.only = T)}
  
}

EnsurePackage("caret")
EnsurePackage("dplyr")
EnsurePackage("ggmap")
EnsurePackage("ggplot2")
EnsurePackage("rworldmap")
EnsurePackage("readxl")
EnsurePackage("tidyr")
EnsurePackage("directlabels")
EnsurePackage("tidyverse")
EnsurePackage("viridis")
EnsurePackage("patchwork")
EnsurePackage("hrbrthemes")
EnsurePackage("fmsb")
EnsurePackage("colormap")
EnsurePackage("RColorBrewer")

#Creating the first script of the poster world map that 
#showcases prevalence of obesity

WorldData <- map_data(map="world")
WorldData <- WorldData[!WorldData$region == "Antarctica",]
worldmap <- na.omit(worldmap)
worldmap$X2016Both.sexes <-  as.numeric(worldmap$X2016Both.sexes)

worldmap[,1] <- as.character(worldmap[,1])

p <- ggplot() + 
  geom_map(data=WorldData, map=WorldData,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black")
p


p<- p+ geom_map(data = worldmap, map=WorldData,
                aes(fill=worldmap$X2016Both.sexes, map_id=Country),
                size=0.5) + 
  labs(x =NULL , y = NULL,
       title="Prevalence of Obesity Around the World in 2016") +
  theme(plot.background = element_rect(fill = "yellow"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "yellow",
                                        colour = "yellow",
                                        size = 0.5, linetype = "solid"),
    legend.position = "bottom")+
  scale_fill_gradient2(name = "% of Obesity",
                       low = "darkslategray1", mid = "red", 
                       high = "red3",
                       midpoint = 35,
                       breaks = c(0,10,20,30,40,50),
        guide = guide_legend(direction = "horizontal",
                             
                             keyheight = unit(2, units = "mm"),
                             keywidth = unit(15, units = "mm"),
                             title.position = 'top',
                             title.hjust = 0.5,
                             label.hjust = 1,
                             nrow = 1,
                             byrow = T,
                             reverse = F,
                             label.position = "bottom"))
p


#Creating line charts to show trends
trendpath <- file.choose()
trend0 <- read_excel(trendpath)

normtrend <- spread(trend0, Category, Calories)
normtrend <- normtrend[!is.na(normtrend$`Prevalence of obesity`),]
normtrend <- normtrend[!is.na(normtrend$Dairy),]

normtrend <-normtrend %>% 
  mutate(TotalCalorie = rowSums(normtrend[,c(2,3,4,5,6,7,9)]))

normtrend[,names(normtrend[c(2,3,4,5,6,7,9)])] <- 
  normtrend[,names(normtrend[c(2,3,4,5,6,7,9)])]/normtrend$TotalCalorie*100

normtrendp <- normtrend[,names(normtrend[c(1,2,3,4,5,6,7,8,9)])]

normtrendp <- round(normtrendp,1)
normtrend[,2:ncol(normtrendp)] <- as.data.frame(lapply(normtrendp[,2:ncol(normtrendp)], normalize))

trend <- gather(data = normtrendp,"Category","Calories",2:ncol(normtrendp))

fldairy <- data.frame(filter(trend,Category == "Dairy"))
fldairyfl <- filter(fldairy,Year == 1976 | Year == 2010)
dairy <- ggplot(data=fldairy, aes(x=Year, y=Calories)) +
  geom_line(aes(y = Calories,x=Year ))+
  labs(title= "Consumption of Dairy",
       y="Percentage")+
  geom_text(data = fldairyfl,aes(label=Calories))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dairy

flfats <- data.frame(filter(trend,Category == "Added fats and oils and dairy fats"))
flfatsfl <- filter(flfats,Year == 1976 | Year == 2010)
fats <- ggplot(data=flfats, aes(x=Year, y=Calories)) +
  geom_line(aes(y = Calories,x=Year ))+
  labs(title= "Consumption of Fats & Oils",
       y="Percentage")+
  geom_text(data = flfatsfl,aes(label=Calories))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fats

flsugar <- data.frame(filter(trend,Category == "Added sugar and sweeteners"))
flsugarfl <- filter(flsugar,Year == 1976 | Year == 2010)
  
sugar <- ggplot(data=flsugar, aes(x=Year, y=Calories)) +
  geom_line(aes(y = Calories,x=Year ))+
  labs(title= "Added Sugar & Sweetners",
       y="Percentage")+
  geom_text(data = flsugarfl,aes(label=Calories))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sugar
  

flobesity <- data.frame(filter(trend,Category == "Prevalence of obesity"))
flobesityfl <- filter(flobesity,Year == 1976 | Year == 2010)
obesity <- ggplot(data=flobesity, aes(x=Year, y=Calories)) +
  geom_line(aes(y = Calories,x=Year ))+
  labs(title= "Prevalence of Overweight",
       y="Percentage")+
  geom_text(data = flobesityfl,aes(label=Calories))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

obesity

flgrains <- data.frame(filter(trend,Category == "Grains"))
flgrainfl <- filter(flgrains,Year ==1976 | Year ==2010)
  
grains <- ggplot(data=flgrains, aes(x=Year, y=Calories)) +
  geom_line(aes(y = Calories,x=Year ))+
  labs(title= "Grains",
       y="Percentage")+
  geom_text(data = flgrainfl,aes(label=Calories))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
grains

flvegetable <- data.frame(filter(trend,Category == "Vegetables"))
flvegetablefl <- filter(flvegetable,Year == 1976 | Year == 2010)

vegetable <- ggplot(data=flvegetable, aes(x=Year, y=Calories)) +
  geom_line(aes(y = Calories,x=Year ))+
  labs(title= "Grains",
       y="Percentage")+
  geom_text(data = flvegetablefl,aes(label=Calories))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
vegetable


#Creating stacked bar chart to visualize consumption

comparecalorie <- filter(trend0, Year == 1975 | Year == 2010)
cctable <- table(comparecalorie$Year,comparecalorie$Category)


barplot(cctable,beside = T,main = "1980 vs 2010 Calorie Consumption of Average American",
        col = c("grey","blue"))

comparecalorie$Category <- gsub("Added fats and oils and dairy fats","fats & oils",comparecalorie$Category)
comparecalorie$Category <- gsub("Added sugar and sweeteners","sugar & sweets",comparecalorie$Category)
comparecalorie$Category <- gsub("Prevalence of obesity","Obesity %",comparecalorie$Category)
comparecalorie <- filter(comparecalorie,Category != "Obesity %")

ggplot(comparecalorie[order(comparecalorie$Calories),], 
       aes(Category, Calories, group=as.factor(Year),
                           fill=as.factor(Year),label=round(Calories))) + 
  geom_bar(stat='identity', position='dodge')+
  geom_text()+
  labs(title="1980 vs 2010 Calorie Consumption of Average American",fill = "Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



#Creating Scatterplot to visualize nutrition profile
# of various food types

nutritionpath <- file.choose()
nutrition <- read.csv(nutritionpath,header = T,stringsAsFactors = F)

nutrition[,4:55] <- data.matrix(nutrition[,4:55])

nutrition.agg <- aggregate(nutrition,list(Type = nutrition$Food.Group),mean,na.rm=TRUE)

#normalize all the values in data
nutrition.agg[,-(1:4)] <- as.data.frame(lapply(nutrition.agg[,-(1:4)], FUN = function(X) (X - min(X))/diff(range(X))))

#load data aggregate according to food types and create 2 new col
#micro nutrient and macro nutrient

#macro nutrient
nutrition.agg <- mutate(nutrition.agg, macro = Protein.g +
                                                Fat.g + 
                          Carbohydrates.g + Water.g + Fiber.g)
nutrition.agg<- mutate(nutrition.agg, micro = 
                              Calcium.mg +
                                Iron.mg +
                                Magnesium.mg + Phosphorus.mg +
                                Zinc.mg + Cupper.mg +
                                Manganese.mg + Selenium.mcg +
                                Vitamin.A.IU + Vitamin.E.mg +
                                Vitamin.D.mcg + Vitamin.C.mg +
                                Vitamin.B6.mg +
                                Folate.B9.mg + Vitamin.B12)

nutrition <- nutrition[order(nutrition$Food.Score,decreasing = T),]

topbottom10 <- nutrition %>%
                filter(rank(desc(nutrition$Food.Score))<=10 |rank(desc(nutrition$Food.Score))>=10)
visual <- rbind(head(nutrition,3000),tail(nutrition,3000))

ggplot(nutrition.agg[-1,], aes(x=macro, 
                   y=micro, 
                   label = Type,size=Saturated.Fat.g,colour=Protein.g)) + 
  geom_point() +
  scale_color_gradient2(breaks = c(seq(from = 0, 
                                      to = 1, by = 0.3)), 
                       low = "green",mid = "blue",high = "brown",
                       midpoint = 0.5)+
  scale_size(range=c(0,8))+
  labs(x="Macro - Carbohydrates, Fats & Proteins",
       y="Micro - Vitamins & Minerals")+
  geom_text(cex=2,vjust=-1.5) +
  theme(axis.text.x = element_text( hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  


#Spidercharts
spiderpath <- file.choose()
spidernet <- read.csv("Spidernet.csv",stringsAsFactors = F,
                      header = T)
par(mar=c(0,0,0,0))
names(spidernet)[1] <- c("Foodtype")

nutrients <- c("Wt.g.","Calories","Tot.fat","Sat.fat",
                   "Cholesterol.mg.","Protein","Carbohydrates",
                   "Fiber","Sodium")
#Pancake flour
pancakef <- spidernet[spidernet$Item == "Pancakes(Flour)",
                      nutrients]
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
pancakef <- rbind(c(200,300,20,10,30,10,50,10,500) , rep(0,8) , pancakef)

spidernet1 <- spidernet[,nutrients.cyc]
par(mfrow = c(2,1),mar=c(0.8,0.8,0.8,0.8))
#Pancake flour
radarchart(pancakef,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,title = "Flour")

pancakew <- spidernet[spidernet$Item == "Pancakes(Whole Wheat)",
                      nutrients]         

pancakew <- rbind(c(200,300,20,10,30,10,50,10,500) , 
                  rep(0,8) , 
                  pancakew)           
#Pancake wheat
radarchart(pancakew,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,
           title = "Wheat Flour")


#Bake vs Fry
#Chips
fry.chips <- spidernet[spidernet$Item == "Chips (Fried)",
                       nutrients]  
fry.chips <- rbind(c(200,300,20,10,30,10,50,10,500) , rep(0,8) , fry.chips)

bake.chips <-spidernet[spidernet$Item == "Chips (Baked)",
                        nutrients]  


bake.chips <- rbind(c(200,300,20,10,30,10,50,10,500) , rep(0,8) , bake.chips)
par(mfrow = c(2,1),mar=c(0.8,0.8,0.8,0.8))
#fry chips
radarchart(fry.chips,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,title = "Fried Chips")

        
#Bake chips
radarchart(bake.chips,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,
           title = "Baked Chips")

#Fruits vs Donut
donut <- spidernet[spidernet$Item == "Donut",
                       nutrients]  
donut <- rbind(c(200,300,20,10,30,10,50,10,500) , rep(0,8) ,
                   donut)

fruit <-spidernet[spidernet$Item == "Fruit",
                       nutrients]  

fruit <- rbind(c(200,300,20,10,30,10,50,10,500) , rep(0,8) ,
                    fruit)
par(mfrow = c(2,1),mar=c(0.8,0.8,0.8,0.8))
#Donut
radarchart(donut,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,title = "Donut")


#Fruit
radarchart(fruit,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,
           title = "Apple Gala")


#Healthy Fat vs non healthy fat
cream.cheese <- spidernet[spidernet$Item == "Cream Cheese",
                   nutrients]  
cream.cheese<- rbind(c(200,300,20,10,30,10,50,10,500) , rep(0,8) ,
               cream.cheese)

peanut.butter <-spidernet[spidernet$Item == "Peanut Butter",
                  nutrients]  

peanut.butter <- rbind(c(200,300,20,10,30,10,50,10,500) , rep(0,8) ,
               peanut.butter)
par(mfrow = c(2,1),mar=c(0.8,0.8,0.8,0.8))
#Donut
radarchart(cream.cheese,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,title = "Cream Cheese")


#Fruit
radarchart(peanut.butter,axistype = 1,calcex=0.6,
           
           # Custom polygon
           pcol = rgb(0.2,0.8,0.5,0.9),
           pfcol =rgb(0.2,0.8,0.5,0.5),
           plwd =2,
           #Custom grid
           cglcol ="grey",cglty = 1,axislabcol = "grey",
           cglwd = 1,
           #Custom labels
           vlcex = 0.6,
           title = "Peanut Butter")

