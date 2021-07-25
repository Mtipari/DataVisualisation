---
title: "Comic Books: By Men, For Men and About Men"
author: "Mrunal Tipari"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


Install and load necessary libraries
```{r}
packages <- c("rvest", "dplyr","ggplot2","fivethirtyeight")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(dplyr)
library(rvest)
library(ggplot2)
library(fivethirtyeight)
```

#### A: Data Wrangling
Three datasets were used for the analysis.

#### 1. Dataset about comic books characters (source: FiveThirtyEight) OR load the data using the csv files

##### Data Extraction
Dataset named comic_characters.csv is imported from fivethirtyeight library containing all the information about comic books characters using R language. The columns giving relevant information were extracted into a dataframe.


```{r}
df <- comic_characters
dfComic = select(df, c(publisher, name, sex, appearances, year))
dfComic = filter(dfComic, sex %in% c('Male Characters','Female Characters'))
```

OR Import the data by reading the csv files

```{r}
dc = read.csv('dc-wikia-data.csv')
dc$publisher = 'DC'
print("DC data")
glimpse(dc)      

marvel = read.csv('marvel-wikia-data.csv')
marvel$publisher = 'Marvel'
print("Marvel data")
glimpse(marvel)

dfCombine = rbind(dc, marvel, use.names=FALSE)
print("Combined data frame")
glimpse(dfCombine)

dfComic = select(dfCombine, c(publisher, name, SEX, APPEARANCES, YEAR))
dfComic = filter(dfComic, SEX %in% c('Male Characters','Female Characters'))
```

##### Data Wrangling
The appearances (number of appearances till 2013) and year (released year) column had missing values which were imputed. The mode for appearances column was calculated and the NAs were replaced by it. 

```{r}
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Calculate the mode 
result <- getmode(dfComic$APPEARANCES)
print(result)

dfComic$APPEARANCES[is.na(dfComic$APPEARANCES)] = 1
```

#### 2. Dataset created about comic creators from web scraping (source: Wikipedia)
There was no readymade dataset available for Comic books creators with their gender. So, the list of American comic books creators (481 records) was scraped from the Wikipedia page using rvest library and stored in a .csv file using R, then the values for the gender were entered manually in Excel. This file was again loaded into the R environment.

```{r}
	scraping_wiki <- read_html("https://en.m.wikipedia.org/wiki/List_of_American_comics_creators")
li_text <- scraping_wiki %>%
  html_nodes("li") %>%
  html_text()
length(li_text)
ComicCreators = li_text[4:484]
ComicCreators = as.data.frame(ComicCreators)
#write.csv(ComicCreators,'ComicCreators.csv')

ComicCreators1 = read.csv('ComicCreators.csv')
glimpse(ComicCreators1)
ComicCreators1 = select(ComicCreators1, -c(X))
```

#### 3. Dataset created about Top Comics Sales from web scraping (source: CBR.com)

##### Data Extraction
There was no dataset available about the best-selling comics, therefore one was created by scraping the table containing those details from CBR.com using rvest library in R environment. 

```{r}
scraping_web <- read_html("https://www.comichron.com/monthlycomicssales/2017/2017-10.html")
tblNodes = html_nodes(scraping_web, 'table')
tbls = html_table(tblNodes)
topComics = as.data.frame(tbls[[1]])
glimpse(topComics)
```

##### Data Wrangling
As there were no details about the gender of the lead characters in the comic books, the appropriate gender was added manually by creating a .csv file and editing it in Excel. The edited .csv file was loaded back in R environment. As the analysis needed only the details of top fifteen best selling comic books, only those rows and relevant columns were selected. Extra details about the comic book title were present in round brackets and the comma present in the sales amount attribute were removed by using gsub function.

```{r}
#write.csv(topComics, 'topComics.csv')
topComics = read.csv('topComics.csv')
topComicsTopFifteen = topComics[1:15,]
glimpse(topComicsTopFifteen)
topComicsTopFifteen = select(topComicsTopFifteen, c(QtyRank,Pub,Title,Sex,EstSales))
str(topComicsTopFifteen)
topComicsTopFifteen$EstSales = as.numeric(gsub(",", "", topComicsTopFifteen$EstSales))
topComicsTopFifteen$Title<-gsub( " *\\(.*?\\) *", "",topComicsTopFifteen$Title)
```

#### B: Data Visualisations
###### Gender wise distribution of comics characters

The library dplyr was loaded to perform data transformation and library ggplot2 was loaded to create visualisations. The dataframe created earlier named dfComic was grouped by sex and publisher name to get the gender-wise count of characters:

```{r}
genderCount<-dfComic %>% group_by(SEX,publisher) %>% summarise(number = n()) %>% arrange(-number)
```

To get the distribution of gender in percentage across the publishers:

```{r}
genderPercentage<-genderCount %>% group_by(publisher) %>% mutate(countT= sum(number)) %>% group_by(SEX) %>% mutate(percentage=100*number/countT)
```

Percentage labels are created which are then used in ggplot.

```{r}
genderPercentage$LABEL <-paste0(round(genderPercentage$percentage,2))
```

A grouped bar chart is created using ggplot, theme function is used to remove the chart junk and geom_text is used to put the label at each bar.

```{r} 
GenderDistri <-ggplot(data=genderPercentage, aes(x=SEX, y=percentage, 
                                                 fill=publisher)) + 
  	geom_bar(width = 0.9, stat="identity", position='dodge') + 	theme_bw()+
theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
geom_text(aes(label=LABEL), position=position_dodge(width=0.9), vjust=-0.25,size=3.0) + 
scale_fill_manual(values = c("orange","steelblue")) +
  	xlab('Gender') + ylab('Percentage') +
  	ggtitle('Gender-wise Distribution of Comic Books Characters')
```

Two dataframes were created to count the number of new Male and Female characters introduced each year from 1935 to 2013. table function was used to get the count.
For Female Characters:

```{r}
dfFemale = select(dfComic, c(SEX,YEAR))
dfFemale = filter(dfFemale, SEX=='Female Characters')
glimpse(dfFemale)
countFemaleYear = table(dfFemale$YEAR)
glimpse(countFemaleYear)
countFemaleYear = as.data.frame(countFemaleYear)
colnames(countFemaleYear) = c("Year","FemaleCount")
```

For Male Characters:

```{r}
dfMale = select(dfComic, c(SEX,YEAR))
dfMale = filter(dfMale, SEX=='Male Characters')
glimpse(dfMale)

countMaleYear = table(dfMale$YEAR)
glimpse(countMaleYear)
countMaleYear = as.data.frame(countMaleYear)
colnames(countMaleYear) = c("Year","MaleCount")
```

The above two dataframes were then joined by year:

```{r}
combinedf = left_join(countMaleYear,countFemaleYear, by = 'Year')
combinedf = as.data.frame(combinedf)
glimpse(combinedf)  
```
As Male or Female characters might not be introduced during particular, NAs are replaced by zero.

```{r}
combinedf[is.na(combinedf)] = 0
#combinedf$Year = as.POSIXct(combinedf$Year)
combinedf$Year = as.numeric(format(combinedf$Year, format="%Y") )
```
Line graph is created to show the trend of new characters introduced. theme functions are used to remove the chart junk and increase data to ink ratio. The labels of the x axis are kept at 45-degree angle to make it readable.

```{r}
trend = ggplot(combinedf, aes(x=Year), group = 1) + 
  geom_line(aes(y = MaleCount, colour = 'Male')) + 
  geom_line(aes(y = FemaleCount, colour = 'Female'))+
  scale_colour_manual("Gender", 
                      breaks = c("Male", "Female"),
                      values = c("blue", "red")) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

```
               
As the data is of 78 years it makes it unreadable when all the years are kept on the graph, so interval of 6 is kept between the years.

```{r}
trend + scale_x_continuous(limits=c(1935, 2013),breaks=seq(1935, 2013, 6)) +
  scale_y_continuous(limits=c(0, 550),breaks=seq(0, 550, 100)) +
  ylab("Number of characters") +
  ggtitle("Trend of New Characters Introduced 1935-2013")
```

The attributes required for this visualisation are selected and a new dataframe named dfComicAppear is created. 

```{r}
dfComicAppear<-as.data.frame(dfComic %>% select(name,SEX,YEAR,APPEARANCES) 
%>% na.omit())
```

A grouped bar chart is created to compare the difference between the number of appearances each year. Different colors are used for Male and Female to spot the difference. The labels are kept at 45-degrees to improve the readability and borders and background of the graph is also removed and kept simple.

```{r}
Appearanace = ggplot(data=dfComicAppear,aes(x=YEAR,y=APPEARANCES,fill = SEX)) + geom_bar(width = 0.9, stat="identity",position='dodge') + theme(axis.text.x = element_text(angle=90, hjust=1)) + scale_x_continuous(limits=c(1935, 2013),breaks=seq(1935, 2013,6)) + 
  theme_bw() + 
theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
       	 panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
ylab("Number of appearances") +
ggtitle('Comparison of Gender-wise Character Appearances 1935-2013')
```
```{r Appearanace, echo=FALSE}
plot(Appearanace)
```

Pie chart has been created to clearly show the difference between the gender of comics creators. The percentage of the distribution is first calculated and stored in a dataframe which is then used to create the pie chart. Geom_text is used to show the percentage on the chart.

```{r}
percentageSex = ComicCreators1%>% group_by(Sex) %>% summarise(number = n()) %>% arrange(-number) 

percentageSex = percentageSex %>% mutate(countT= sum(number)) %>% mutate(percentage=100*number/countT)

pieCreator <-ggplot(percentageSex,aes(x="",y=percentage,fill=Sex)) + 
geom_bar(stat='identity',width = 1) + 
coord_polar(theta="y") + theme_void() + theme(axis.text.x=element_blank()) + 
geom_text(aes(x=1.2, y=c(50,97),label = 
paste(round(percentage,2),"%"))) +
scale_y_continuous(expand = c(0,0))+
ggtitle("Gender Ratio of Comic Creators")
```

Top 15 Best Selling Comic Books of 2017
	The bar chart is created and ordered according to the top sales. The bars are coloured based on the lead character gender in comic books. The labels are kept at 45-degree.

```{r}
topComicSale = ggplot(topComicsTopFifteen , aes(x=reorder(Title,-
EstSales),y=EstSales,fill=Sex )) + geom_bar(stat = "identity")+
  		theme_bw()+
theme(panel.border = element_blank(),
    			panel.grid.major = element_blank(),
      			panel.grid.minor = element_blank(),
       		axis.text.x = element_text(angle = 45, hjust = 1))+
   	ggtitle("Top 15 Best Selling Comic Books of 2017")+
  	xlab("Comic Book Title") + ylab("Sales")
```


```{r topComicSale, echo=FALSE}
plot(topComicSale)
```

