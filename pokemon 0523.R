
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggthemes)
library(highcharter)

pokemon<-read.csv("C:\\Users\\User\\Desktop\\pokemon.csv",sep=",",stringsAsFactors=F)

head(pokemon)
tail(pokemon)
summary(pokemon)


#分別把18個總族的寶可夢列出來
subset(pokemon,Type.1=="Bug")
subset(pokemon,Type.1=="Dark")
subset(pokemon,Type.1=="Dragon")
subset(pokemon,Type.1=="Electric")
subset(pokemon,Type.1=="Fairy")
subset(pokemon,Type.1=="Fighting")
subset(pokemon,Type.1=="Fire")
subset(pokemon,Type.1=="Flying")
subset(pokemon,Type.1=="Ghost")
subset(pokemon,Type.1=="Grass")
subset(pokemon,Type.1=="Ice")
subset(pokemon,Type.1=="Normal")
subset(pokemon,Type.1=="Poison")
subset(pokemon,Type.1=="Psychic")
subset(pokemon,Type.1=="Rock")
subset(pokemon,Type.1=="Steel")
subset(pokemon,Type.1=="Water")


#Type.1 pokemons
pokemon %>% group_by(Type.1) %>% summarise(Total_pokemon=n())%>% data.frame()
#Type.2 pokemons
pokemon %>% group_by(Type.2) %>% summarise(Total_pokemon=n())%>% data.frame()
#Total number of pokemons by generations
pokemon %>% group_by(Generation) %>% summarise(Total_pokemon=n())%>% data.frame()


#Top 10 pokemon with high hp
pokemon %>% group_by(Name,HP)%>% arrange(desc(HP)) %>% ungroup()  %>%
  select("Name","Type.1","HP")%>%head(10)%>% data.frame()

#Top 10 pokemon with high speed
pokemon %>% group_by(Name,Speed)%>% arrange(desc(Speed)) %>% ungroup()  %>%
  select("Name","Type.1","Speed")%>%head(10)%>% data.frame()


# 長條圖
hist(pokemon$HP,col = "red",freq = F,xlab = "HP",main = "HP")
hist(pokemon$Attack,col = "gray",freq = F,xlab = "Attack",main = "Attack")
hist(pokemon$Defence,col = "blue",freq = F,xlab = "Defence",main = "Defence")
hist(pokemon$Sp.Atk,col = "green",freq = F,xlab = "Sp.Atk",main = "Sp.Atk")
hist(pokemon$Sp.Def,col = "purple",freq = F,xlab = "Sp.Def",main = "Sp.Def")
hist(pokemon$Speed,col = "yellow",freq = F,xlab = "Speed",main = "Speed")


## true if single type
singleType.flag <- pokemon$Type.2 == ""

## plot attack vs defense
ggplot(pokemon, aes(x = Attack, y = Defense, color = singleType.flag)) + 
  geom_point(alpha = 0.5) + 
  scale_color_discrete(name = "Type", breaks = c("FALSE","TRUE"), 
                       labels = c("Dual", "Single"))
## plot special attack vs special defense
ggplot(pokemon, aes(x = Sp..Atk, y = Sp..Def, color = singleType.flag)) + 
  geom_point(alpha = 0.5) + labs(x = "Special Attack", y = "Special Defense") +
  scale_color_discrete(name = "Type", breaks = c("FALSE","TRUE"), 
                       labels = c("Dual", "Single"))

## plot Hp vs speed
ggplot(pokemon, aes(x = HP, y = Speed, color = singleType.flag)) + 
  geom_point(alpha = 0.5) + 
  scale_color_discrete(name = "Type", breaks = c("FALSE","TRUE"), 
                       labels = c("Dual", "Single"))

colnames(pokemon)<-c("id","Name","Type.1","Type.2","HP","Attack","Defense","Sp.Atk","Sp.Def","Speed","Generation","Legendary")
Type.1<-c("Dragon","Steel","Flying","Psychic","Rock" ,"Fire","Electric" ,"Dark","Ghost" ,"Ground","Ice", "Water","Grass","Fighting", "Fairy" ,"Poison","Normal","Bug")
color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797","#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")
COL<-data.frame(Type.1,color)

merge(
  merge(pokemon %>% dplyr::group_by(Type.1) %>% dplyr::summarize(tot=n()),
        pokemon %>% dplyr::group_by(Type.1,Legendary) %>% dplyr::summarize(count=n()),by='Type.1'),
  COL, by='Type.1') %>% 
  ggplot(aes(x=reorder(Type.1,tot),y=count)) + 
  geom_bar(aes(fill=color,alpha=Legendary),color='white',size=.25,stat='identity') + 
  scale_fill_identity() + coord_flip() + theme_fivethirtyeight() + 
  ggtitle("Pokemon Distribution") + scale_alpha_discrete(range=c(.9,.6))
 
# pair
n <- setdiff(names(pokemon), c("Name", "X.", "Type.1", "Type.2", "Legendary", "Generation"))
temp <- pokemon[n]
pairs(temp)

##MDS
mds <- dist(pokemon[,5:10]) %>%
  cmdscale()%>%
  data.frame()
xx <- ifelse(pokemon$X. %in% c(25,133,143),"like","soso")
# ggplot(data = mds,mapping = aes(x=X1,y=X2,color=pokemon$Type.1))+
#   geom_point()
hchart(mds,"scatter",hcaes(x=X1,y=X2,group=pokemon$Type.1))
hchart(mds,"scatter",hcaes(x=X1,y=X2,group=xx))
