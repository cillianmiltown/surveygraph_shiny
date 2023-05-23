devtools::install_github("surveygraph/surveygraphr")
surveygraphr::gensurvey()

library(ggplot2)

S <- surveygraphr::gensurvey(200,25)
class(S)
results <- surveygraphr::exploregraph(S)

df <- data.frame(radius = unlist(results[1]), degree = unlist(results[2]), LCC = unlist(results[3]))

head(df)
rm(list = ls())
df
ggplot(df, aes(x=radius,y=LCC))+geom_point()

edgelist <- surveygraphr::listgraph(S)



igraph::make_graph()
g <- igraph::make_graph(edges=edgelist, directed=FALSE)

plot(g)

plot(g, vertex.size=5, vertex.label=NA)



load("data_with_means.RData")



ICSMP_500 <- sample_n(df,500)

variable.names(ICSMP_500)



COVID_measures <- ICSMP_500 %>% 
  select(
    "contact1"
    ,"contact2"
    ,"contact3"
    ,"contact4"
    ,"contact5" 
    ,"psupport1"
    ,"psupport2"
    ,"psupport3"
    ,"psupport4"
    ,"psupport5"
    ,"hygiene1"                 
    ,"hygiene2"
    ,"hygiene3"
    ,"hygiene4"
    ,"hygiene5"
    ,"physical_contact_tot"
    ,"physical_hygiene_tot"
    ,"policy_support_tot")


save(ICSMP_500,COVID_measures, file="ICSMP_500.RData")

#sssurvey cran
#anesr on github




obj_vx_fun <- function(x){
  noquote(paste("obj$", noquote(d[x]),sep = ""))}

sapply(1:length(d), obj_vx_fun)

obj_vx_fun(2)

c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)

d_vector <- sapply(1:length(d), obj_vx_fun)


inputted_variables_fun <- function(x){
  eval(parse(text=d_vector[x]))}


# eval(parse(text=d_vector[1]))



obj <- get_data()

S <- obj$data

c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)

S <- S %>% select(c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5))
S <- cbind.data.frame(
  as.numeric(unlist(S[1]))
  ,as.numeric(unlist(S[2]))
  ,as.numeric(unlist(S[3]))
  ,as.numeric(unlist(S[4]))
  ,as.numeric(unlist(S[5]))
)
colnames(S) <- c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)


# use [[]] to call list


#### 16th May 2023 ####


S1 <- surveygraphr::generate_survey_polarised(m=300, n=15, polarisation=0.25)
getwd()
load("surveygraph/ICSMP_500.RData")
S1

S <- COVID_measures
S[] <- lapply(S, as.numeric)

S <- na.omit(S)

(S[,1])

names1 <- data.frame(id=c(1:length(S[,1])), group=S[,1])
names2 <- data.frame(id=c(1:length(S)))

edgelists <- surveygraphr::graph_edgelists(S)

g1 <- igraph::graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)
g2 <- igraph::graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)


V(g1)$group
V(g1)$color <- ifelse(V(g1)$group == 10, "blue", "red")
V(g1)$color

isolated_nodes1 <- which(degree(g1)==0)
isolated_nodes2 <- which(degree(g2)==0)

g1c <- delete.vertices(g1, isolated_nodes1)
g2c <- delete.vertices(g2, isolated_nodes2)

E(g2c)$label= E(g2c)$weight

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2
     #, layout=layout.fruchterman.reingold
     , main="respondents")
plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="items")








# https://cran.r-project.org/web/packages/essurvey/vignettes/intro_ess.html

# install.packages("devtools")
devtools::install_github("ropensci/essurvey")
install.packages("essurvey")

library(essurvey)
set_email("cillian.mchugh@ul.ie")
#set_email("cillian.miltown@gmail.com")
show_countries()
import_all_cntrounds()
show
show_country_rounds("Turkey")

show_rounds()
one_round <- import_rounds(1)



df <- read.csv("../SurveyGraph/ESS10.csv")

df <- dplyr::sample_n(df,500)

head(df)

table(df$cntry)



ESS_GB_500 <- dplyr::sample_n(
  df[which(df$cntry=="GB"),],
  500)

ESS_500 <- df

save(ESS_500,ESS_GB_500, file="ESS_500.RData")











