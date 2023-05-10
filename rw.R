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