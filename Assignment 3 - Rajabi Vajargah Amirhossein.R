
library("tidyverse")


##question 2
division<-read_delim("/cloud/project/raw/taxdmp/division.dmp",delim = "\t",col_names = FALSE)%>% na_if(.,"|")%>%Filter(function(x)!all(is.na(x)),.)
division_tibble<-as_tibble(division)
names(division_tibble)<- c("division_id","division_cde","division_name", "comments")
division_tibble
View(division_tibble)

nodes<-read_delim("/cloud/project/raw/taxdmp/nodes.dmp",delim = "\t",col_names = FALSE)%>% na_if(.,"|")%>%Filter(function(x)!all(is.na(x)),.)
nodes_tibble<-as_tibble(nodes)
names(nodes_tibble)<-c("tax_id","parent_tax_id","rank","embl_code","division_id","inherited_div_flag","genetic_code_id","inherited_GC_flag","mitochondrial_genetic_code_id","inherited_MGC_flag","GenBank_hidden_flag","hidden_subtree_root_flag","comments")
nodes_tibble
View(nodes_tibble)

names<- read_delim("/cloud/project/raw/taxdmp/names.dmp",delim="|",col_names=FALSE,trim_ws=TRUE)%>%Filter(function(x)!all(is.na(x)),.)
names_tibble<-as_tibble(names)
names(names_tibble)<-c("tax_id","name_txt","unique_name","name_class")
View(names_tibble)

##question 3

nodes_tibble%>% select(-c("genetic_code_id","mitochondrial_genetic_code_id","embl_code","comments"))%>%select(-contains("flag")) -> refined_nodes
refined_nodes
View(refined_nodes)

division_tibble%>%select(-c("comments"))-> refined_division
refined_division
View(refined_division)

names_tibble%>% filter(name_class=="scientific name")%>%select(-c(name_class))-> refined_names
refined_nodes
View(refined_names)


#question 6
q6_tibble <- full_join(refined_nodes,refined_names)
q6_tibble
View(q6_tibble)



#question 7
q7_tibble <- left_join(q6_tibble,refined_division)
q7_tibble
View(q7_tibble)


#question 8

my_function8 <- function(any_tibble,x){
  any_tibble <- select(any_tibble,name_txt,tax_id)
  any_tibble <- filter(any_tibble,name_txt==x)
  any_tibble <- select(any_tibble,tax_id)
  any_tibble <- str_c(any_tibble)
  return(any_tibble)
  
}

my_function8(q6_tibble,"Mus musculus")


#question 9

my_function9 <- function(anyy_tibble,x){
  anyy_tibble <- filter(anyy_tibble,parent_tax_id==x)
  return(anyy_tibble)
}

my_function9(q6_tibble,"10090")

#question 10
#for this question I use function of question 11 and then filter for species and genus

my_function11(q6_tibble,"2157") -> archaea_tibble
archaea_tibble%>%filter(rank=="species")%>%nrow()
archaea_tibble%>%filter(rank=="genus")%>%nrow()

#the code to determine the species and genus in bacteria
my_function11(q6_tibble,"2") -> bacteria_tibble
bacteria_tibble%>%filter(rank=="species")%>%nrow()
bacteria_tibble%>%filter(rank=="genus")%>%nrow()

#the code to determine the species and genus in eukarya
my_function11(q6_tibble,"2759") -> eukaryota_tibble
eukaryota_tibble%>%filter(rank=="species")%>%nrow()
eukaryota_tibble%>%filter(rank=="genus")%>%nrow()


#question 11

my_function11 <- function(my_tibble,y){
children_to_explore<-c(y)

direct_desc <- filter(q6_tibble,parent_tax_id==children_to_explore[1])
children_to_explore <- append(children_to_explore,direct_desc$tax_id)

for(i in 2:length(children_to_explore)){
  loop_tibble <- filter(q6_tibble,parent_tax_id%in%children_to_explore)
  children_to_explore <- append(children_to_explore,loop_tibble$tax_id)
}

answer <- filter(q6_tibble,tax_id%in%children_to_explore)
return(answer)
}

my_function11(q6_tibble,"2157")

#question asks for the division so we can find tax_id of each division (either it be phylum, kingdom or etc...)
#and then put it in the second elemnt of the function and it will give out the tibble consisting of taxa in that division


#question 12

path_to_root <- function(input_tibble,z) {
  
  t1 <- filter(q6_tibble,tax_id==z)
  t1.1 <- c()
  t1.1<- append(t1.1,t1$parent_tax_id)
  t1.2 <- str_c(t1$name_txt)
  while(print(t1.2)!="root"){
    t1 <- filter(q6_tibble,tax_id==t1.1[1:length(t1.1)])
    t1.1 <- c()
    t1.1<- append(t1.1,t1$parent_tax_id)
    t1.2 <- str_c(t1$name_txt)
    
  }
}

path_to_root(q6_tibble,"10090")




