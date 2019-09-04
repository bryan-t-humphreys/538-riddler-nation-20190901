
data_dist <- read.csv("/Desktop/riddler.csv", stringsAsFactors=FALSE)


# Functions
war_battle <- function(my_dist, challenger_dist)
{
     my_points <- 0
     challenger_points <- 0
     differences <- numeric()
     
     for(i in 1:10)
     {
          challenger_i = as.numeric(challenger_dist[i])
          my_i = as.numeric(my_dist[i])
          
          if( is.na(challenger_i))
          {
               challenger_i <- 0         
          }
          differences[i] <- my_i - challenger_i
          
          if(my_i > challenger_i)
          {
               my_points <- my_points + i
          }
          else if(my_i < challenger_i)
          {
               challenger_points <- challenger_points + i
          }
          else if(my_i == challenger_i)
          {
               challenger_points <- challenger_points + i/2
               my_points <- my_points + i/2
          }
     }
     
     output <- list()
     output$my_points <- my_points
     output$challenger_points <- challenger_points
     output$differences <- differences
     
     return(output)     
}


fight_all_wars <- function(my_dist, data_dist)
{
     my_wins <- 0
     challenger_wins <- 0
     ties <- 0
     results_list <- numeric()
     
     for(j in 1:nrow(data_dist))
     {
          
          challenger <- as.numeric(data_dist[j, 1:10])
          
          fight <- war_battle(my_dist, challenger)
          
          my_points <- fight$my_points
          challenger_points <- fight$challenger_points
          if(j == 1)
          {
               differences <- fight$differences
          }
          else
          {
               differences <- rbind(differences, fight$differences)
          }
          
          
          if(my_points > challenger_points)
          {
               my_wins <- my_wins + 1
               results_list[j] <- 1
          }
          else if(my_points < challenger_points)
          {
               challenger_wins <- challenger_wins + 1
               results_list[j] <- -1
          }
          else if(my_points == challenger_points)
          {
               ties <- ties + 1
               results_list[j] <- 0
          }
     }
     
     output <- list()
     output$my_wins <- my_wins
     output$challenger_wins <- challenger_wins
     output$ties <- ties
     output$differences <- differences
     output$results <- results_list
     return(output)
}


# Optimize
for(z in 1:500)
{
     FAW <- fight_all_wars(my_dist, data_dist)
     
     indices = sample(1:10,2)
     subtract_index = indices[1]
     add_index = indices[2]
     
     my_dist_new = my_dist
     
     test <- sample(1:3, 1)
     
     if(my_dist[subtract_index] > (test-1) & my_dist_new[add_index] < (100-test))
     {
          
          my_dist_new[subtract_index] = my_dist[subtract_index] - test
          my_dist_new[add_index] = my_dist[add_index] + test
     }
     
     FAW_NEW <- fight_all_wars(my_dist_new, data_dist)
     
     if(FAW_NEW$my_wins >= FAW$my_wins)
     {
          my_dist = my_dist_new
          
          print(FAW_NEW$my_wins)
     }
     

}


boxplot(FAW$differences)
abline(h = 0)
