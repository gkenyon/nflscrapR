generate_lineup <- function(n){
  
  pred_sal <- proj %>% 
    filter(avg_type=='robust') %>%
    mutate(Name = ifelse(pos=="DST", last_name, paste(first_name, last_name))) %>%
    inner_join(sal, by=c("Name")) %>%
    select(Name, team, position, points, Salary, sd_pts) %>%
    filter(!is.na(points), !is.na(Salary)) %>%
    group_by(Name) %>%
    mutate(sal_max=max(Salary)) %>%
    filter(Salary==sal_max) %>%
    group_by(Name) %>%
    mutate(pts_pred = rnorm(1, points, sd_pts),
           lineup=n) %>%
    select(-sal_max)
  
  
  obj <- pred_sal$pts_pred
  
  mat <- rbind(t(model.matrix(~ position + 0,pred_sal)), t(model.matrix(~ position + 0,pred_sal)), rep(1, nrow(pred_sal)), pred_sal$Salary)
  
  dir <- c("=","=","<=","<=","<=", "=","=",">=",">=",">=","=","<=")
  
  rhs <- c(1,1,3,2,4,1,1,2,1,3,9,50000)
  
  result <- lp("max", obj, mat, dir, rhs, all.bin = TRUE)   
  
  results <- pred_sal[which(result$solution == 1),]
  
  return(results)
}
