library(doParallel)
library(tidyverse)
library(drake)

rbind_with_names <- function(...){
    bound_rows <- rbind(...)
    colnames(bound_rows) <- c("var","median","index")
    bound_rows
}

plan.a <- drake_plan(
    mat = foreach(i=1:1000, .combine = cbind) %do% {
    rnorm(1000,mean=i,sd=sqrt(i))
    },
    medians = foreach(col=iter(mat, by = "column"), .combine = "c") %do% {
        median(col)
    }
)

plan.b <- drake_plan(
    sim_var = 
        foreach(col=iter(mat, by = "column"), m=iter(medians),i=icount(),.combine = rbind_with_names)  %do% 
        {
            c(var(col),m,i)
        },
        abs_var_diff = abs(sim_var[,1] - sim_var[,3])
)

plan <- bind_rows(plan.a,plan.b)

config <- drake_config(plan)
vis_drake_graph(config)

make(config=config)



