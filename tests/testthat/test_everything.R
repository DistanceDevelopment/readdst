# this just tests all the projects that we ship with readdst
# we expect everything to work!

# do a pretty printer...
this_project <- system.file("Stratify-solutions", package="readdst")
this_project <- paste0(this_project, "/Stratify\\ solutions")

#for(this_project in projects){
context(this_project)

cc <- convert_project(this_project)
for(i in seq_along(cc)){


  result <- test_stats(cc[[i]], 1:2)

  test_that(names(cc)[i],{
    for(j in 1:nrow(result)){
      expect_equal(result$Pass[j], "✓", info=result$Statistic[[j]])
    }
  })
}


