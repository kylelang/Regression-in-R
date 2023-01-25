### Title:    Regression in R: Package Installation Script
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2023-01-25

install.packages(c("MLmetrics",
                   "dplyr",
                   "magrittr",
                   "wec",
                   "psychTools",
                   "DAAG",
                   "rockchalk",
                   "car",
                   "sandwich",
                   "lmtest"),
                 repos = "http://cloud.r-project.org",
                 dependencies = TRUE)

