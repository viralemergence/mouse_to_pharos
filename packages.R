## store your packages here

## core packages
library(targets)
library(tarchetypes)
library(renv)
library(dotenv)
library(conflicted) # make sure we know where function calls are coming from and which is preferred. See .Rprofile file for default preferences


## packages necessary for startup script
library(here)
library(usethis)

## recommended
library(fnmate) # quickly create functions renv::install("milesmcbain/fnmate")
library(tflow) # allows for some nice targets add-ins renv::install("milesmcbain/tflow")


## for pharos
library(dplyr)
library(wddsWizard) # renv::install("viralemergence/wddsWizard")
library(jsonvalidate)
library(janitor)
