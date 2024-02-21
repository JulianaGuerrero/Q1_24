## Peer code review
## transportation cost analysis - master script
## Author: Juliana Guerrero
## Date: 02/21/2024

## Purpose: Run master script to
## Identify relevant factors that influence the cost of transportation

# remove all objects
rm(list = ls())

## load packages
options(scipen = 999)
library(tidyverse)
library(ggplot2)
library(fixest)

## set file paths for analysis and scripts
dropbox='C:/Users/wb605157/Dropbox'
github ='C:/Users/wb605157/DIME/GitHub/peer_code_review/Q1_24'

## running scripts
source(file.path(github,'cleaning_transformation.R'))
source(file.path(github,'models.R'))
