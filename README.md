# ProjectShowcase
This repository is for Showcasing purposes only. Embedded SQL is not intended to pull from a sample dataset or other database. 
Repo consists of two files: Helper.R and app.R. <br />
Helper.R is the feeder file needed to run app.R, a reactive Shiny dashboard to designed pull certain Mutual Fund statistics upon the users request.
Objective:
Create an application that can be accessed via a user’s browser that creates a blended portfolio of user-specified Mutual Funds and calculates various aggregated metrics.
Helper.R:
Use odbcConnect to initiate connection to internal database. The stringr package is a set of wrappers to help embed SQL within the R syntax. The objective is to extract various returns and key identifiers to drive downstream analytics.
Data cleaning and manipulation techniques are used with a combination of arguments from the data.table and dplr libraries, while the pipe (%>%) used is from the magrittr package. A helpful way to read the pipe operator is to read it as “and then”. So, for example, 
mutate(return_code = as.character(return_code)) %>% bind_rows(FundQuery) can be translated as, create a character vector called return_code “and then” append the vector (row-wise) to the existing object FundQuery.
Make_blend is a function that combines all respective Mutual Funds. A weakness of the function is that it requires all funds to have the same amount of history. Example: If all funds have 15-year returns, except for one, then the blended portfolio will not be built.
Any other metric that will be calculated is derived from functions built in this file.
App.R:
Built for users that need to extract and view risk-return metrics of portfolio data, based upon user input. There is little ability to define expectation about what weights or funds a user may want with respect to building a portfolio, this application puts the flexibility in their hands.
The user can update fund identifiers and weights within a csv file, and load it into the shiny app. After the user select calculate, they are able to view and download the summary statistics for their personal use.
