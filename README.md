# neissviz
NEISS Injury Data Explorer

Live application found at https://viriya.shinyapps.io/final/
Currently not working because memory load is too high for a free shiny account.

Download the app.R file, unzip and follow these instructions. 

Install R and  R Studio. Load the app.R file, then click Run App and the application will run as a pop-up which you can then optionally open in a browser.
If the app does not run, it's most likely bcause you do not have the required packages installed.
Install the required packages by running `install.packages("package_name")` with the package_name in parenthesis.

For the neiss package, extra steps are needed.
`install.packages("devtools")` and then `devtools::install_github("hadley/neiss")`.
Note that this might take a while to download because of the big amount of data.
