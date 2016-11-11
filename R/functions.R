library(tidyverse)
library(readxl)
library(maptools)
library(stringr)

process_tarrant <- function(path_to_excel = "data-raw/tarrant/tarrant_nov9.xlsx") {
  
  # Issue - weird file mismatch.  Open in Excel and save as "tarrant_nov9.xlsx"
  df <- read_excel(path_to_excel, sheet = 26, skip = 2)
  
  # Keep the Precinct and total votes columns (every 4th col starting w/col 6)
  # plus overall Total
  cols <- c(1, seq(6, 70, 4), 71)
  
  sub <- df[,cols]
  
  nms <- c("precinct", "trump", "clinton", "johnson", "stein", 
           paste0("other", 1:13), "total")
  
  names(sub) <- nms
  
  sub$precinct <- paste0("439", sub$precinct)
  
  # Sum up the "other" columns
  sub$other <- rowSums(sub[6:18])
  
  sub1 <- sub %>%
    select(precinct:stein, other, total) %>%
    filter(precinct != "439Total:")
  
  return(sub1)

}


process_dallas <- function(path_to_excel = "data-raw/dallas/dallas_nov9.xlsx") {
  
  # Read in the sheet
  df <- read_excel(path_to_excel, sheet = 4, skip = 2)
  
  # Keep precinct and totals for the four candidates
  cols <- c(1, seq(8, 26, 6))
  
  sub <- df[,cols]
  
  names(sub) <- c("precinct", "trump", "clinton", 
                  "johnson", "stein")
  
  # Clean up the "precinct" column; will require group sums
  
  # Retain the number before the dash
  sub1 <- sub %>%
    filter(precinct != "Total:") %>%
    mutate(precinct = paste0("113", 
                             str_sub(precinct, 1, 4))) %>%
    group_by(precinct) %>%
    summarize(trump = sum(trump), 
              clinton = sum(clinton), 
              johnson = sum(johnson), 
              stein = sum(stein))
  
  return(sub1)
  
}


process_denton <- function(path_to_excel = "data-raw/denton/denton_nov9.xlsx") {
  
  # Read in the sheet
  df <- read_excel(path_to_excel, sheet = 4, skip = 2)
  
  cols <- c(1, seq(6, 18, 4))
  
  sub <- df[,cols]
  
  names(sub) <- c("precinct", "trump", "clinton", 
                  "johnson", "stein")
  
  sub1 <- sub %>%
    filter(precinct != "Total:") %>%
    mutate(precinct = paste0("121", precinct))
  
  return(sub1)
}


# Some info about Collin - there are some new precincts
# not in the shapefile.  They are precincts 210-214.  
# 
# 210: was part of 25
# 211: was part of 38
# 212: was part of 134
# 213: was part of 163
# 214: was part of 178

process_collin <- function() {
  
  trump <- read_csv("data/tabula-collin_trump1.csv")
  
  trump_total <- trump %>%
    rename(X1 = `Jurisdiction Wide`) %>%
    filter(X1 == "Total" | grepl("PCT", X1)) %>%
    select(X1, X9) %>%
    mutate(trump = lead(X9)) %>%
    filter(X1 != "Total") %>%
    select(precinct = X1, trump) %>%
    mutate(trump = as.integer(str_replace(trump, "\\s[^ ]+$", "")))
  
  other <- read_csv("data/tabula-collin_other1.csv") %>%
    rename(X3 = `President/Vice-President`) %>%
    filter(X1 == "Total" | grepl("PCT", X1)) %>%
    select(X1:X4) %>%
    # Shift over problem rows
    mutate(X3 = ifelse(is.na(X3), X4, X3)) %>%
    select(-X4) %>%
    mutate(clinton_temp = lead(X2), 
           js_temp = lead(X3)) %>%
    filter(X1 != "Total") %>%
    mutate(clinton = as.integer(str_replace(clinton_temp, 
                                            "\\s[^ ]+$", "")), 
           johnson = as.integer(sub(" .*", "", js_temp)), 
           stein_temp = str_sub(js_temp, start = -8, end = -1)) %>%
    mutate(stein = as.integer(str_sub(stein_temp, 1, 2))) %>%
    select(precinct = X1, clinton, johnson, stein)
  
  full <- left_join(trump_total, other, by = "precinct") %>%
    # Group new precincts together to correspond with shapes
    mutate(precinct = ifelse(precinct == "PCT 210", "PCT 25", 
                             ifelse(precinct == "PCT 211", "PCT 38", 
                                    ifelse(precinct == "PCT 212", "PCT 134", 
                                           ifelse(precinct == "PCT 213", "PCT 163", 
                                                  ifelse(precinct == "PCT 214", "PCT 178", precinct)))))) %>%
    group_by(precinct) %>%
    summarize_all(sum) %>%
    ungroup() %>%
    mutate(precinct = paste0("85", str_pad(str_replace(precinct, "PCT ", ""), width = 4, 
                                           side = "left", pad = "0")))
  
  
    return(full)
    
  
}

process_rockwall <- function(path_to_excel = "data-raw/rockwall/rockwall_nov10.xlsx") {
  
  df <- read_excel(path_to_excel, sheet = 4, skip = 2)
  
  cols <- c(1, seq(8, 32, 6), 33)
  
  sub <- df[,cols]
  
  names(sub) <- c("precinct", "trump", "clinton", "johnson", 
                  "stein", "other", "total")
  
  sub1 <- sub %>%
    filter(precinct != "Total:") %>%
    mutate(precinct = paste0("397", str_pad(precinct, 4, "left", "0")))
  
  return(sub1)
  
  
}
  



votes_to_xy <- function(vtds, scaling_factor = 1) {
  
  values <- c("trump", "clinton", "johnson", "stein")
  
  xys <- bind_rows(
    lapply(values, function(value) {
      
      vtds[[value]] <- as.integer(vtds[[value]] / scaling_factor)
      
      dots <- dotsInPolys(vtds, vtds[[value]])
      
      proj4string(dots) <- proj4string(vtds)
      
      dots_xy <- spTransform(dots, CRS("+proj=longlat +datum=WGS84"))
      
      dots_xy$longitude <- coordinates(dots_xy)[,1]
      dots_xy$latitude <- coordinates(dots_xy)[,2]
      
      dots_xy$candidate <- value
      
      return(dots_xy@data)
      
    })
  )
  
}

