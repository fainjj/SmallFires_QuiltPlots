# Credits ####
# Author:  Justin J. Fain
# Date:    27 March 2019
# Scope: Contig. US (CONUS), 2017
#
# This script is designed to process CONUS fire data, associate the fires with
# landcover types dervied from four different landcover products, and generate
# quilt plots (tiled heatmaps) to assess agreement among the products.
#
#

# Packages & Setup ####
require(tidyverse)
require(lubridate)
require(ggplot2)
require(scales)
require(here)

# Use the 'here' function from the here package, not lubridate::here
here <- here::here

# Use this to set your project directory. This directory must contain the data
# folder distributed with this script.
here::set_here()

# Data & Tables ####

# Study area fire records
ks <- read_csv(here('Data', 'Study_Areas', 'Kansas_2017_Sel_ALL.csv'))
ar <- read_csv(here('Data', 'Study_Areas', 'Arkansas_2017_Sel_ALL.csv'))
fl <- read_csv(here('Data', 'Study_Areas', 'Florida_2017_Sel_ALL.csv'))

# Table to relate CDL and IGBP codes
lookup.table <- read_csv(here('Data', 'CDLcodesIGBP.csv'))
# Change NA values to match MODIS/VIIRS
lookup.table$IGBPcode[lookup.table$IGBPcode == 999] <- -9999
# Names for IGBP code classes
names.lut <- read_csv(here('Data', 'IGBPClassNamesLUT.csv'))
# Also add a class for "Undefined"
names.lut <- rbind(names.lut, data.frame(Code = -9999, IGBP_Class_Name = 'Undefined'))

# 2017 fires for CONUS
data17 <- list.files(here('Data'), pattern = '[[:upper:]].*2017', full.names = TRUE) %>%
  lapply(read_csv) %>%
  {do.call(rbind, .)}

# Create New Variables ####

# Function to extract and add a month column
add_months <- function(df){df <- mutate(df, Month = month(as.Date(df$ACQ_DATE)))}
# Apply months to all fire data sets
data17 <- add_months(data17)
ks <- add_months(ks)
ar <- add_months(ar)
fl <- add_months(fl)

# Plots ####
# Big nasty function for generating quilt plots
quilt.fires <- function(fire.data){

  # Function to extract and add IGBP from lookup table
  add.cdl.igbp <- function(CDLcode){
    lookup.table[lookup.table$Code == CDLcode, ]$IGBPcode
  }

  # Function to extract and add IGBP class names from lookup table
  add.igbp.name <- function(CDLcode){
    names.lut[names.lut$Code == CDLcode, ]$IGBP_Class_Name
  }

  # Assuming this is a single-month plot, get the name of the month
  this.month <- month.name[as.numeric(unique(fire.data$Month))]

  # Crop data layer
  cdl <- sapply(fire.data$CDLcode, add.cdl.igbp, simplify = 'vector')

  # Add cdl info to fire data
  fire.data <- mutate(fire.data, CDL_IGBP = cdl)

  # IGBP class names
  igbp.name <- sapply(fire.data$CDL_IGBP, add.igbp.name, simplify = 'vector')

  # Add class names to fire data
  fire.data <- mutate(fire.data, IGBP_Name = igbp.name)

  # Get counts of landcover types for each of the landcover products
  viirs <- table(fire.data$VIIRS) %>% as.data.frame() %>% mutate(Product = 'VIIRS (1 km)')
  m12q1 <- table(fire.data$MCD12Q1) %>% as.data.frame() %>% mutate(Product = 'MCD12Q1 (500 m)')
  m12c1 <- table(fire.data$MCD12C1) %>% as.data.frame() %>% mutate(Product = 'MCD12C1 (5.6 km)')
  cdl   <- table(fire.data$CDL_IGBP) %>% as.data.frame() %>% mutate(Product = 'CDL (30 m)')

  # Combine counts for easy plotting
  fire.counts <- rbind(viirs, m12c1, m12q1, cdl)

  # Prefered order of landcover types on the plot
  igbp_code_order <- c(17,16,15,13,12,14,11,10,9,8,7,6,1,2,3,4,5,-9999)
  # Convert codes to names
  igbp_code_order <- sapply(igbp_code_order, add.igbp.name, simplify = 'vector')
  # Set column names
  colnames(fire.counts) <- c('Landcover Type', 'Frequency', 'Product')
  # Apply IGBP classification names to fire counts
  fc.names <- sapply(fire.counts$`Landcover Type`, add.igbp.name, simplify = 'vector')
  fire.counts <- mutate(fire.counts, `Landcover Type` = fc.names)
  # Set the order in which the products should be displayed
  fire.counts$Product <- factor(fire.counts$Product,
                                levels = rev(c('CDL (30 m)', 'MCD12Q1 (500 m)', 'VIIRS (1 km)', 'MCD12C1 (5.6 km)')))
  # Use IGBP code order to enforce plotting order
  fire.counts$`Landcover Type` <- factor(fire.counts$`Landcover Type`,
                                         levels = igbp_code_order)


  fire.counts <- complete(fire.counts, Product, `Landcover Type`, fill = list(Frequency = NA))

  # Here is where the actual quilt plot happens
  quilt <- ggplot(data = fire.counts, mapping = aes(`Landcover Type`, Product)) +
    geom_tile(mapping = aes(fill = Frequency), color = 'white') +
    labs(fill = 'Fire Frequency') +
    scale_fill_gradient(low = muted('green'), high = "mediumvioletred", na.value = 'grey70') +
    scale_x_discrete(position = 'top') +
    theme_grey() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0),
          axis.title.x = element_blank(), panel.grid.major = element_blank()) +
    ggtitle(this.month)

  # Finally return the plot
  return(quilt)

}

# Store monthly plots as lists. Months with no fires are NULL.
# I've expanded this first one so you can see all of the parts
monthly.quilts.conus <- lapply(1:12, # Months are passed as numbers
                               function(month.num){
                                 tryCatch( # Handle months with no fires
                                   quilt.fires(data17 %>% # Apply the quilting function
                                                 filter(Month == month.num)),
                                   error = function(e) NULL # Control value of non-plots in list
                                   )
                                 }
                               )

# Plots for Kansas
monthly.quilts.ks <- lapply(1:12, function(month.num){tryCatch(quilt.fires(ks %>% filter(Month == month.num)), error = function(e) NULL)})
# Plots for Arkansas
monthly.quilts.ar <- lapply(1:12, function(month.num){tryCatch(quilt.fires(ar %>% filter(Month == month.num)), error = function(e) NULL)})
# Plots for Florida
monthly.quilts.fl <- lapply(1:12, function(month.num){tryCatch(quilt.fires(fl %>% filter(Month == month.num)), error = function(e) NULL)})

# Make indexing these lists easier by naming each element
names(monthly.quilts.conus) <- month.name
names(monthly.quilts.ks)    <- month.name
names(monthly.quilts.ar)    <- month.name
names(monthly.quilts.fl)    <- month.name
