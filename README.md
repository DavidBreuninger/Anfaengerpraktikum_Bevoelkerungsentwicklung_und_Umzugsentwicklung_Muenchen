# Praktikum

Author: Ferdinand Balser, Wenyu Lou, Duc Pham, David Breuninger

## Introduction

## Usage

We use R version 4.5.3 for this project. Some packages may need the latest R version.

To start, please source the "setup.R" file, this file will install and load all needed packages.

After that you can run the individual r files.

## Directory Structure

### Data

Contains the original data sets used for the project.

The data set "Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv" was downloaded from https://opendata.muenchen.de/dataset/indikatorenatlas-bevoelkerung-mobilitaetsziffer-83r65mct
and the data set "Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv" was downloaded from https://opendata.muenchen.de/dataset/indikatorenatlas-bevoelkerung-bevoelkerungsdichte-83r65mct
Both were downloaded on the 03.03.2026.

The data set "vablock_stadtbezirk.csv" was downloaded from https://opendata.muenchen.de/dataset/vablock_stadtbezirke_opendata.

The folder "Exceldateien Jahrbuch" contains several Excel sheets from 2005 to 2024, these ones were provided to us.

### Programs

Contains r files, which create the figures, saved in the "Results" directory.

The file "data_cleansing.R" contains code cleaning the initial data sets.

The file "functions.R" contains functions that were mainly written to clean the data sets.

### Clean_Data

Contains data sets resulting from the data cleansing step. 

### Results

This directory contains all the figures of the analysis. The figures were saved in jpg format.