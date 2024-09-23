
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Madrid pollution 2023

This project aims to demonstrate some useful techniques of data
processing and analysis to derive trends at quarter level of
neighborhoods of a city out of individual sensor data spread throughout
the city. The most important tools used include interpolation, vectorial
datacube aggregation, and trend time series analysis.

In this case, a Madrid pollution dataset was used (available at the
[Madrid Open Data portal](https://datos.madrid.es/portal/site/egob)).
The dataset is initially provided in a wide format and provides
information for multiple pollutants across 2023.

The main workflow of the project can be accessed at the
`Processing-and-Analysis.md` file in this repository.

As an addition to the main analysis, we leverage the processed data to
implement an LSTM model for the time series data of NO2 of Madrid
throughout year 2023. The workflow for this add-on to the project is
available under the `LSTM Analysis.Rmd` file of the current repository.
