# Philly Election Explorer
A way to explore Philadelphia elections.  Presents an interactive choropleth map of the strength of a candidates support in each division and ward in Philly.  The darker the precinct, the higher the support.

## Election Results
Resulsts were published here and downloaded into this project: http://phillyelectionresults.com/

## Map
The ward/division shapefile came from the open data philly website: https://www.opendataphilly.org/dataset/political-ward-divisions

## Where can I see this?
https://hollander.shinyapps.io/2018-5-Philly-Primary/

## Other
Created for no other reason than to learn more about leaflet, shiny, election resulsts, etc...  I'm open to someone coming up with other ways to explore this data. In order to change to a new election, you simply have to download new election resulsts from the website above and change the file that is being read in helper.R.  In the future, I hope to allow file uploads.
