#' Make a USA map with all the counties colored in
#'
#' @param df The data frame of the counties in the US and the data to plot
#' @param county_index The numerical index (base 1) of the column which contains
#'   the FIPS code
#' @param plot_index The numerical index (base 1) of the column which contains
#'   the data we want to plot
#' @param save T/F determines if we want to save the plot
#' @param filename If we want to save, where are we saving?
#'
#' @return
#' @export
#'
#' @examples
mapValues <- function(df, county_index=1,plot_index=2,save=TRUE,filename="./usa_plot.png",titleString="default plot"){

  data(county.fips)
  county.fips$polyname <- as.character(county.fips$polyname)
  stateCounty <-matrix(data=unlist(strsplit(county.fips$polyname,",")),ncol=2,byrow=T)
  county.fips$state <-stateCounty[,1]
  county.fips$county <- stateCounty[,2]
  county.fips <- county.fips[,-2]

  map.county <-map_data('county')
  shape_data <-
    merge(
      y = map.county,
      x = county.fips,
      by.x = c("state", "county"),
      by.y = c("region", "subregion")
    )


  plot_data <-
    merge(x = shape_data,
          y = df[, c(county_index, plot_index)],
          by.x = c("fips"),
          by.y=names(df)[county_index])

  plot_data2 <- plot_data[order(plot_data$order),]
  fillString <- names(df)[plot_index]

  ggplot(plot_data2,aes_string(x="long",y="lat",group="group",fill=fillString)) +
    geom_polygon()+ coord_map() +
    ggtitle(titleString)


}
