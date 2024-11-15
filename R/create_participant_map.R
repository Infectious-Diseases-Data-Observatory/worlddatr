#' Create a cloropleth map of the number of participants in a dataset
#'
#' @param DATA_DM Data frame with includes the column "COUNTRY", listing the 3
#'   letter ISO country code where the participant is located in. The
#'   Demographics (DM) domain in SDTM and IDDO-SDTM for example.
#' @param include_ATA Boolean. Should Antarctica be included on the map? Default
#'   is `FALSE`
#' @param include_n Boolean. Should `n = X` where X is the number of
#'   participants, be included in the map. Default is `TRUE`
#' @param title Character. Title of the choropleth map.
#' @param subtitle Character. Subtitle of the choropleth map.
#' @param legend Character. Legend title of the choropleth map.
#' @param colour_high Colour of the high end of the scale, either using the name
#'   of a base colour or the HEX code, i.e. "red" or "#F9250C. This will be the
#'   colour of the country with the highest value. Default is IDDO-branded blue
#'   "#14B1E7"
#' @param colour_low Colour of the low end of the scale, either using the name
#'   of a base colour or the HEX code, i.e. "red" or "#F9250C. This will be the
#'   colour of the country with the lowest value. Default is IDDO branded blue
#'   at 30% tint, "#CCECF9"
#' @param colour_default Colour of the other countries without participant data,
#'   either using the name of a base colour or the HEX code, i.e. "red" or
#'   "#F9250C. Default is IDDO branded red at 5% tint, "#FDF3F4"
#' @param colour_borders Colour of the country borders, either using the name of
#'   a base colour or the HEX code, i.e. "red" or "#F9250C. Default is "black"
#' @param colour_background Colour of the plot background, either using the name
#'   of a base colour or the HEX code, i.e. "red" or "#F9250C". Default is
#'   "#FFFFFF" (white).
#' @param scale_breaks Numeric list. Specify custom scale breaks for the legend,
#'   i.e. c(100, 500, 1000, 2000). Default is pretty_breaks from the scales
#'   package.
#' @param log_scale Boolean. Should the number of participants be transformed
#'   into the log scale for when the distribution of values is very uneven.
#'   Default is `FALSE`.
#'
#' @return A cloropleth map displaying the number of participants in each
#'   country
#'
#' @export
#'
#' @author Rhys Peploe
#'
#' @examples
#' library(worlddatr)
#' set.seed(123) # for reproducibility
#'
#' countries <- sample(world_income$alpha_3_code, 100, replace = FALSE)
#'
#' probabilities <- runif(length(countries))
#' probabilities <- probabilities / sum(probabilities)
#'
#' country_data <- data.frame(COUNTRY = sample(countries, 10000, replace = TRUE, prob = probabilities))
#'
#' create_participant_map(DATA_DM     = country_data,
#'                        include_ATA = FALSE,
#'                        include_n   = TRUE,
#'                        title       = "Number of Participants in Studies across Countries",
#'                        subtitle    = "Data from studies registered between 2000 - 2024",
#'                        colour_high = "#14B1E7",
#'                        colour_low  = "#CCECF9",
#'                        colour_default = "#FDF3F4",
#'                        colour_borders = "black",
#'                        colour_background = "#FFFFFF",
#'                        scale_breaks = pretty_breaks(),
#'                        log_scale = FALSE
#'                        )
#'
create_participant_map <- function(DATA_DM,
                                   include_ATA = FALSE, include_n = TRUE,
                                   title = "", subtitle = "", legend = legend,
                                   colour_high = "#14B1E7", colour_low = "#CCECF9",
                                   colour_default = "#FDF3F4", colour_borders = "black",
                                   colour_background = "#FFFFFF",
                                   scale_breaks = pretty_breaks(), log_scale = FALSE){
  GROUP_DM = DATA_DM %>%
    group_by(COUNTRY) %>%
    summarise(n_participants = n()) %>%
    ungroup()

  sum_participants = sum(GROUP_DM$n_participants)

  world_map_DM = GROUP_DM %>%
    right_join(world_map,
               by = c("COUNTRY" = "alpha_3_code"))

  if(include_ATA == FALSE){
    world_map_DM = world_map_DM %>%
      filter(COUNTRY != "ATA")
  }

  if(log_scale == TRUE){
    world_map_DM = world_map_DM %>%
      mutate(n_participants = log(n_participants))
  }

  plot = ggplot(world_map_DM, aes(x = long, y = lat, group = group)) +
    geom_polygon(colour = colour_borders, fill = colour_default, linewidth = 0.01) +
    geom_polygon(data = world_map_DM %>%
                   filter(!is.na(n_participants)),
                 mapping = aes(x = long, y = lat, group = group, fill = n_participants),
                 alpha = 0.85, colour = colour_borders, linewidth = 0.01) +
    scale_fill_gradient(high = colour_high, low = colour_low,
                        breaks = scale_breaks) +
    theme(panel.background = element_rect(fill = colour_background),
          plot.background = element_rect(fill = colour_background),
          panel.grid = element_blank(),
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.key.height = unit(1, 'cm')) +
    labs(fill = legend,
         title = title,
         subtitle = subtitle)

  if(include_n == TRUE){
    plot = plot +
      annotate("text", x = 165, y = -60, label = str_c("n = ", sum_participants))
  }

  return(plot)
}
