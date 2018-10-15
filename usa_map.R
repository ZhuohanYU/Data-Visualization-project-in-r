#'@details basically only need the package of plotly, other packages may cause problems
#'with interactive map
#'@export

library(plotly)
library(ggplot2)

df <- read.csv("L:/Pictures/5374-Sagar/usa_map.csv",stringsAsFactors=FALSE,header=TRUE)
df$NumberofInjuries.Illnesses2012[is.na(df$NumberofInjuries.Illnesses2012)]<-48400
df$Injuries.Illnesses2012Rate[which(is.na(df$Injuries.Illnesses2012Rate))]<-3.5
# interactive: hover mouse and present the details, you may add more with html 
# script <br>
df$hover <-
  with(df,
       paste(
         State,
         "<br>",
         "Number of Fatalities:",
         df$NumberofFatalities2012,
         "<br>",
         "<br>",
         "Fatalities Rank in US 2012:",
         StateRankFatalities2012,
         "<br>",
         '<br>',
         "Rate% of Injur/Ill: ",
         Injuries.Illnesses2012Rate,
         "<br>",
         "<br>",
         "Average of Penalties FY2013:",
         PenaltiesFY2013.Average..,
         "<br>",
         "<br>",
         "Rank of Penalties:",
         PenaltiesFY2013.Rank.,
         "<br>",
         "<br>",
         "Number of Inspectors:",
         Inspectors,
         "<br>"
       ))

# set a boundary with black line 
bd <- list(color = toRGB("black"), width = 2)

# set some map features using albersusa 
geo <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = T,
  lakecolor = toRGB('black')
)

# render map by using plot_geo function from plotly
p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(z = ~ RateofFatalities2012,
    text = ~ hover,
    locations = ~ code,
    color = ~ RateofFatalities2012,
    colors = 'Reds') %>%
  colorbar(title = "Fatalities Rate in 2012") %>%
  layout(title = " Work saftly and Health map in US 2012",font=3,titlsize=2,
         geo = geo) 
p

df$hover1 <-
  with(df,
       paste(
         State,
         "<br>",
         "Number of Injur/Ill:",
         NumberofInjuries.Illnesses2012,
         "<br>",
         "<br>",
         "Fatalities Rank in US 2012:",
         StateRankFatalities2012,
         "<br>",
         '<br>',
         "Rate% of Fatalities:",
         RateofFatalities2012,
         "<br>",
         "<br>",
         "Average of Penalties FY2013:",
         PenaltiesFY2013.Average..,
         "<br>",
         "<br>",
         "Rank of Penalties:",
         PenaltiesFY2013.Rank.,
         "<br>",
         "<br>",
         "Number of Inspectors:",
         Inspectors,
         "<br>"
       ))

q<-plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(z = ~ Injuries.Illnesses2012Rate,
            text = ~ hover1,
            locations = ~ code,
            color = ~ Injuries.Illnesses2012Rate,
            colors = 'Blues') %>%
  colorbar(title = "Rate of Injur/Ill in 2012") %>%
  layout(title ="Work saftly and Health map in US 2012",font=3,titlsize=2,
         geo = geo) 
q

df$StateRankFatalities2012<-51-df$StateRankFatalities2012































