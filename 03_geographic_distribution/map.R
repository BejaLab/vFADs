suppressMessages(library(dplyr))
library(maps)
library(ggplot2)
library(ggalt)

xlims = c(-155, 70)
ylims = c(-50, 50)

#order df by station in ascending order and count in descending order
df.des.counts <- read.csv("../data/map_df.tsv", sep = "\t") %>% tbl_df

df.des.counts$Station <- as.factor(df.des.counts$Station)
df.des.counts$family <- as.factor(df.des.counts$family)
df.des.counts$Region <- factor(df.des.counts$Region, levels=c("SPO","NPO","IO","SAO","NAO","MS","RS","SO"))

# df to draw negative stations
df.neg_map <- df.des.counts %>% filter(rpkm == 0) %>% group_by(Station) %>% filter(n()>1) %>% select(-family) %>% distinct()

# df to delineate the oceanic provinces
df.regions <- df.des.counts  %>% select(-family)  %>% select(-rpkm) %>% distinct()

##################################################################################################################

color_continent = "#e6e6e6"
color_boundaries = color_continent

wrld <- map_data("world")

p <- ggplot()
p <- p + theme(panel.background = element_rect(fill =NA),
               panel.border = element_rect(colour = "#000000",size = 1,linetype = "solid",fill =NA),
               axis.title = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_text(),
               axis.ticks.y = element_line(),
               legend.position="bottom",
               legend.background = element_rect(fill="white", colour = "black"),
               legend.key = element_rect(fill=NA))

#Draws the map and assigns background color
p <- p + geom_polygon( data=wrld, aes(x=long, y=lat, group = group),colour=color_continent, fill=color_continent )

# color scale
cols <- c("desC_fam2" = "#7137C8", "desC_fam1" = "#a0892c",
          "SPO" = "pink","NPO" = "green",
          "IO" = "orange",
          "SAO" = "blue","NAO" = "green",
          "MS" = "yellow",
          "RS" = "blue")

#Plots negative stations
neg_map <- p + geom_point( data=df.neg_map %>% filter(rpkm == 0),
                         shape = 21,
                         colour = "#a7a7a7",
                         fill = "#a7a7a7",
                       colour="black",fill="black",
                         size = 1,
                         aes(x=Longitude, y=Latitude)
)
#Plots positive stations fam1, plot separately to manually shift 0.5 long and lat the overlaying positive fam2 stations
map <- neg_map + geom_point( data=df.des.counts %>% filter(rpkm > 0) %>% filter(family == "desC_fam1"),
                          shape = 21,
                         size = 2,
                          aes(x=Longitude, y=Latitude, colour=family, fill=family)
)

#Plots positive stations fam2
map <- map + geom_point( data=df.des.counts %>% filter(rpkm > 0) %>% filter(family == "desC_fam2"),
                             shape = 21,
                             size = 2,
                             aes(x=Longitude+.5, y=Latitude+.5, colour=family, fill=family)
) + scale_colour_manual(values = cols) + scale_fill_manual(values=cols)


map <- map + coord_fixed(xlim = xlims, ylim = ylims)

rmap <- map + geom_encircle(data=df.regions %>% filter(Region != "SO"),
                    aes(x=Longitude, y=Latitude, colour=Region),
                    expand=0.005, s_shape=.9
                    )
rmap
ggsave(file="./vDesaturases_w_regions.svg", plot=rmap, width=14, height=8)
