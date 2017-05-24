library(maps)
library(mapdata)
library(maptools)
library(spatstat)
library(spatstat.utils)
library(rgeos)
library(sp)
library(leaflet)

# Load data
df <- read.csv("MetroBarcelona.csv")

mapAreas <- c("Spain")

counties <- map("worldHires",
                "Spain",
                exact = TRUE,
                fill=TRUE,
                plot=FALSE)

countries <- gUnaryUnion(map2SpatialPolygons(counties,
                                             IDs = counties$names,
                                             proj4string = CRS("+proj=longlat +datum=WGS84")))

# Transform the map to UTM coordinates
countries <- spTransform(countries,
                         CRS("+proj=utm +zone=31 ellps=WGS84"))
W <- as(countries, "owin")

# Transform coordinates from long and lat to UTM coordinates
lonLat <- data.frame(ID = df$Id, X = df$Longitude, Y = df$Latitude)
coordinates(lonLat) <- c("X", "Y")
proj4string(lonLat) <- CRS("+proj=longlat +datum=WGS84")
utmCoord <- spTransform(lonLat, CRS("+proj=utm +zone=31 ellps=WGS84"))

X <- ppp(x = utmCoord$X,
         y = utmCoord$Y,
         window = W)

# Dirichlet tesselation
y <- dirichlet(X)

names(y$tiles) <- df$Name

# Convert large tess to spatial polygons
owin2Polygons <- function(x, id="1") {
  stopifnot(is.owin(x))
  x <- as.polygonal(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1),] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords = closering(cbind(p$x, p$y)),
                             hole = is.hole.xypolygon(p))
                               })
  z <- Polygons(pieces, id)
  return(z)
}

tess2SP <- function(x) {
  stopifnot(is.tess(x))
  y <- tiles(x)
  nom <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nom[i])
  return(SpatialPolygons(z))
}

tessSP <- tess2SP(y)

# Transform polygons from UTM back to long and lat
proj4string(tessSP) <- CRS("+proj=utm +zone=31 ellps=WGS84")
tessSP <- spTransform(tessSP,
                      CRS("+proj=longlat +datum=WGS84"))

# Get metro lines and their stops
l1 <- subset(df, !is.na(df$L1))
l1 <- l1[order(l1$L1), ]
l2 <- subset(df, !is.na(df$L2))
l2 <- l2[order(l2$L2), ]
l3 <- subset(df, !is.na(df$L3))
l3 <- l3[order(l3$L3), ]
l4 <- subset(df, !is.na(df$L4))
l4 <- l4[order(l4$L4), ]
l5 <- subset(df, !is.na(df$L5))
l5 <- l5[order(l5$L5), ]
l6 <- subset(df, !is.na(df$L6))
l6 <- l6[order(l6$L6), ]
l7 <- subset(df, !is.na(df$L7))
l7 <- l7[order(l7$L7), ]
l8 <- subset(df, !is.na(df$L8))
l8 <- l8[order(l8$L8), ]
l9S <- subset(df, !is.na(df$L9S))
l9S <- l9S[order(l9S$L9S), ]
l9N <- subset(df, !is.na(df$L9N))
l9N <- l9N[order(l9N$L9N), ]
l10 <- subset(df, !is.na(df$L10))
l10 <- l10[order(l10$L10), ]
l11 <- subset(df, !is.na(df$L11))
l11 <- l11[order(l11$L11), ]
l12 <- subset(df, !is.na(df$L12))
l12 <- l12[order(l12$L12), ]


# Generate leaflet map
leaflet() %>%
  # Base map
  addProviderTiles("Hydda.Full") %>%
  # Voronoi layer
  addPolygons(data = tessSP,
              stroke = TRUE,
              color = "black", weight = 0.5,
              fill=TRUE, fillOpacity = 0,
              label = df$Name,
              popup = df$Name) %>%
  # L1
  addPolylines(data = l1,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#e1393e") %>%
  # L2
  addPolylines(data = l2,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#9c459a") %>%
  # L3
  addPolylines(data = l3,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#53b955") %>%
  # L4
  addPolylines(data = l4,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#febd10") %>%
  # L5
  addPolylines(data = l5,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#317bc8") %>%
  # L6
  addPolylines(data = l6,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#847dc6") %>%
  # L7
  addPolylines(data = l7,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#ae6118") %>%
  # L8
  addPolylines(data = l8,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#e659b4") %>%
  # L9S
  addPolylines(data = l9S,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#f68429") %>%
  # L9N
  addPolylines(data = l9N,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#f68429") %>%
  # L10
  addPolylines(data = l10,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#00adef") %>%
  # L11
  addPolylines(data = l11,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#a8d164") %>%
  # L12
  addPolylines(data = l12,
               lng = ~Longitude,
               lat = ~Latitude,
               color = "#b6b3e1") %>%
  # Metro stops
  addCircles(data = df,
             lng = ~Longitude,
             lat = ~Latitude,
             radius = 10,
             popup = df$Name,
             label = df$Name,
             color = "black", weight = 1, opacity = 1,
             fillColor = "white", fillOpacity = 1)