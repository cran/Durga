## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(Durga)
library(datasets)

## -----------------------------------------------------------------------------
# Inspect the insulin data set
head(insulin[insulin$id < 4, ])

# Calculate differences in group means
d <- DurgaDiff(insulin, data.col = "sugar", group.col = "treatment", id.col = "id")
d

## -----------------------------------------------------------------------------
# Use a formula to define the model
DurgaDiff(sugar ~ treatment, insulin, id.col = "id")

## ----fig.height=4, fig.width=4------------------------------------------------
DurgaPlot(d)

## ----fig.height=4, fig.width=8------------------------------------------------
# Show two plots in the same figure, and increase margins so they don't look crowded
par(mfrow = c(1, 2), mar = c(5, 5, 4, 5) + 0.1)

DurgaPlot(d, 
          # Display box plots
          box = TRUE, 
          # Don't display data points
          points = FALSE, 
          # Don't display paired-value lines
          paired = FALSE, 
          main = "Box plot")

par(mar = c(5, 5, 4, 1) + 0.1)

DurgaPlot(d, 
          # Display bar charts
          bar = TRUE, 
          # Position effect size below
          ef.size.position = "below",
          # No paired-value lines
          paired = FALSE, 
          main = "Bar chart")

## ----fig.height=4, fig.width=5------------------------------------------------
# No title on this plot, so reduce the top margin size
par(mar = c(5, 4, 1, 1) + 0.1)

# Immature males are yellow, mature are red. Display juveniles first, and label
# with colour rather than developmental stage
d <- DurgaDiff(mass ~ maturity, damselfly, na.rm = TRUE,
               groups = c("Yellow" = "juvenile", "Red" = "adult"))
DurgaPlot(d, 
          # Custom y-axis label
          left.ylab = "Body mass (mg)", 
          # Specify the colours to (approximately) match the data
          group.colour = c("orange", "red"), 
          box = TRUE, box.fill = FALSE)

## ----fig.height=5, fig.width=8.5----------------------------------------------
par(mfrow = c(1, 2), mar = c(11, 4, 3, 6) + 0.1)
# Only show 2 groups even though there are 3 in the data
d <- DurgaDiff(iris, data.col = "Sepal.Length", group.col = "Species",
                   groups = c("versicolor", "setosa"))
DurgaPlot(d, 
          # Custom colours for points - the points are partially transparent
          points = c(DurgaTransparent("coral", .6), DurgaTransparent("darkturquoise", .8)),
          # Different colurs for violin (border) and violin fill
          violin = c("red", "blue"), violin.fill = c("#f8a8c840", "#a8c8f840"), 
          main = "Data subset; Two groups")

par(mar = c(5, 4, 3, 1) + 0.1)
d <- DurgaDiff(iris, data.col = "Sepal.Length", group.col = "Species")
# Define colours for the 3 species
bgCols <- c(DurgaTransparent("red", 0.5), 
            DurgaTransparent("blue", 0.5), 
            DurgaTransparent("green", 0.5))
DurgaPlot(d, 
          # Make all boxes the same colour
          box = "grey50", box.fill = "grey90",
          # Black for border/outline colour of points
          points = "black", 
          # Additional data point parameters: small squares filled with colour for species
          points.params = list(pch = 22, cex = 0.8, bg = bgCols[iris$Species]),
          main = "Coloured point fill")

## ----fig.height=6, fig.width=9------------------------------------------------
par(mfrow = c(1, 2))
d <- DurgaDiff(iris, data.col = "Sepal.Length", group.col = "Species")

# Only show error bars (95% CI) and central tendency
DurgaPlot(d, error.bars.type = "CI", 
          # Custom colour for central tendency
          central.tendency = rainbow(3), 
          points = FALSE, violin = FALSE, main = "Mean and 95% CI")

par(mar = c(5, 2.4, 4, 2) + 0.1)
DurgaPlot(d, bar = TRUE, error.bars.type = "SD", points = FALSE,  
          left.ylab = "", ef.size.label = "",
          main = "Bar chart with std. deviation")

## ----fig.height=4, fig.width=8------------------------------------------------
par(mfrow = c(1, 2))

di <- DurgaDiff(iris, "Sepal.Length", "Species")

DurgaPlot(di, contrasts = "*", main = "Problematic effect sizes")
mtext("a)", cex = 1.5, col = "brown", font = 2, adj = -0.3, line = 1.4)

DurgaPlot(di, main = "Default effect sizes (subset)")
mtext("b)", cex = 1.5, col = "brown", font = 2, adj = -0.3, line = 1.4)

## ----fig.height=4, fig.width=7------------------------------------------------
d <- DurgaDiff(petunia, 1, 2)
# Use the top margin for brackets
op <- par(mar = c(2, 4, 4, 1) + 0.1)

# Custom group colours
cols = rainbow(3)

# Save return value from DurgaPlot
p <- DurgaPlot(d,
               # Don't draw effect size because we will draw brackets
               ef.size = FALSE, 
               # Don't draw frame because brackets will appear in the upper margin
               frame.plot = FALSE, 
               # Custom group colours
               group.colour = cols)

# Customise colours by drawing brackets with the colour of the taller group.
# Get the taller group from each difference
tallerIdx <- sapply(d$group.differences, function(diff) {
  # If the group difference > 0, the first group is taller
  ifelse(diff$t0 > 0, diff$groupIndices[1], diff$groupIndices[2])
  })

# Draw the brackets
DurgaBrackets(p, labels = "level CI", br.col = cols[tallerIdx])

## ----fig.height=4, fig.width=8------------------------------------------------
par(mfrow = c(1, 2), mar = c(3, 4, 3, 1) + 0.1)

# Construct some data with multiple groups
set.seed(1) # Ensure we always get the exact same data set
multiGroupData <- data.frame(Measurement = unlist(lapply(c(10, 12, 8, 8.5, 9, 9.1),
                                                         function(m) rnorm(40, m))),
                             Group = rep(paste0("g", 1:6), each = 40),
                             Id = rep(1:6, 40))
d <- DurgaDiff(multiGroupData, 1, 2)

DurgaPlot(d, 
          # Visually group each pair
          group.dx = c(0.18, -0.18), 
          # Show box plots
          box = TRUE, 
          # Make the boxes narrow 
          box.params = list(boxwex = 0.4), 
          # Don't display effect size or data points
          ef.size = FALSE, points = FALSE, 
          xlim = c(0.8, 6.2), # Reduce white space on plot left and right
          main = "Grouped into pairs")

DurgaPlot(d,
          # Shift violins left
          violin.dx = -0.12,
          # Shift points right
          points.dx = 0.1,
          # Make points small and jittered
          points.params = list(cex = 0.5), points.method = "jitter",
          # No central tendency indicator
          central.tendency = FALSE,
          ef.size = FALSE, main = "Component shifts")

## ----eval=FALSE---------------------------------------------------------------
#  # Example snippet, make *.dx arguments relative to group.dx
#  group.dx <- c(0.75, 0)  # Shift group 1 right, leave group 2 unchanged
#  DurgaPlot(..., group.dx = group.dx,
#            central.tendency.dx = group.dx + 0.1, # Shift mean & error bars right
#            violin.dx = group.dx + c(-0.4, -0.5), # Shift violins left
#            ef.size.dx = -0.5)

## ----fig.height=6, fig.width=8------------------------------------------------
par(mar = c(5, 9, 3, 1) + 0.1)

# Define effect size labels and their positions on the y-axis
effectSizes <- c("No effect" = 0, "Large positive effect" = 0.8, "Huge positive effect" = 2)

# Give the groups friendly names
groups <- c("Self" = "self_fertilised", "Westerham" = "westerham_cross", "Inter" = "inter_cross")

# Calculate Cohen's d rather than unstandardised difference in means
d <- DurgaDiff(petunia, 1, 2, effect.type = "cohens", groups = groups)

DurgaPlot(d, violin = FALSE, main = "Cohen's d, labelled effect size", 
          points.method = "jitter", 
          # Use our ticks and labels instead of default
          ef.size.ticks = effectSizes, 
          # Horizontal tick labels
          ef.size.params = list(las = 1),
          # Don't label the effect size y-axis because it won't fit with the long tick labels
          ef.size.label = "")

## ----fig.height=4.5, fig.width=6----------------------------------------------
# Define a function that calculates the statistic for two vectors
log2Ratio <- function(x1, x2) log2(mean(x2) / mean(x1))

# Generate some random data
set.seed(1)
data <- data.frame(species = rep(c("Sp1", "Sp2"), each = 10),
                   volume = c(rnorm(10, mean = 5, sd = 2), rnorm(10, mean = 10, sd = 2)))

# Apply log2Ratio as the effect type
d <- DurgaDiff(data, "volume", "species", effect.type = log2Ratio)

# Apply custom labels to describe the effect type, since it is not Sp2 - Sp1
d$group.differences[[1]]$label.plot <- expression("log"[2] ~ italic("Sp 2") * ":" * italic("Sp 1"))

# Plot, and set the y-axis label for effect size
DurgaPlot(d, ef.size.label = expression("log"[2]~"ratio"))

# It is also possible to set the label to be used for printing to the console
d$group.differences[[1]]$label.print <- "log2 Sp 2:Sp 1"
d


## ----eval=FALSE---------------------------------------------------------------
#  # Create a new function that calls effectsize::cohens_d and returns just the calculated statistic
#  myCohens <- function(x1, x2) {
#    # Note that we swap the arguments because Durga expects the calculation to be x2 - x1
#    cd <- effectsize::cohens_d(x2, x1, pooled_sd = FALSE)
#    # Extract and return just the Cohen's d value
#    cd$Cohens_d
#  }
#  # Pass it in to DurgaDiff as the effect.type
#  d <- DurgaDiff(petunia, 1, 2, effect.type = myCohens)
#  
#  # Specify the effect type label
#  DurgaPlot(d, ef.size.label = expression("Cohen's d"))

## ----fig.height=5, fig.width=7------------------------------------------------
par(mar = c(5, 4, 3, 1) + 0.1)

# Define display order and display pretty group names
groups = c("Control 1" = "g1", "Treatment 1" = "g2", 
           "Control 2" = "g3", "Treatment 2" = "g4", 
           "Control 3" = "g5", "Treatment 3" = "g6")
d <- DurgaDiff(multiGroupData, 1, 2, 3, groups = groups, 
                   contrasts = "g2 - g1, g4 - g3, g6 - g5")
# Reduce text size
par(cex = 0.7)
DurgaPlot(d, box = TRUE, 
          # Group into pairs
          group.dx = c(0.15, -0.15), 
          # Don't display points
          points = FALSE, 
          # Adjust plot width to reduce left/right wasted space
          xlim = c(1, 6), 
          # Don't display pair lines
          paired = FALSE, 
          # Hide the boxplot median line (medcol = NA) and make the boxes narrow (boxwex = 0.4)
          box.params = list(medcol = NA, boxwex = 0.4), 
          # Display mean and error bar
          central.tendency = "#8060b0a0",      # Display and colour central tendency
          central.tendency.symbol = "segment", 
          central.tendency.width = 0.07,
          error.bars = "#8060b0a0",            # Same colour for error bars
          # Don't display the effect size violin
          ef.size.violin = FALSE, 
          main = "Group offsets"
        )

## ----fig.height=5, fig.width=7------------------------------------------------
# Add in some fake sex data to the insulin data set
data <- cbind(insulin, Sex = sample(c("Male", "Female"), nrow(insulin), replace = TRUE))
# Thin the data so that individual symbols are visible.
# Obviously these data manipulations are for plot demonstration purposes only
data <- data[data$id %in% 1:15, ]

d <- DurgaDiff(data, "sugar", "treatment", "id", 
                   groups = c("Before insulin" = "before", "After insulin" = "after"), na.rm = TRUE)
par(mar = c(2, 4, 4, 1))
# Use different colours to show sex
cols <- RColorBrewer::brewer.pal(3, "Set1")
DurgaPlot(d, 
          # Don't draw containing box
          frame.plot = FALSE,
          # Relabel the contrasts
          contrasts = c("Blood sugar change" = "after - before"),
          left.ylab = "Blood sugar level",
          # Customise the violins
          violin.shape = c("left", "right"), violin.dx = c(-0.055, 0.055), violin.width = 0.3, 
          # Customise the points to display sex
          points = "black",
          points.params = list(bg = ifelse(data$Sex == "Female", cols[1], cols[2]), 
                               pch = ifelse(data$Sex == "Female", 21, 24)), 
          # Customise the effect size
          ef.size.pch = 19,
          ef.size.violin = "blue",
          ef.size.violin.shape = "full",
          # Make mean lines match group colour
          ef.size.line.col = RColorBrewer::brewer.pal(3, "Set2")[2:1],
          ef.size.line.lty = 3,
          central.tendency = FALSE,
          main = "Effects of insulin on blood sugar")
# Add a legend
legend("topright", c("Female", "Male"), pch = c(21, 24), pt.bg = cols)

## ----fig.height=5, fig.width=6------------------------------------------------

d <- DurgaDiff(insulin, "sugar", "treatment", "id",
               groups = c("Before insulin" = "before", "After insulin" = "after"), na.rm = TRUE)

# Change effect size tick label
d$group.differences[[1]]$label.plot <- "Change in sugar"

# Horizontal axis tick labels, move y-axis labels outwards to make space for the
# horizontal ticks, and ensure the margins are large enough to show them
par(las = 1, mgp = c(4, 1, 0), mar = c(3, 5.5, 4, 5.5) + 0.1)

cols <- RColorBrewer::brewer.pal(3, "Set2")
colsTr <- DurgaTransparent(cols, 0.7)
DurgaPlot(d, paired = FALSE,
          # Move 2nd group to be at the same position as 1st
          group.dx = c(0.2, -0.8),
          # X-axis ticks are meaningless since both groups are at the same location
          x.axis = FALSE,
          # Adjust plot width and spacing
          xlim = c(0.7, 2.1),
          violin = TRUE, violin.shape = "right", violin.fill = colsTr, 
          # No need to specify violin position because violin.dx defaults to group.dx
          # Bumpy violins
          violin.adj = 1,
          # Box plots are black outlines
          box = "black", box.dx = c(0.06, -0.895), box.fill = colsTr, 
          # Don't show outliers
          box.outline = FALSE,
          # Thin boxes, solid thick lines
          box.params = list(boxwex = 0.06, lty = 1, lwd = 2, 
                            # normal line weight for median line
                            medlwd = 2, 
                            # no end of whisker line
                            staplecol = NA), 
          # Points as solid colours, slightly smaller than default
          points = cols, points.params = list(pch = 16, cex = 0.8),
          points.method = "jitter", points.dx = c(-0.14, -1.14), points.spread = 0.09,
          # Move effect size left to near where 2nd group would have been
          ef.size.dx = -1.2,
          left.ylab = "Blood sugar",
          main = "Rain cloud with effect size"
)

legend("topright", c("Before insulin", "After insulin"), col = cols, fill = colsTr, bty = "n")

