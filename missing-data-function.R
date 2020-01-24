require("mice")

my.missing.data.model <- function(model) {
        mice::md.pattern(model.frame(model))
}
