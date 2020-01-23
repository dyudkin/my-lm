my.lmer <- function(data,
                            predictor,
                            dv,
                            controls,
                            rdm.effect,
                            row.name = predictor,
                            add.controls = NULL,
                            rm.controls = NULL,
                            caption = NULL) {
        model <-
                my.return.model.lmer(data,
                                     predictor,
                                     dv,
                                     controls,
                                     rdm.effect,
                                     add.controls,
                                     rm.controls)
        if (Console == T) {
                print(summary(model))
                print(my.lmer.output((model), row.name))
        }
        if (PDF == T) {
                print_lmer(model, predictor, dv, timepoint, caption)
        }
        invisible(model)
}


my.return.model.lmer <- function(data, predictor, dv, controls, rdm.effect, add.controls = NULL, rm.controls = NULL) {
        controls <- controls[!controls %in% rm.controls]
        controls <- paste(c(controls, add.controls), collapse = " + ")
        rdm.effect <- paste0("(1|", rdm.effect, ")")
        predictors <-
                paste(predictor, controls, rdm.effect, sep = " + ")
        formula <- paste(dv, predictors, sep = " ~ ")
        model <- lmer(formula, data = data)
        return(model)
}


my.lmer.output <- function(model, variable) {
        coefs <- summary(model)$coefficients[variable, ]
        beta <- sprintf("%.2f", round(coefs[1], 2))
        SE <- sprintf("%.2f", round(coefs[2], 2))
        t <- sprintf("%.2f", round(coefs[4], 2))
        df <- round(coefs[3], 0)
        p <- round(coefs[5], 3)
        output <-
                ifelse(
                        p >= .001,
                        paste0(
                                "_B_",
                                " = ",
                                beta,
                                ", _SE_",
                                " = ",
                                SE,
                                ", _t_(",
                                df,
                                ") = ",
                                t,
                                ", _p_ = ",
                                p
                        ),
                        ifelse(
                                p < .001,
                                paste0(
                                        "_B_",
                                        " = ",
                                        beta,
                                        ", _SE_",
                                        " = ",
                                        SE,
                                        ", _t_(",
                                        df,
                                        ") = ",
                                        t,
                                        ", _p_ < ",
                                        .001
                                ),
                                NA
                        )
                )
        output <- as.vector(output)
        return(output)
}




my.lmer.output <- function(model, variable) {
        coefs <- summary(model)$coefficients[variable, ]
        beta <- sprintf("%.2f", round(coefs[1], 2))
        SE <- sprintf("%.2f", round(coefs[2], 2))
        t <- sprintf("%.2f", round(coefs[4], 2))
        df <- round(coefs[3], 0)
        p <- round(coefs[5], 3)
        output <-
                ifelse(
                        p >= .001,
                        paste0(
                                "_B_",
                                " = ",
                                beta,
                                ", _SE_",
                                " = ",
                                SE,
                                ", _t_(",
                                df,
                                ") = ",
                                t,
                                ", _p_ = ",
                                p
                        ),
                        ifelse(
                                p < .001,
                                paste0(
                                        "_B_",
                                        " = ",
                                        beta,
                                        ", _SE_",
                                        " = ",
                                        SE,
                                        ", _t_(",
                                        df,
                                        ") = ",
                                        t,
                                        ", _p_ < ",
                                        .001
                                ),
                                NA
                        )
                )
        output <- as.vector(output)
        return(output)
}
