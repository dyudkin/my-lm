require(tidyverse)
require(magrittr)
require(papaja)
require(broom)


my.lm <- function(data,
                  predictor,
                  dv,
                  controls,
                  row.name = predictor,
                  add.controls = NULL,
                  rm.controls = NULL,
                  caption = NULL) {
        model <-
                my.return.model.lm(data,
                                   predictor,
                                   dv,
                                   controls,
                                   add.controls,
                                   rm.controls)
        if (Console == T) {
                print(summary(model))
                print(my.lm.output((model), row.name))
        }
        if (PDF == T) {
                print_lm(model, predictor, dv, caption)
        }
        invisible(model)
}


my.return.model.lm <- function(data, predictor, dv, controls, add.controls = NULL, rm.controls = NULL) {
        controls <- controls[!controls %in% rm.controls]
        controls <- paste(c(controls, add.controls), collapse = " + ")
        predictors <- paste(predictor, controls, sep = " + ")
        formula <- paste(dv, predictors, sep = " ~ ")
        model <- lm(formula, data = data)
        return(model)
}


my.lm.output <- function(model, variable) {
        coefs <- filter(tidy(model), term == variable)
        beta <- sprintf("%.2f", round(coefs$estimate, 2))
        SE <- sprintf("%.2f", round(coefs$std.error, 2))
        t <- sprintf("%.2f", round(coefs$statistic, 2))
        df <- glance(model)$df.residual %>% round()
        p <- round(coefs$p.value, 3)
        output2 <-
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
        output <- as.vector(output2)
        print(summary(model))
        return(output)
}


print_lm <-
        function(themodel, 
                 predictor,
                 dv,
                 caption
        ) {
                df <- themodel %>% summary %>% coefficients %>% tail(-1)
                confint <- confint(themodel) %>% tail(-1)
                #std.beta <- std_beta(themodel)[,1:2]
                order <- order(row.names(df))
                df <- merge(df, confint, by = "row.names") %>%
                        #merge(std.beta, by.x = "Row.names", by.y = "term")%>%
                        arrange(order)
                df$df <- (themodel$df.residual)
                
                
                dv <- colnames(model.frame(themodel))[1]
                
                df <- df %>%
                        rename("p" = "Pr(>|t|)")
                
                df$pval = format(round(df$p, digits = 3), nsmall = 3) # round p-values into new variable
                df$pstring = as.character(df$pval) # convert this variable intro new string variable
                df$pmaterial = df$pstring
                
                
                df$pstring[df$p < 0.1 &
                                   df$p >= 0.05] <-
                        paste(df$pmaterial[df$p < 0.1 &
                                                   df$p >= 0.05], "(ref:cross)", sep = "")
                df$pstring[df$p < 0.05 &
                                   df$p >= 0.01] <-
                        paste(df$pmaterial[df$p < 0.05 &
                                                   df$p >= 0.01], "(ref:ast)", sep = "")
                df$pstring[df$p < 0.01 &
                                   df$p >= 0.001] <-
                        paste(df$pmaterial[df$p < 0.01 &
                                                   df$p >= 0.001], "(ref:ast)", "(ref:ast)", sep = "")
                df$pstring[df$p < .001] <-
                        paste("< ",
                              "0.001",
                              "(ref:ast)",
                              "(ref:ast)",
                              "(ref:ast)",
                              sep = "")
                
                df$pstring <-
                        gsub("0\\.", "\\.", df$pstring)  # add for removing the zero in front of the p values
                
                df <- df %>% select(-c(p, pval, pmaterial))
                
                
                df$rhs <- df %>% rownames()
                rownames(df) <- c()
                df <- df %>% rename(
                        "b" = "Estimate"
                        ,
                        "SE" = "Std. Error"
                        ,
                        "df" = "df"
                        ,
                        "t" = "t value"
                        ,
                        "(ref:cilower)" = "2.5 %"
                        ,
                        "(ref:ciupper)" = "97.5 %"
                        ,
                        "predictor" = "Row.names"
                        ,
                        "p" = "pstring"
                        #, "b" = "std.estimate"
                )
                
                df <-
                        df %>% select(predictor,
                                      b,
                                      SE,
                                      "(ref:cilower)",
                                      "(ref:ciupper)",
                                      t,
                                      df,
                                      p)
                
                return(
                        apa_table(
                                df,
                                align = c("l", "r", "r", "r", "r", "r", "r", "S[table-format = <0.3]","r"),
                                caption = caption,
                                note = paste(
                                        "(ref:ast)(ref:ast)(ref:ast)(ref:italp) < .001, 
                                                                        (ref:ast)(ref:ast)(ref:italp) < .01, 
                                                                        (ref:ast)(ref:italp) < .05, 
                                                                        (ref:cross)(ref:italp) <.10",
                                        sep = " "
                                ),
                                placement = "h",
                                longtable = TRUE,
                                escape = T
                        )
                )
                
        }
