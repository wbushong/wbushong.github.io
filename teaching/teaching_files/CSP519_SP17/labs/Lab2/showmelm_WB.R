
p.cor <- function(x) {
  model.data <- as.data.frame(model.matrix(x)[,-1])
  names(model.data) <- letters[1:length(names(model.data))]
  model.data <- na.omit(model.data)
  model.data$Y <-  fitted(x) + resid(x)

  predictors <- names(model.data)[-(which(names(model.data) =="Y"))]

  if (length(predictors) < 2) {
    (sr <- cor(model.data))[2,1]
  } else {

    resid.values <- model.data[, c("Y", predictors)]

    for(i in names(model.data)[-(which(names(model.data) =="Y"))] ) {
      resid.values[,predictors[predictors == i]] <- resid(lm(as.formula(paste(i, " ~ ", 1, paste(" +", predictors[-which(predictors == i)], collapse=""))), data=model.data))
    }



    (sr <- cor(resid.values)[,1][-1])
  }
}



vif <- function(mod, ...) {
  ## Copied from the car package (thanks to John Fox! ##
  if (any(is.na(coef(mod))))
    stop("there are aliased coefficients in the model")
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) {
    result <- NA } else {R <- cov2cor(v)
                         detR <- det(R)
                         result <- matrix(0, n.terms, 3)
                         rownames(result) <- terms
                         colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
                         for (term in 1:n.terms) {
                           subs <- which(assign == term)
                           result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs,
                                                                                              -subs]))/detR
                           result[term, 2] <- length(subs)
                         }
                         if (all(result[, 2] == 1))
                           result <- result[, 1]
                         else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
                         result
                       }
}


########### strip zeros function #########
strip0 <- function(x) {
  save.dims <- dim(x)
  save.dimnames <-dimnames(x)
  x.mat.1 <- as.matrix(x)
  x.mat.2 <- matrix(x.mat.1, nrow=1)
  x.stripped <- sub("-0.", "-.", x.mat.2, fixed=TRUE)
  x.stripped <- sub("0.", ".", x.stripped, fixed=TRUE)
  x.mat <- matrix(x.stripped)
  dim(x.mat) <- save.dims
  dimnames(x.mat) <- save.dimnames
  x <- as.data.frame(x.mat, stringsAsFactors=FALSE)
  return(x)
}
########## corstars function
corstars <- function(x){
  require(Hmisc)
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P

  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))

  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew, stringsAsFactors=FALSE)

  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])

  ## strip leading zeros as per APA style
  Rnew <- strip0(Rnew)
  Rnew <- format(Rnew, justify="left")
  ## provide the result
  return(Rnew)
}

## lm to df function ##

lm2df <- function(x) {
  DF <- as.data.frame(model.matrix(x)[,-1])
  names(DF) <- letters[1:length(names(DF))]
  DF <- na.omit(DF)
  DF$Y <-  fitted(x) + resid(x)
  names(DF) <- c(colnames(model.matrix(x))[-1], as.character(as.formula(x$call))[2])
  return(DF)
}

higher.lm <- function(x, sets=NULL, ...) {
  if(length(sets) > 0) {
    l <- length(names(x$coefficients))
    predictors <- names(x$coefficients)[2:l]
    dat <- as.character(x$call["data"])
    outcome <- as.character(x$terms)[2]
    vars <- rownames(attributes(x$terms)$factors)
    DF <- get(dat)[vars]
    DF <- na.omit(DF)
    MODS <- list()
    keep <- numeric()
    fo <- character()
    for(i in 1:length(sets)) {
      keep <- unique(c(keep, c(sets[[1]], sets[[i]])))
      fo[i] <- paste(outcome, "~", paste(predictors[keep], collapse="+"))
      MODS[[i]] <- lm(as.formula(fo[i]), data=DF)
    }
    AOV <- list()
    DR2 <- numeric()
    for(i in 1:(length(MODS)-1)) {
      AOV[[i]] <- anova(MODS[[i]], MODS[[i+1]])
      DR2[i] <- summary(MODS[[i+1]])$r.squared - summary(MODS[[i]])$r.squared
    }
    return(list(fo=fo, AOV=AOV, DR2=DR2, MODS=MODS))
  }
}


showme.lm <- function(x, sr2=TRUE, tol=FALSE, vif=FALSE, cor=FALSE, sets=NULL, digits=4, verbose = FALSE) {
  if(length(sets) < 1) {
    tmp <- summary(x)

    if(sr2==TRUE) {
      R2 <- (p.cor(x))^2
      tmp$coefficients <- cbind(tmp$coefficients, c(NA, R2))
      colnames(tmp$coefficients)[which(colnames(tmp$coefficients) == "")] <- "sr2"
    }
    if(tol==TRUE) {
      VIF <- vif(x)
      TOL <- 1/VIF
      tmp$coefficients <- cbind(tmp$coefficients, c(NA, TOL))
      colnames(tmp$coefficients)[which(colnames(tmp$coefficients) == "")] <- "Tol."
    }
    if(vif==TRUE) {
      VIF <- vif(x)
      TOL <- 1/VIF
      tmp$coefficients <- cbind(tmp$coefficients, c(NA, VIF))
      colnames(tmp$coefficients)[which(colnames(tmp$coefficients) == "")] <- "VIF"
    }

    MODS <- x
    MODS.sum <- tmp
    AOV <- anova(x)
    Rtable <- tmp
  }

  if(cor==TRUE) {
    DF <- lm2df(x)
    COR <- corstars(DF)
    cat(" ### Correlation Matrix ### \n \n")
    print(COR)
    cat("\n ")
  }


  if(length(sets) > 1) {
    HIGHER <- higher.lm(x, sets=sets)
    AOV <- HIGHER$AOV
    MODS <- HIGHER$MODS
    DR2 <- HIGHER$DR2
    fo <- HIGHER$fo

    cat("### Multiple df Tests ### \n \n")
    for(i in 1:length(AOV)) {
      names(AOV)[[i]] <-paste("Comparison of models", i, "and", i+1)
      print(AOV[i])
      cat("Delta R2 =", DR2[i], "\n \n")
    }

    MODS.sum <- list()
    for(i in 1:length(MODS)) {
      MODS.sum[[i]] <- summary(MODS[[i]])
    }

    add.names <- character()

    if(sr2==TRUE) {
      add.names <- c(add.names, "sr2")
      for(i in 1:length(MODS.sum)) {
        R2 <- (p.cor(MODS[[i]]))^2
        MODS.sum[[i]]$coefficients <- cbind(MODS.sum[[i]]$coefficients, c(NA, R2))
      }
    }


    if(vif==TRUE) {
      add.names <- c(add.names, "VIF")
      for(i in 1:length(MODS.sum)) {
        VIF <- vif(MODS[[i]])
        MODS.sum[[i]]$coefficients <- cbind(MODS.sum[[i]]$coefficients, c(NA, VIF))
      }
    }


    if(tol==TRUE) {
      add.names <- c(add.names, "Tol.")
      for(i in 1:length(MODS.sum)) {
        VIF <- vif(MODS[[i]])
        TOL <- 1/VIF
        MODS.sum[[i]]$coefficients <- cbind(MODS.sum[[i]]$coefficients, c(NA, TOL))
      }
    }

    Rtable <- MODS.sum

    for(i in 1:length(Rtable)) {
      colnames(Rtable[[i]]$coefficients)[which(colnames(Rtable[[i]]$coefficients) == "")] <- add.names

    }

    for(i in 1:length(Rtable)) {
      Rtable[[i]]$call <- call("lm", as.formula(fo[i]))
      names(Rtable[i]) <- paste("Model", i, "summary")
    }


  }

  Rtable$coefficients <- round(Rtable$coefficients, digits = digits)
  if (verbose) {
    cat("### Model Summaries ### \n")
    print(Rtable)
  }

  Result <- list(Models = MODS, ModelSummaries = MODS.sum, ANOVAtables = AOV, RegressionTables=Rtable)
  invisible(Result)
}
