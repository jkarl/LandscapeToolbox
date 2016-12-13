grts2<-function (design, DesignID = "Site", SiteBegin = 1, type.frame = "finite", 
    src.frame = "shapefile", in.shape = NULL, sp.object = NULL, 
    att.frame = NULL, id = NULL, xcoord = NULL, ycoord = NULL, 
    stratum = NULL, mdcaty = NULL, startlev = NULL, maxlev = 11, 
    maxtry = 1000, shift.grid = TRUE, do.sample = rep(TRUE, length(design)), 
    shapefile = TRUE, prjfilename = NULL, out.shape = "sample") 
{
    if (is.null(design)) 
        stop("\nA design list must be provided.")
    strata.names <- names(design)
    if (is.null(strata.names)) {
        if (length(design) > 1) {
            stop("\nThe design list must be named.")
        }
        else {
            warning("\nSince the single stratum specified in the design list was not named, \n\"None\" will be used for the stratum name.\n")
            strata.names <- "None"
            names(design) <- strata.names
        }
    }
    temp <- match(src.frame, c("shapefile", "sp.object", "att.frame"), 
        nomatch = 0)
    if (temp == 0) 
        stop(paste("\nThe value provided for argument src.frame, \"", 
            src.frame, "\" is not a valid value.", sep = ""))
    sp.ind <- FALSE
    if (src.frame == "sp.object") {
        if (is.null(sp.object)) 
            stop("\nAn sp package object is required when the value provided for argument src.frame \nequals \"sp.object\".")
        sp.ind <- TRUE
        src.frame <- "shapefile"
        in.shape <- "tempfile0921"
        sp2shape(sp.object, in.shape)
    }
    if (src.frame == "shapefile" && is.null(att.frame)) 
        att.frame <- read.dbf(in.shape)
    if (src.frame == "att.frame" && type.frame != "finite") 
        stop(paste("\nThe value provided for argument type.frame must equal \"finite\" when argument \nsrc.frame equals \"att.frame\"  The value provided for argument type.frame was \n\"", 
            type.frame, "\".", sep = ""))
    if (is.null(id)) {
        id <- "id"
        att.frame$id <- 1:nrow(att.frame)
    }
    else {
        temp <- match(id, names(att.frame), nomatch = 0)
        if (temp == 0) 
            stop(paste("\nThe value provided for the column from att.frame that identifies ID value for \neach element in the frame, \"", 
                id, "\", does not occur among the columns in \natt.frame.", 
                sep = ""))
        if (length(unique(att.frame[, id])) != nrow(att.frame)) 
            stop(paste("\nThe ID values for elements of the frame that are provided in att.frame are not \nunique.", 
                sep = ""))
        if (src.frame == "att.frame") {
            if (is.factor(att.frame[, id])) 
                att.frame[, id] <- as.character(att.frame[, id])
        }
        else {
            if (sp.ind) {
                src.temp <- "sp.object"
            }
            else {
                src.temp <- "shapefile"
            }
            if (!is.numeric(att.frame[, id])) 
                stop(paste("\nThe ID values in column \"", id, 
                  "\" of att.frame must be numeric when argument \nsrc.frame equals \"", 
                  src.temp, "\".", sep = ""))
            if (any(att.frame[, id] < 1)) 
                stop(paste("\nThe ID values in column \"", id, 
                  "\" of att.frame must be positive integers when \nargument src.frame equals \"", 
                  src.temp, "\".", sep = ""))
            att.temp <- read.dbf(in.shape)
            if (any(att.frame[, id] > nrow(att.temp))) 
                stop(paste("\nThe ID values in column \"", id, 
                  "\" of att.frame must not exceed the number of \nrecords when argument src.frame equals \"", 
                  src.temp, "\".", sep = ""))
            rm(att.temp)
            if (!is.integer(att.frame[, id])) 
                att.frame[, id] <- as.integer(att.frame[, id])
        }
    }
    if (is.null(stratum)) {
        if (length(strata.names) > 1) 
            stop("\nThe column from att.frame that identifies stratum membership was not provided \nand design specifies more than one stratum.")
        stratum <- "stratum"
        att.frame$stratum <- factor(rep(strata.names, nrow(att.frame)))
    }
    else {
        temp <- match(stratum, names(att.frame), nomatch = 0)
        if (temp == 0) 
            stop(paste("\nThe value provided for the column from att.frame that identifies stratum \nmembership for each element in the frame, \"", 
                stratum, "\", does not occur \namong the columns in att.frame.", 
                sep = ""))
    }
    if (!is.factor(att.frame[, stratum])) 
        att.frame[, stratum] <- as.factor(att.frame[, stratum])
    seltype.ind <- FALSE
    for (s in strata.names) {
        if (design[[s]]$seltype != "Equal") {
            seltype.ind <- TRUE
        }
    }
    if (seltype.ind) {
        if (is.null(mdcaty)) 
            stop(paste("\nThe name of the column from att.frame that identifies the unequal probability \ncategory for each element in the frame must be provided.", 
                sep = ""))
        temp <- match(mdcaty, names(att.frame), nomatch = 0)
        if (temp == 0) 
            stop(paste("\nThe value provided for the column from att.frame that identifies the unequal \nprobability category for each element in the frame, \"", 
                mdcaty, "\", \ndoes not occur among the columns in att.frame.", 
                sep = ""))
    }
    if (!is.null(startlev)) {
        if (startlev < 1) 
            stop("\nThe value for startlev cannot be less than 1")
        if (startlev > 11) 
            stop("\nThe value for startlev cannot be greater than 11")
        if (maxlev < 1) 
            stop("\nThe value for maxlev cannot be less than 1")
        if (maxlev > 11) 
            stop("\nThe value for maxlev cannot be greater than 11")
        if (startlev > maxlev) 
            stop("\nThe value for startlev cannot be greater than the value for maxlev")
    }
    else {
        if (maxlev < 1) 
            stop("\nThe value for maxlev cannot be less than 1")
        if (maxlev > 11) 
            stop("\nThe value for maxlev cannot be greater than 11")
    }
    if (type.frame == "finite") {
        first <- TRUE
        SiteBegin <- SiteBegin
        if (src.frame == "shapefile") {
            temp <- .Call("readShapeFilePts", in.shape)
            xcoord <- "x"
            ycoord <- "y"
            att.frame$x <- temp$x[att.frame[, id]]
            att.frame$y <- temp$y[att.frame[, id]]
        }
        else if (src.frame == "att.frame") {
            if (is.null(xcoord)) 
                xcoord <- "x"
            if (is.null(ycoord)) 
                ycoord <- "y"
            temp <- match(c(xcoord, ycoord), names(att.frame), 
                nomatch = 0)
            if (any(temp == 0)) 
                stop(paste("\nThe names for one or both of the columns containing the x-coordinates and \ny-coordinates, \"", 
                  xcoord, "\" and \"", ycoord, "\", \ndo not occur among the column names in att.frame.", 
                  sep = ""))
        }
        if (length(do.sample) > 1) {
            if (length(do.sample) != length(design)) 
                stop("\nArgument do.sample must be the same length as the design list.")
            if (is.null(names(do.sample))) {
                names(do.sample) <- strata.names
            }
            else {
                temp <- match(names(do.sample), strata.names, 
                  nomatch = 0)
                if (any(temp) == 0) 
                  temp.str <- vecprint(names(do.sample)[temp == 
                    0])
                stop(paste("\nThe following names in do.sample do not occur among the names in design:\n", 
                  temp.str, sep = ""))
            }
        }
        else if (is.null(names(do.sample))) {
            names(do.sample) <- strata.names
        }
        for (s in strata.names) {
            cat(paste("\nStratum:", s, "\n"))
            temp <- att.frame[, stratum] == s
            grtspts.ind <- TRUE
            if (sum(temp) == 0) {
                warning(paste("\nThe stratum column in the attributes data frame contains no values that match \nthe stratum named \"", 
                  s, "\" in the design list.\n", sep = ""))
                next
            }
            else if (sum(temp) == 1) {
                warning(paste("\nThe stratum column in the attributes data frame contains a single value that \nmatches the stratum named \"", 
                  s, "\" in the design list. \nThe sample for this stratum will be composed of a single point.\n", 
                  sep = ""))
                grtspts.ind <- FALSE
            }
            if (design[[s]]$seltype == "Equal") {
                sframe <- data.frame(id = I(att.frame[temp, id]), 
                  x = att.frame[temp, xcoord], y = att.frame[temp, 
                    ycoord], mdcaty = rep("Equal", nrow(att.frame[temp, 
                    ])))
            }
            else if (design[[s]]$seltype == "Unequal") {
                sframe <- data.frame(id = I(att.frame[temp, id]), 
                  x = att.frame[temp, xcoord], y = att.frame[temp, 
                    ycoord], mdcaty = factor(att.frame[temp, 
                    mdcaty]))
            }
            else if (design[[s]]$seltype == "Continuous") {
                sframe <- data.frame(id = I(att.frame[temp, id]), 
                  x = att.frame[temp, xcoord], y = att.frame[temp, 
                    ycoord], mdcaty = att.frame[temp, mdcaty])
            }
            else {
                stop(paste("\nThe value provided for the type of random selection, \"", 
                  design[[s]]$seltype, "\", \nfor stratum \"", 
                  s, "\" is not valid.", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                if (any(is.na(sframe$mdcaty))) 
                  stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", 
                    s, "\".", sep = ""))
            }
            else if (design[[s]]$seltype == "Continuous") {
                if (any(is.na(sframe$mdcaty))) 
                  stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", 
                    s, "\".", sep = ""))
                if (!is.numeric(sframe$mdcaty)) 
                  stop(paste("\nThe type of random selection for stratum \"", 
                    s, "\" is \"Continuous\", \nbut the unequal probability category values are not numeric.", 
                    sep = ""))
                if (any(sframe$mdcaty < 0)) 
                  stop(paste("\nNonpositive values were detected among the unequal probability category values \nfor stratum \"", 
                    s, "\".", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                if (is.null(design[[s]]$caty.n)) 
                  stop(paste("The type of random selection was set to \"Unequal\", but caty.n was not \nprovided for stratum \"", 
                    s, "\".", sep = ""))
                temp <- match(names(design[[s]]$caty.n), levels(as.factor(sframe$mdcaty)), 
                  nomatch = 0)
                if (any(temp == 0)) {
                  temp.str <- vecprint(names(design[[s]]$caty.n)[temp == 
                    0])
                  stop(paste("\nThe following names in caty.n for stratum \"", 
                    s, "\" do not occur \namong the levels of the mdcaty variable in att.frame:\n", 
                    temp.str, sep = ""))
                }
            }
            if (!is.numeric(design[[s]]$panel)) 
                stop(paste(" The design list must contain numeric values in the panel argument for \nstratum \"", 
                  s, "\".\n", sep = ""))
            design[[s]]$panel <- round(design[[s]]$panel)
            design[[s]]$panel <- design[[s]]$panel[design[[s]]$panel > 
                0]
            if (length(design[[s]]$panel) == 0) 
                stop(paste(" The design list does not not contain any valid values of the panel \nargument for stratum \"", 
                  s, "\".\n", sep = ""))
            if (design[[s]]$seltype == "Unequal") {
                if (!is.numeric(design[[s]]$caty.n)) 
                  stop(paste(" The design list must contain numeric values in the caty.n argument for \nstratum \"", 
                    s, "\".\n", sep = ""))
                design[[s]]$caty.n <- round(design[[s]]$caty.n)
                design[[s]]$caty.n <- design[[s]]$caty.n[design[[s]]$caty.n > 
                  0]
                if (length(design[[s]]$caty.n) == 0) 
                  stop(paste(" The design list does not not contain any valid values of the caty.n \nargument for stratum \"", 
                    s, "\".\n", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                temp <- sframe$mdcaty %in% names(design[[s]]$caty.n)
                if (any(!temp)) {
                  sframe <- sframe[temp, ]
                }
            }
            if (is.null(design[[s]]$over)) 
                design[[s]]$over <- 0
            if (design[[s]]$seltype != "Unequal") {
                samplesize <- sum(design[[s]]$panel)
                n.desired <- sum(samplesize, design[[s]]$over)
            }
            else {
                if (sum(design[[s]]$panel) != sum(design[[s]]$caty.n)) 
                  stop("\nThe sum of panel sample sizes does not equal sum of caty.n sample sizes")
                samplesize <- sum(design[[s]]$caty.n)
                if (design[[s]]$over == 0) {
                  n.desired <- design[[s]]$caty.n
                }
                else {
                  over.n <- design[[s]]$over * design[[s]]$caty.n/sum(design[[s]]$caty.n)
                  if (any(over.n != floor(over.n))) 
                    warning(paste("\nOversample size is not proportional to category sample sizes for stratum\n\"", 
                      s, "\".\n", sep = ""))
                  n.desired <- design[[s]]$caty.n + ceiling(over.n)
                }
            }
            if (design[[s]]$seltype == "Equal") 
                sframe$mdm <- mdmpts(sframe$mdcaty, c(Equal = n.desired))
            else if (design[[s]]$seltype == "Unequal") 
                sframe$mdm <- mdmpts(sframe$mdcaty, n.desired)
            else sframe$mdm <- n.desired * sframe$mdcaty/sum(sframe$mdcaty)
            if (grtspts.ind) {
                stmp <- grtspts(src.frame, in.shape, sframe, 
                  sum(n.desired), SiteBegin, shift.grid, do.sample[s], 
                  startlev, maxlev)
            }
            else {
                stmp <- data.frame(siteID = SiteBegin, id = sframe$id, 
                  xcoord = sframe$x, ycoord = sframe$y, mdcaty = sframe$mdcaty, 
                  wgt = 1/sframe$mdm)
                row.names(stmp) <- 1
                attr(stmp, "nlev") <- NA
            }
            if (nrow(stmp) < sum(n.desired)) 
                warning(paste("\nThe size of the selected sample was less than the desired size for stratum\n\"", 
                  s, "\".\n", sep = ""))
            stmp$stratum <- as.factor(rep(s, nrow(stmp)))
            stmp$panel <- as.character(rep("OverSamp", nrow(stmp)))
            n.panel <- length(design[[s]]$panel)
            browser()
            if (nrow(stmp) < samplesize) {
                n.short <- samplesize - nrow(stmp)
                n.temp <- n.short/n.panel
                if (n.temp != floor(n.temp)) {
                  n.temp <- c(ceiling(n.temp), rep(floor(n.temp), 
                    n.panel - 1))
                  i <- 1
                  while (sum(n.temp) != n.short) {
                    i <- i + 1
                    n.temp[i] <- n.temp[i] + 1
                  }
                }
                np <- c(0, cumsum(design[[s]]$panel - n.temp))
            }
            else {
                np <- c(0, cumsum(design[[s]]$panel))
            }
            for (i in 1:n.panel) stmp$panel[(np[i] + 1):np[i + 
                1]] <- names(design[[s]]$panel[i])
            if (design[[s]]$over > 0 || nrow(stmp) < samplesize) {
                if (design[[s]]$seltype != "Unequal") {
                  if (nrow(stmp) < samplesize) {
                    stmp$wgt <- n.desired * stmp$wgt/nrow(stmp)
                  }
                  else {
                    stmp$wgt <- n.desired * stmp$wgt/samplesize
                  }
                }
                else {
                  if (nrow(stmp) < samplesize) {
                    n.caty <- length(design[[s]]$caty.n)
                    n.temp <- n.short/n.caty
                    nc <- design[[s]]$caty.n - n.temp
                  }
                  else {
                    nc <- design[[s]]$caty.n
                  }
                  for (i in names(n.desired)) {
                    stmp$wgt[stmp$mdcaty == i] <- n.desired[i] * 
                      stmp$wgt[stmp$mdcaty == i]/nc[i]
                  }
                }
            }
            if (first) {
                sites <- stmp
                levels(sites$stratum) <- strata.names
                first <- FALSE
            }
            else {
                sites <- rbind(sites, stmp)
            }
            SiteBegin <- SiteBegin + nrow(stmp)
        }
    }
    else if (type.frame == "linear") {
        first <- TRUE
        SiteBegin <- SiteBegin
        if (is.null(att.frame$length_mdm)) {
            temp <- .Call("getRecordShapeSizes", in.shape)
            if (length(temp) != nrow(att.frame)) 
                stop("\nThe number of rows in the attribute data frame does not equal the number of \nrecords in the shapefile(s) in the working directory.")
            att.frame$length_mdm <- temp
        }
        elmsize <- "length_mdm"
        for (s in strata.names) {
            cat(paste("\nStratum:", s, "\n"))
            temp <- att.frame[, stratum] == s
            if (sum(temp) == 0) {
                warning(paste("\nThe stratum column in the attributes data frame contains no values that match \nthe stratum named \"", 
                  s, "\" in the design list.\n", sep = ""))
                next
            }
            if (design[[s]]$seltype == "Equal") {
                sframe <- data.frame(id = att.frame[temp, id], 
                  mdcaty = rep("Equal", nrow(att.frame[temp, 
                    ])), len = att.frame[temp, elmsize])
            }
            else if (design[[s]]$seltype == "Unequal") {
                sframe <- data.frame(id = att.frame[temp, id], 
                  mdcaty = factor(att.frame[temp, mdcaty]), len = att.frame[temp, 
                    elmsize])
            }
            else if (design[[s]]$seltype == "Continuous") {
                sframe <- data.frame(id = att.frame[temp, id], 
                  mdcaty = att.frame[temp, mdcaty], len = att.frame[temp, 
                    elmsize])
            }
            else {
                stop(paste("\nThe value provided for the type of random selection, \"", 
                  design[[s]]$seltype, "\", \nfor stratum \"", 
                  s, "\" is not valid.", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                if (any(is.na(sframe$mdcaty))) 
                  stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", 
                    s, "\".", sep = ""))
            }
            else if (design[[s]]$seltype == "Continuous") {
                if (any(is.na(sframe$mdcaty))) 
                  stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", 
                    s, "\".", sep = ""))
                if (!is.numeric(sframe$mdcaty)) 
                  stop(paste("\nThe type of random selection for stratum \"", 
                    s, "\" is \"Continuous\", \nbut the unequal probability category values are not numeric.", 
                    sep = ""))
                if (any(sframe$mdcaty < 0)) 
                  stop(paste("\nNonpositive values were detected among the unequal probability category values \nfor stratum \"", 
                    s, "\".", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                if (is.null(design[[s]]$caty.n)) 
                  stop(paste("The type of random selection was set to \"Unequal\", but caty.n was not \nprovided for stratum \"", 
                    s, "\".", sep = ""))
                temp <- match(names(design[[s]]$caty.n), levels(as.factor(sframe$mdcaty)), 
                  nomatch = 0)
                if (any(temp == 0)) {
                  temp.str <- vecprint(names(design[[s]]$caty.n)[temp == 
                    0])
                  stop(paste("\nThe following names in caty.n for stratum \"", 
                    s, "\" do not occur \namong the levels of the mdcaty variable in att.frame:\n", 
                    temp.str, sep = ""))
                }
            }
            if (!is.numeric(design[[s]]$panel)) 
                stop(paste(" The design list must contain numeric values in the panel argument for \nstratum \"", 
                  s, "\".\n", sep = ""))
            design[[s]]$panel <- round(design[[s]]$panel)
            design[[s]]$panel <- design[[s]]$panel[design[[s]]$panel > 
                0]
            if (length(design[[s]]$panel) == 0) 
                stop(paste(" The design list does not not contain any valid values of the panel \nargument for stratum \"", 
                  s, "\".\n", sep = ""))
            if (design[[s]]$seltype == "Unequal") {
                if (!is.numeric(design[[s]]$caty.n)) 
                  stop(paste(" The design list must contain numeric values in the caty.n argument for \nstratum \"", 
                    s, "\".\n", sep = ""))
                design[[s]]$caty.n <- round(design[[s]]$caty.n)
                design[[s]]$caty.n <- design[[s]]$caty.n[design[[s]]$caty.n > 
                  0]
                if (length(design[[s]]$caty.n) == 0) 
                  stop(paste(" The design list does not not contain any valid values of the caty.n \nargument for stratum \"", 
                    s, "\".\n", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                temp <- sframe$mdcaty %in% names(design[[s]]$caty.n)
                if (any(!temp)) {
                  sframe <- sframe[temp, ]
                }
            }
            if (is.null(design[[s]]$over)) 
                design[[s]]$over <- 0
            if (design[[s]]$seltype != "Unequal") {
                samplesize <- sum(design[[s]]$panel)
                n.desired <- sum(samplesize, design[[s]]$over)
            }
            else {
                if (sum(design[[s]]$panel) != sum(design[[s]]$caty.n)) 
                  stop("\nThe sum of panel sample sizes does not equal sum of caty.n sample sizes")
                samplesize <- sum(design[[s]]$caty.n)
                if (design[[s]]$over == 0) {
                  n.desired <- design[[s]]$caty.n
                }
                else {
                  over.n <- design[[s]]$over * design[[s]]$caty.n/sum(design[[s]]$caty.n)
                  if (any(over.n != floor(over.n))) 
                    warning(paste("\nOversample size is not proportional to category sample sizes for stratum\n\"", 
                      s, "\".\n", sep = ""))
                  n.desired <- design[[s]]$caty.n + ceiling(over.n)
                }
            }
            if (design[[s]]$seltype == "Equal") 
                sframe$mdm <- mdmlin(sframe$len, sframe$mdcaty, 
                  c(Equal = n.desired))
            else if (design[[s]]$seltype == "Unequal") 
                sframe$mdm <- mdmlin(sframe$len, sframe$mdcaty, 
                  n.desired)
            else sframe$mdm <- n.desired * sframe$mdcaty/sum(sframe$len * 
                sframe$mdcaty)
            stmp <- grtslin(in.shape, sframe, sum(n.desired), 
                SiteBegin, shift.grid, startlev, maxlev)
            stmp$stratum <- as.factor(rep(s, nrow(stmp)))
            stmp$panel <- rep("OverSamp", nrow(stmp))
            np <- c(0, cumsum(design[[s]]$panel))
            for (i in 1:length(design[[s]]$panel)) stmp$panel[(np[i] + 
                1):np[i + 1]] <- names(design[[s]]$panel[i])
            if (design[[s]]$over > 0) {
                if (design[[s]]$seltype != "Unequal") {
                  stmp$wgt <- n.desired * stmp$wgt/samplesize
                }
                else {
                  nc <- design[[s]]$caty.n
                  for (i in names(n.desired)) {
                    stmp$wgt[stmp$mdcaty == i] <- n.desired[i] * 
                      stmp$wgt[stmp$mdcaty == i]/nc[i]
                  }
                }
            }
            if (first) {
                sites <- stmp
                levels(sites$stratum) <- strata.names
                first <- FALSE
            }
            else {
                sites <- rbind(sites, stmp)
            }
            SiteBegin <- SiteBegin + nrow(stmp)
        }
    }
    else if (type.frame == "area") {
        first <- TRUE
        SiteBegin <- SiteBegin
        if (is.null(att.frame$area_mdm)) {
            temp <- .Call("getRecordShapeSizes", in.shape)
            if (length(temp) != nrow(att.frame)) 
                stop("\nThe number of rows in the attribute data frame does not equal the number of \nrecords in the shapefile(s) in the working directory.")
            att.frame$area_mdm <- temp
        }
        elmsize <- "area_mdm"
        for (s in strata.names) {
            cat(paste("\nStratum:", s, "\n"))
            temp <- att.frame[, stratum] == s
            if (sum(temp) == 0) {
                warning(paste("\nThe stratum column in the attributes data frame contains no values that match \nthe stratum named \"", 
                  s, "\" in the design list.\n", sep = ""))
                next
            }
            if (design[[s]]$seltype == "Equal") {
                sframe <- data.frame(id = att.frame[temp, id], 
                  mdcaty = rep("Equal", nrow(att.frame[temp, 
                    ])), area = att.frame[temp, elmsize])
            }
            else if (design[[s]]$seltype == "Unequal") {
                sframe <- data.frame(id = att.frame[temp, id], 
                  mdcaty = factor(att.frame[temp, mdcaty]), area = att.frame[temp, 
                    elmsize])
            }
            else if (design[[s]]$seltype == "Continuous") {
                sframe <- data.frame(id = att.frame[temp, id], 
                  mdcaty = att.frame[temp, mdcaty], area = att.frame[temp, 
                    elmsize])
            }
            else {
                stop(paste("\nThe value provided for the type of random selection, \"", 
                  design[[s]]$seltype, "\", \nfor stratum \"", 
                  s, "\" is not valid.", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                if (any(is.na(sframe$mdcaty))) 
                  stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", 
                    s, "\".", sep = ""))
            }
            else if (design[[s]]$seltype == "Continuous") {
                if (any(is.na(sframe$mdcaty))) 
                  stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", 
                    s, "\".", sep = ""))
                if (!is.numeric(sframe$mdcaty)) 
                  stop(paste("\nThe type of random selection for stratum \"", 
                    s, "\" is \"Continuous\", \nbut the unequal probability category values are not numeric.", 
                    sep = ""))
                if (any(sframe$mdcaty < 0)) 
                  stop(paste("\nNonpositive values were detected among the unequal probability category values \nfor stratum \"", 
                    s, "\".", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                if (is.null(design[[s]]$caty.n)) 
                  stop(paste("The type of random selection was set to \"Unequal\", but caty.n was not \nprovided for stratum \"", 
                    s, "\".", sep = ""))
                temp <- match(names(design[[s]]$caty.n), levels(as.factor(sframe$mdcaty)), 
                  nomatch = 0)
                if (any(temp == 0)) {
                  temp.str <- vecprint(names(design[[s]]$caty.n)[temp == 
                    0])
                  stop(paste("\nThe following names in caty.n for stratum \"", 
                    s, "\" do not occur \namong the levels of the mdcaty variable in att.frame:\n", 
                    temp.str, sep = ""))
                }
            }
            if (!is.numeric(design[[s]]$panel)) 
                stop(paste(" The design list must contain numeric values in the panel argument for \nstratum \"", 
                  s, "\".\n", sep = ""))
            design[[s]]$panel <- round(design[[s]]$panel)
            design[[s]]$panel <- design[[s]]$panel[design[[s]]$panel > 
                0]
            if (length(design[[s]]$panel) == 0) 
                stop(paste(" The design list does not not contain any valid values of the panel \nargument for stratum \"", 
                  s, "\".\n", sep = ""))
            if (design[[s]]$seltype == "Unequal") {
                if (!is.numeric(design[[s]]$caty.n)) 
                  stop(paste(" The design list must contain numeric values in the caty.n argument for \nstratum \"", 
                    s, "\".\n", sep = ""))
                design[[s]]$caty.n <- round(design[[s]]$caty.n)
                design[[s]]$caty.n <- design[[s]]$caty.n[design[[s]]$caty.n > 
                  0]
                if (length(design[[s]]$caty.n) == 0) 
                  stop(paste(" The design list does not not contain any valid values of the caty.n \nargument for stratum \"", 
                    s, "\".\n", sep = ""))
            }
            if (design[[s]]$seltype == "Unequal") {
                temp <- sframe$mdcaty %in% names(design[[s]]$caty.n)
                if (any(!temp)) {
                  sframe <- sframe[temp, ]
                }
            }
            if (is.null(design[[s]]$over)) 
                design[[s]]$over <- 0
            if (design[[s]]$seltype != "Unequal") {
                samplesize <- sum(design[[s]]$panel)
                n.desired <- sum(samplesize, design[[s]]$over)
            }
            else {
                if (sum(design[[s]]$panel) != sum(design[[s]]$caty.n)) 
                  stop("\nThe sum of panel sample sizes does not equal sum of caty.n sample sizes")
                samplesize <- sum(design[[s]]$caty.n)
                if (design[[s]]$over == 0) {
                  n.desired <- design[[s]]$caty.n
                }
                else {
                  over.n <- design[[s]]$over * design[[s]]$caty.n/sum(design[[s]]$caty.n)
                  if (any(over.n != floor(over.n))) 
                    warning(paste("\nOversample size is not proportional to category sample sizes for stratum\n\"", 
                      s, "\".\n", sep = ""))
                  n.desired <- design[[s]]$caty.n + ceiling(over.n)
                }
            }
            if (design[[s]]$seltype == "Equal") 
                sframe$mdm <- mdmarea(sframe$area, sframe$mdcaty, 
                  c(Equal = n.desired))
            else if (design[[s]]$seltype == "Unequal") 
                sframe$mdm <- mdmarea(sframe$area, sframe$mdcaty, 
                  n.desired)
            else sframe$mdm <- n.desired * sframe$mdcaty/sum(sframe$area * 
                sframe$mdcaty)
            stmp <- grtsarea(in.shape, sframe, sum(n.desired), 
                SiteBegin, shift.grid, startlev, maxlev, maxtry)
            if (nrow(stmp) < sum(n.desired)) 
                warning(paste("\nThe size of the selected sample was less than the desired size for stratum \n\"", 
                  s, "\".\n", sep = ""))
            stmp$stratum <- as.factor(rep(s, nrow(stmp)))
            stmp$panel <- as.character(rep("OverSamp", nrow(stmp)))
            n.panel <- length(design[[s]]$panel)
            if (nrow(stmp) < samplesize) {
                n.short <- samplesize - nrow(stmp)
                n.temp <- n.short/n.panel
                if (n.temp != floor(n.temp)) {
                  n.temp <- c(ceiling(n.temp), rep(floor(n.temp), 
                    n.panel - 1))
                  i <- 1
                  while (sum(n.temp) != n.short) {
                    i <- i + 1
                    ntemp[i] <- n.temp[i] + 1
                  }
                }
                np <- c(0, cumsum(design[[s]]$panel - n.temp))
            }
            else {
                np <- c(0, cumsum(design[[s]]$panel))
            }
            for (i in 1:n.panel) stmp$panel[(np[i] + 1):np[i + 
                1]] <- (design[[s]]$panel[i])
            if (design[[s]]$over > 0 || nrow(stmp) < samplesize) {
                if (design[[s]]$seltype != "Unequal") {
                  if (nrow(stmp) < samplesize) {
                    stmp$wgt <- n.desired * stmp$wgt/nrow(stmp)
                  }
                  else {
                    stmp$wgt <- n.desired * stmp$wgt/samplesize
                  }
                }
                else {
                  if (nrow(stmp) < samplesize) {
                    n.caty <- length(design[[s]]$caty.n)
                    n.temp <- n.short/n.caty
                    nc <- design[[s]]$caty.n - n.temp
                  }
                  else {
                    nc <- design[[s]]$caty.n
                  }
                  for (i in names(n.desired)) {
                    stmp$wgt[stmp$mdcaty == i] <- n.desired[i] * 
                      stmp$wgt[stmp$mdcaty == i]/nc[i]
                  }
                }
            }
            if (first) {
                sites <- stmp
                levels(sites$stratum) <- strata.names
                first <- FALSE
            }
            else {
                sites <- rbind(sites, stmp)
            }
            SiteBegin <- SiteBegin + nrow(stmp)
        }
    }
    else {
        stop(paste("\nThe value provided for the type of frame, \"", 
            type.frame, "\", is not valid.", sep = ""))
    }
    if (sp.ind) {
        file.remove(paste(in.shape, ".dbf", sep = ""), paste(in.shape, 
            ".shp", sep = ""), paste(in.shape, ".shx", sep = ""))
    }
    sites$siteID <- as.character(gsub(" ", "0", paste(DesignID, 
        "-", format(sites$siteID), sep = "")))
    sites$EvalStatus <- rep("NotEval", nrow(sites))
    sites$EvalReason <- rep(" ", nrow(sites))
    tm <- match(sites$id, att.frame[, id])
    if (design[[s]]$seltype == "Equal") 
        td <- match(c(id, stratum), names(att.frame))
    else td <- match(c(id, stratum, mdcaty), names(att.frame))
    temp <- names(att.frame)[-td]
    if (length(temp) > 0) {
        sites <- cbind(sites, att.frame[tm, -td])
        if (length(temp) == 1) 
            names(sites)[ncol(sites)] <- temp
    }
    sites <- sites[, -match("id", names(sites))]
    if (type.frame == "finite" && src.frame == "shapefile") 
        sites <- sites[, -match(c("x", "y"), names(sites))]
    if (src.frame == "shapefile") {
        if (type.frame == "linear") 
            sites <- sites[, -match("length_mdm", names(sites))]
        else if (type.frame == "area") 
            sites <- sites[, -match("area_mdm", names(sites))]
    }
    n <- nrow(sites)
    IDs <- as.character(1:n)
    row.names(sites) <- IDs
    attr(sites, "design") <- design
    ifelse(is.null(startlev), attr(sites, "startlev") <- "Not specified", 
        attr(sites, "startlev") <- startlev)
    ifelse(is.null(maxlev), attr(sites, "maxlev") <- "Not specified", 
        attr(sites, "maxlev") <- maxlev)
    attr(sites, "endlev") <- attributes(stmp)$nlev
    attr(sites, "maxtry") <- maxtry
    attr(sites, "shift.grid") <- shift.grid
    attr(sites, "do.sample") <- do.sample
    SpointsMat <- matrix(0, nrow = n, ncol = 2)
    rownames(SpointsMat) <- IDs
    SpointsMat[, 1] <- sites[, 2]
    SpointsMat[, 2] <- sites[, 3]
    sp.obj <- SpatialPointsDataFrame(SpatialPoints(SpointsMat), 
        data = sites)
    if (shapefile == TRUE) {
        temp <- sapply(sites, is.factor)
        if (any(temp)) {
            sites.tmp <- sites
            for (i in seq(ncol(sites.tmp))[temp]) {
                sites.tmp[, i] <- as.character(sites.tmp[, i])
                temp <- sites.tmp[, i] == "" | is.na(sites.tmp[, 
                  i])
                if (any(temp)) {
                  sites.tmp[temp, i] <- " "
                }
            }
            .Call("writeShapeFilePoint", sites.tmp$xcoord, sites.tmp$ycoord, 
                prjfilename, names(sites.tmp), sites.tmp, out.shape)
        }
        else {
            .Call("writeShapeFilePoint", sites$xcoord, sites$ycoord, 
                prjfilename, names(sites), sites, out.shape)
        }
    }
    invisible(sp.obj)
}
