WriteOut <- function(x, writetype = "all", areanames = NULL) {
  
  if (class(x) == "list") {
    if(length(areanames) == 0){
      areanam <- x[[1]]$areanam
    }else{
      areanam <- areanames
  }
    if (writetype == "all") {
      .NexusOut(x)
      for (i in 1:length(x)) {
        .WriteTablesSpGeo(x[[i]], prefix = names(x)[i])
      }
      if (length(dim(x[[1]]$coexistence_classified)) == 0) {
        warning("no coexistence matrix found")
      } else {
        for (i in 1:length(x)) {
          .OutHeatCoEx(x[[i]], prefix = names(x)[i])
        }
      }
      for (i in 1:length(x)) {
        .OutPlotSpPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartSpec(x[[i]], prefix = names(x)[i])
        .OutMapAll(x[[i]], prefix = names(x)[i])
        .OutMapPerSpecies(x[[i]], prefix = names(x)[i])
        .OutMapPerPoly(x[[i]], prefix = names(x)[i])
      }
    }
    if (writetype == "graphs") {
      for (i in 1:length(x)) {
        .OutPlotSpPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartPoly(x[[i]], prefix = names(x)[i])
        .OutBarChartSpec(x[[i]], prefix = names(x)[i])
      }
    }
    if (writetype == "maps") {
      for (i in 1:length(x)) {
        .OutMapAll(x[[i]], prefix = names(x)[i])
        .OutMapPerSpecies(x[[i]], prefix = names(x)[i])
        .OutMapPerPoly(x[[i]], areanames = areanames, prefix = names(x)[i])
      }
    }
    if (writetype == "statistics") {
      for (i in 1:length(x)) {
        .WriteTablesSpGeo(x[[i]], prefix = names(x)[i])
      }
    }
    if (writetype == "nexus") {
      .NexusOut(x)
    }
    if (writetype == "coexistence") {
      if (length(dim(x[[1]]$coexistence_classified)) == 0) {
        print("No coexistence matrix found")
      } else {
        for (i in 1:length(x)) {
          .OutHeatCoEx(x[[i]], prefix = names(x)[i])
        }
      }
    }
  } else {
    if(length(areanames) == 0){
        areanam <- x$areanam
      }else{
        areanam <- areanames
    }
    if (writetype == "all") {
      .NexusOut(x)
      .WriteTablesSpGeo(x)
      
      if (length(dim(x$coexistence_classified)) == 0) {
        warning("No coexistence matrix found")
      } else {
        .OutHeatCoEx(x, prefix = "")
      }
      
      .OutPlotSpPoly(x, prefix = "")
      .OutBarChartPoly(x, prefix = "")
      .OutBarChartSpec(x, prefix = "")
      .OutMapAll(x, prefix = "")
      .OutMapPerSpecies(x, prefix = "")
      .OutMapPerPoly(x, areanames = areanam, prefix = "")
    }
    if (writetype == "graphs") {
      .OutPlotSpPoly(x, prefix = "")
      .OutBarChartPoly(x, prefix = "")
      .OutBarChartSpec(x, prefix = "")
      .OutMapAll(x, prefix = "")
      .OutMapPerSpecies(x, prefix = "")
      .OutMapPerPoly(x, areanames = areanam, prefix = "")
    }
    if (writetype == "statistics") {
      .WriteTablesSpGeo(x)
    }
    if (writetype == "nexus") {
      .NexusOut(x)
    }
    if (writetype == "coexistence") {
      if (length(dim(x$coexistence_classified)) == 0) {
        print("No coexistence matrix found")
      } else {
        .OutHeatCoEx(x, prefix = "")
      }
    }
  }
} 