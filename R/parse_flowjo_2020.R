#' Parse_flowjo_2020  function
#'
#' This function replaces parse_flowjo() from FlowSOM package.
#' @param files_path a path  to the directory containing the FCS files
#' @param wsp_file a path to the workspace
#' @param plot Defaults to FALSE
#' @param group Defaults to "All Samples"
#' @examples files_path <- "/Users/christophe/R/2019R/BM84-2/ CA/ CA 2020/FSOM 18"
#' @examples wsp_path <- "/Users/christophe/R/2019R/BM84-2/ CA/ CA 2020/FSOM 18/BM84-2 FS18.wsp"
#' @examples tubes  <- parse_flowjo_2020(files_path, wsp_path, plot = TRUE)
#' parse_flowjo_2020()

parse_flowjo_2020 <- function (files_path, wsp_file, group = "All Samples", plot = FALSE) 
{
  wsp <- CytoML::open_flowjo_xml(wsp_file)
  files <- list.files(files_path, pattern = ".fcs")
  gates <- CytoML::flowjo_to_gatingset(wsp, name = "All Samples", path = files_path)
  result <- list()
  for (file in files) {
    print(paste0("Processing ", file))
    file_id <- which(files==file)
    if (length(file_id) == 0) {
      stop("File not found. Files available: ", gsub("_[0-9]*$", 
                                                     "\n", files_in_wsp))
    }
    gate_names <- flowWorkspace::gs_get_pop_paths(gates[file_id], 
                                                  path = "auto")
    gatingMatrix <- matrix(FALSE, nrow = length(gh_pop_get_indices(gates[file_id], "root")), 
                           ncol = length(gate_names), dimnames = list(NULL, 
                                                                      gate_names))
    for (gate in gate_names) {
      gatingMatrix[, gate] <- gh_pop_get_indices(gates[file_id], gate)
    }
    ff <- gh_pop_get_data(gates[file_id], "root")
    ff@exprs[, "Time"] <- ff@exprs[, "Time"] * 100
    result[[file]] <- list(flowFrame = ff, gates = gatingMatrix)
  }
  if (plot) {
    plot(gates)
  }
  if (length(files) == 1) {
    result <- result[[1]]
  }
  else {
    result <- list(flowSet = flowCore::flowSet(lapply(result, 
                                                      function(x) x$flowFrame)), gates = lapply(result, 
                                                                                                function(x) x$gates))
  }
  return(result)
}
