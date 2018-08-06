##' Convert to a DZSlides presentation
##'
##' Format for converting from R Markdown to a DZSlides presentation.
##' @param incremental should lists be incremental?
##' @param fig_width figure width
##' @param fig_height figure height
##' @param fig_retina retina figure?
##' @param fig_caption figure caption?
##' @param dev figure device
##' @param smart smart?
##' @param self_contained self contained?
##' @param highlight highlighting style
##' @param mathjax mathjax
##' @param template template path
##' @param css extra css
##' @param includes extra includes
##' @param keep_md keep markdown file?
##' @param lib_dir ...
##' @param md_extensions markdown extensions to use
##' @param pandoc_args extra pandoc arguments
##' @param ... other arguments
##' @return R Markdown output format to pass to \code{\link{render}}
##' @author Christoffer Moesgaard Albertsen
##' @importFrom rmarkdown beamer_presentation pdf_document html_document pandoc_path_arg
##' @examples
#' \dontrun{
#' 
#' library(rmarkdown)
#' 
#' # simple invocation
#' render("pres.Rmd", dzslides_presentation())
#' 
#' # specify an option for incremental rendering
#' render("pres.Rmd", dzslides_presentation(incremental = TRUE))
#' }
##' ##' @export
dzslides_presentation <- function(incremental = FALSE,
                                  fig_width = 8,
                                  fig_height = 6,
                                  fig_retina = if (!fig_caption) 2,
                                  fig_caption = FALSE,
                                  dev = 'png',
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  highlight = "default",
                                  mathjax = "default",
                                  template = "default",
                                  css = NULL,
                                  includes = NULL,
                                  keep_md = FALSE,
                                  lib_dir = NULL,
                                  md_extensions = NULL,
                                  pandoc_args = NULL,
                                  ...) {

  # base pandoc options for all reveal.js output
  args <- c()

  # template path and assets
  if (identical(template, "default")){
    args <- c(args, "--template",
              rmarkdown::pandoc_path_arg(system.file("rmd","dzslides","default.html",package="caMisc")))
  }else if (file.exists(system.file("rmd","dzslides",template,package="caMisc"))) {
      args <- c(args, "--template",
                rmarkdown::pandoc_path_arg(system.file("rmd","dzslides","default.html",package="caMisc")))
  }else if (!is.null(template))
    args <- c(args, "--template", rmarkdown::pandoc_path_arg(template))

  # incremental
  if (incremental)
    args <- c(args, "--incremental")
  
  # content includes
  args <- c(args, rmarkdown::includes_to_pandoc_args(includes))

  # additional css
  for (css_file in css)
    args <- c(args, "--css", rmarkdown::pandoc_path_arg(css_file))
  
  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

   ##  # use files_dir as lib_dir if not explicitly specified
  ##   if (is.null(lib_dir))
  ##     lib_dir <- files_dir

  ##   # extra args
  ##   args <- c()

  ##   # slidy
  ##   slidy_path <- rmarkdown_system_file("rmd/slidy/Slidy2")
  ##   if (!self_contained)
  ##     slidy_path <- normalized_relative_to(
  ##       output_dir, render_supporting_files(slidy_path, lib_dir))
  ##   args <- c(args, "--variable", paste("slidy-url=",
  ##                                       pandoc_path_arg(slidy_path), sep=""))

  ##   # highlight
  ##   args <- c(args, pandoc_highlight_args(highlight, default = "pygments"))

  ##   # return additional args
  ##   args
  }

                                        # return format
    rmarkdown::output_format(
                   knitr = rmarkdown::knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev),
    pandoc = rmarkdown::pandoc_options(to = "dzslides",
                            from = rmarkdown::from_rmarkdown(fig_caption, md_extensions),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    base_format = rmarkdown::html_document_base(smart = smart, lib_dir = lib_dir,
                                     self_contained = self_contained,
                                     mathjax = mathjax,
                                     bootstrap_compatible = TRUE, 
                                     pandoc_args = pandoc_args, ...))
}
