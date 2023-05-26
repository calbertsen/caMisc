##' @export
minislide_presentation <- function (fig_width = 8, fig_height = 6, 
                                    fig_retina = if (!fig_caption) 2, fig_caption = FALSE, dev = "png", 
                                    smart = TRUE, self_contained = TRUE, highlight = "default", 
                                    mathjax = "default", template = "default", css = NULL, includes = NULL, 
                                    keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, 
                                    ...) 
{
    args <- c()
    args <- c(args, "--section-divs")
    if (identical(template, "default")) {
        args <- c(args, "--template", rmarkdown::pandoc_path_arg(system.file("rmd/dzslides/default.minislide", 
            package = "caMisc")))
    }
    else if (!is.null(template)) 
        args <- c(args, "--template", rmarkdown::pandoc_path_arg(template))
    if (!identical(highlight, "default") & !is.null(highlight))
        args <- c(args, "--highlight-style", highlight)
     args <- c(args, rmarkdown::includes_to_pandoc_args(includes))
    for (css_file in css) args <- c(args, "--css", rmarkdown::pandoc_path_arg(css_file))
    pre_processor <- function(metadata, input_file, runtime, 
        knit_meta, files_dir, output_dir) {
    }
    rmarkdown::output_format(knitr = rmarkdown::knitr_options_html(fig_width, 
        fig_height, fig_retina, keep_md, dev), pandoc = rmarkdown::pandoc_options(to = "html5", 
        from = rmarkdown::from_rmarkdown(fig_caption, c(md_extensions,"+smart")), 
        args = args), keep_md = keep_md, clean_supporting = self_contained, 
        pre_processor = pre_processor, base_format = rmarkdown::html_document_base(smart = FALSE, 
            lib_dir = lib_dir, self_contained = self_contained, 
            mathjax = mathjax, bootstrap_compatible = FALSE, pandoc_args = pandoc_args, 
            ...))
}



##' @export
get_minislide_template <- function(path = "."){
    from_path <- system.file("rmd","minislide", package = "caMisc")
    files <- list.files(from_path)
    file.copy(file.path(from_path,files),
              file.path(path,files))
}
