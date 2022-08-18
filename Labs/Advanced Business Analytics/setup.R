# Set some global options
options(warn=-1)
options(rgl.useNULL=TRUE) # must be run before library(rgl)
options(scipen=10)
options(repr.matrix.max.cols=74)
# options(jupyter.plot_scale=1)
options(repr.plot.res=600, jupyter.plot_scale=5)
options(repos="http://cran.us.r-project.org") # for install packages
options(stringsAsFactors=TRUE)
suppressMessages(RNGversion("3.6.1")) # ensure consistent use of set.seed across different versions of R


# Install packages one time only
# install.packages("lightgbm")
# install.packages("plyr")
# install.packages("dplyr")

# install.packages("base64enc")
# install.packages("caret")
# install.packages("colorspace")
#install.packages("dummies")
# install.packages("e1071")
# install.packages("entropy")
# install.packages("ggdendro")
# install.packages("GGally")
# install.packages("ggplot2")
# install.packages("gtools")
# install.packages("ggwordcloud")
# install.packages("gridExtra")
# install.packages("htmltools")
# install.packages("igraph")
# install.packages("IRdisplay")
# install.packages("kableExtra")
# install.packages("kknn")
# install.packages("ks")
# install.packages("knitr")
# install.packages("lubridate")
# install.packages("MASS")
# install.packages("mclust")
# install.packages("mvtnorm")
# install.packages("neuralnet")
# install.packages("NLP")
# install.packages("pracma")
# install.packages("polyclip")
# install.packages("psych")
# install.packages("qgraph")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("rgl")
# install.packages("rjson")
# install.packages("rmarkdown")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("simmer")
# install.packages("simmer.plot")
# install.packages("stringr")
# install.packages("SnowballC")
# install.packages("tm")
require(psych)
require(lubridate)
require(reshape2)
require(ggplot2)
require(gridExtra)
require(GGally)
require(rgl)
require(plyr)
require(dplyr)

# Load libraries
# suppressMessages(library(lightgbm)) # must be first library loaded
# suppressMessages(library(plyr))
# suppressMessages(library(dplyr))

# suppressMessages(library(base64enc))
# suppressMessages(library(caret))
# suppressMessages(library(colorspace))
# suppressMessages(library(devtools))
#suppressMessages(library(dummies))
# suppressMessages(library(e1071))
# suppressMessages(library(entropy))
# suppressMessages(library(ggdendro))
# suppressMessages(library(GGally))
# suppressMessages(library(ggplot2))
# suppressMessages(library(gtools))
# suppressMessages(library(ggwordcloud))
# suppressMessages(library(gridExtra))
# suppressMessages(library(htmltools))
# suppressMessages(library(igraph))
# suppressMessages(library(IRdisplay))
# suppressMessages(library(kableExtra))
# suppressMessages(library(kknn))
# suppressMessages(library(ks))
# suppressMessages(library(knitr))
# suppressMessages(library(lubridate))
# suppressMessages(library(MASS))
# suppressMessages(library(mclust))
# suppressMessages(library(mvtnorm))
# suppressMessages(library(neuralnet))
# suppressMessages(library(NLP))
# suppressMessages(library(pracma))
# suppressMessages(library(polyclip))
# suppressMessages(library(psych))
# suppressMessages(library(qgraph))
# suppressMessages(library(readxl))
# suppressMessages(library(reshape2))
#suppressMessages(library(rgl))
# suppressMessages(library(rjson))
# suppressMessages(library(rmarkdown))
# suppressMessages(library(rpart))
# suppressMessages(library(rpart.plot))
# suppressMessages(library(simmer))
# suppressMessages(library(simmer.plot))
# suppressMessages(library(stringr))
# suppressMessages(library(SnowballC))
# suppressMessages(library(tm))



# Set some constants
DAY_NAMES = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
MONTH_NAMES = c("January","February","March","April","May","June","July","August","September","October","November","December")
FFO = "(first few observations)"


# Set some visualization constants
PALETTE = rep(c("#00A9FF", "#FF6347", "#7CAE00", "#C77CFF", "goldenrod2", "chartreuse4", "salmon", "bisque4"),100)
NEW_COLOR = "grey60"
guides.standard = ggplot2::guides(color=ggplot2::guide_legend(override.aes=list(size=2, alpha=1)))
theme.x_axis_only = ggplot2::theme(axis.title.y=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(), axis.ticks.y=ggplot2::element_blank(), panel.grid.major.y=ggplot2::element_blank(), panel.grid.minor.y=ggplot2::element_blank())
theme.y_axis_only = ggplot2::theme(axis.title.x= ggplot2::element_blank(), axis.text.x=ggplot2::element_blank(), axis.ticks.x=ggplot2::element_blank(), panel.grid.major.x=ggplot2::element_blank(), panel.grid.minor.x=ggplot2::element_blank())
theme.x_axis_45 = ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1, vjust=1))
theme.x_axis_90 = ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, hjust=1, vjust=0.5))
theme.no_axes = theme.x_axis_only + theme.y_axis_only
theme.no_axis_titles = ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank())
theme.no_x_axis_title = ggplot2::theme(axis.title.x=ggplot2::element_blank())
theme.legend_title = ggplot2::theme(legend.title=ggplot2::element_text(size=7))
theme.legend_below = ggplot2::theme(legend.position="bottom")
theme.no_legend = ggplot2::theme(legend.position="none")
standard_arrow = ggplot2::arrow(angle=15, type="closed", length=ggplot2::unit(10, "points"))
restore = NULL


# Define some useful functions

as.binary = function(v, s) { as.numeric(v == s) }

as.class = function(prob, class, cutoff=0.5)
# applicable only for 2 classes
# returns vector of logical if prob columns are FALSE and TRUE, else returns vector of factor
  { f = prob[,class]>=cutoff
    if (dimnames(prob)[[2]][1] == class) f = factor(f, levels=c(TRUE, FALSE), labels=c(class, setdiff(colnames(prob),class)))
    if (dimnames(prob)[[2]][2] == class) f = factor(f, levels=c(FALSE, TRUE), labels=c(setdiff(colnames(prob),class), class))
    if (all(colnames(prob) %in% c("FALSE","TRUE"))) { f = f=="TRUE" }
    attr(f, "names") = NULL
    f }

	
as.covariance_matrix = function(sd, correlation) { matrix(c(sd[1]^2, prod(sd)*correlation, prod(sd)*correlation, sd[2]^2), 2, 2) }

as.data.frame.content = function(x) { d = as.data.frame(unlist(lapply(1:length(x), function(i) x[[i]]$content))); colnames(d) = "V1"; d }

boundary_line = function(support_vector)
  { edge.1 = support_vector[support_vector$class == levels(support_vector$class)[1], ]
    edge.2 = support_vector[support_vector$class == levels(support_vector$class)[2], ]
    if (nrow(edge.1)==1) { temp = edge.1; edge.1 = edge.2; edge.2 = edge.1 }
    model = lm(as.formula(paste0(colnames(edge.1)[2], "~", colnames(edge.1)[1])), data=edge.1)
    i1 = as.numeric(model$coefficient[1])
    m = as.numeric(model$coefficient[2])
    i3 = as.numeric(edge.2[1,2] - m*edge.2[1,1])
    i2 = mean(c(i1, i3))
    data.frame(intercept=i2, slope=m) }

clock.start = function() options("start_time" = Sys.time())
clock.stop  = function() { x = Sys.time() - getOption("start_time")
                           IRdisplay::display_html(sprintf("<i>Elapsed time %0.2f %s</i>", as.numeric(x), attr(x, "units"))) }

custom_name_repair = function(x) { x = gsub("\\((.+)\\)", "\\.\\1", x)
                                   x = gsub(" ", "\\.", x)
                                   x = gsub("@", "", x)
                                   x }

dummify = function(...) suppressWarnings(dummy.data.frame(..., drop=FALSE, omit.constants=FALSE))

exhaustive = function(v, keep="class")
  { v = v[!(v %in% keep)]
    x = list()
    for (r in 1:length(v))
      { d = combinations(n=length(v), r=r, v=v)
        x = append(x, alply(1:nrow(d), 1, function(i) paste(d[i,], sep=","))) }
    for (i in 1:length(x)) { x[[i]] = append(x[[i]], keep) }
    unname(x) }

factor_reset = function(d) { a_ply(colnames(d), 1, function(cn) { if (all(is.na(d[,cn]))) d[,cn] <<- as.logical(d[,cn])
                                                                  if (is.factor(d[,cn])) d[,cn] <<- factor(as.character(d[,cn]), levels=sort(unique(as.character(d[,cn])))) } )
                             d }
	
fmt = function(x, title=NULL, position="center", row.names=FALSE, force=FALSE, adjust=FALSE, highlight_cells=NULL)
  { object = as.character(sys.call(sys.parent()))[2]
    if (is.null(title)) title = object
    else if (!is.na(title) & title==FFO) title = paste(sub("\\[.+\\]", "", object), FFO) 
    else if (!is.na(title) & title=="") title = "&nbsp;"
	
	xtype = is.null(colnames(x))
   
    if (!xtype & is.na(title) & !force) { caption = NULL }
    if (!xtype & is.na(title) & force)  { caption = "&nbsp;" }
    if (!xtype & !is.na(title))         { caption = title }
    if (xtype & is.na(title) & !force)  { caption = NULL; cn = " " }
    if (xtype & is.na(title) & force)   { caption = "&nbsp;"; cn = " " }
    if (xtype & !is.na(title) & !force) { caption = NULL; cn = title }
    if (xtype & !is.na(title) & force)  { caption = "&nbsp;"; cn = title }
    if (xtype & !is.na(title) & force & adjust)  { caption = title; cn = "&nbsp;" }
   
    if (!is.null(caption)) caption = paste0("<", position, "><font color=\"black\"><b>", caption, "</b></font></", position, ">")
        
    x = as.data.frame(x); if (xtype) colnames(x) = cn 
    num_rows = nrow(x)
    for (j in colnames(x)) { if (is.factor(x[,j])) x[,j] = as.character(x[,j]) }
   
    if (!is.null(highlight_cells))
      { if (!is.list(highlight_cells)) highlight_cells = list(highlight_cells)
        for (e in highlight_cells) 
          { if (e[1] == 0) colnames(x)[e[2]] = cell_spec(colnames(x)[e[2]], color="red", bold=TRUE)
            if (e[2] == 0) rownames(x)[e[1]] = cell_spec(rownames(x)[e[1]], color="red", bold=TRUE)
            else x[e[1],e[2]] = cell_spec(x[e[1],e[2]], color="red", bold=TRUE) } }
   
  
    x =  kableExtra::kable(x, escape=FALSE, format="html", align=rep("r", ncol(x)), caption=caption, row.names=row.names, table.attr="style=\"white-space: nowrap; background: white; color: black;\"")
    if (row.names) x = kableExtra::column_spec(x, 1, bold=TRUE)
    x = kableExtra::row_spec(x, 0, background="#FFFFFF", color="black")
    for (row in num_rows) {
      x = kableExtra::row_spec(x, row, background="#FFFFFF", color="black")
    }
    
    if (as.character(sys.call(-1))[1] == "eval") IRdisplay::display_html(as.character(x))
    else x }

fmt.cm = function(cm, ...) { m = as.data.frame(matrix(cm, nrow=nrow(cm), ncol=ncol(cm), dimnames=attr(cm, "dimnames")))
                             x = fmt(m, ..., row.names=TRUE)
                             if (as.character(sys.call(-1))[1] == "eval") display_html(as.character(x))
                             else x}

fmt.dm = function(dm) { d = as.data.frame(as.matrix(dm))
                        d1 = adply(1:ncol(d), 1, function(j) sprintf("%0.4f", d[,j]))
                        d1 = d1[, -1]
                        row.names(d1) = row.names(d)
                        names(d1) = names(d)
                        d1 }
						
fmt.model= function(model, ...) { m = data.frame(method=model$method[1],
                                                 vars=paste(model$vars, collapse=", "),
                                                 parameter_1=model$parameter_1[1], parameter_2=model$parameter_2[1])
								  if (class(model) == "my_classifier_construction_methodClass") m$classes = paste(as.character(model$classes), collapse=", ")
                                  x = fmt(m, ..., row.names=FALSE)
                                  if (as.character(sys.call(-1))[1] == "eval") IRdisplay::display_html(as.character(x))
                                  else x}

fmt.text = function(text, bold=TRUE) IRdisplay::display_html(paste0("<p style=\"font-size: 12px\">", ifelse(bold,"<b>",""), text))

fmt.toc = function(a, detail=FALSE)
  { spaces = function(n) paste0(rep("&nbsp;", n), collapse="")
    if (detail) { pad1 = "<br><br>"; pad2 = "<br>"; emphasize.on = "<b>"; emphasize.off = "</b>" } else { pad1 = "<br>";     pad2 = "";     emphasize.on = "";    emphasize.off = "" }

    p = ""
    heading = ""

    for (e in names(a))
      { heading.new = sub("/[^/]+", "", e)
   
        if (heading.new != heading) { if (heading != "") p = paste0(p, pad1)
                                      heading = heading.new
                                      h = if (grepl("^[0-9]+ ", heading)) paste0(sub("^([0-9]+) .+$", "\\1", heading), "-") else ""
                                      p = paste0(p, emphasize.on, heading, emphasize.off, "<br>") }

        if (grepl("/", e)) { subheading = sub("^.+/", "", e)
                             p = paste0(p, pad2, spaces(6), emphasize.on, h, subheading, emphasize.off, "<br>") }
  
        if (detail)
          for (ee in a[[e]]){  if (grepl("^[0-9]+ ", ee))                         p = paste0(p, spaces(12), ee, "<br>")
                               else if (grepl("^[0-9]+.[0-9]+ ", ee))             p = paste0(p, spaces(18), ee, "<br>")
                               else if (grepl("^[0-9]+.[0-9].[0-9]+ ", ee))       p = paste0(p, spaces(24), ee, "<br>")
                               else if (grepl("^[0-9]+.[0-9].[0-9].[0-9]+ ", ee)) p = paste0(p, spaces(30), ee, "<br>") } }

    IRdisplay::display_html(p) }
	
focus_data = function(data, hit, emphasis=10) { prob.x = as.numeric(as.character(factor(hit, levels=c(TRUE, FALSE), labels=c(1,emphasis))))
                                                prob = prob.x / sum(prob.x)
                                                data[sample(1:nrow(data), replace=TRUE, prob=prob),] }

front = function(d, class=NULL) { x = colnames(d)
                                  if (is.null(class)) d[, c(tail(x,1), head(x,-1))] 
                                  else                d[, c(class, setdiff(x, class))] }

gather.toc = function(root = "..", ext=".ipynb", hide=c())
  { f = str_sort(list.files(root, recursive=TRUE, pattern=paste0("*",ext)), numeric=TRUE)
    for (h in hide) { f = f[grep(paste0("^",h,ext,"$"), f, invert=TRUE)]
                      f = f[grep(paste0("/",h,ext,"$"), f, invert=TRUE)] }

    a = list()
    for (i in paste0(root, "//", f))
      { x = fromJSON(file=i)
        y = compact(llply(x$cells, function(e) if (e$cell_type=="markdown") e$source else NULL))
        y = compact(llply(y, function(e) { ee = grep("^##+", e, value=TRUE); if (length(ee)!=0) ee else NULL } ))
        y = llply(y, function (e) sub("\\n", "", e))
        a[[i]] = y }
              
    names(a) = sub("([^/]+)/(\\1)$", "\\1", sub("\\.ipynb", "", f))
              
    a = llply(a, function(e) as.list(unlist(llply(e, function(ee) as.list(ee) ))))
                                              
    a = llply(a, function (e)
                   { b = list(); h1 = 0; h2 = 0; h3 = 0; h4 = 0
                     for (ee in e)
                       { if      (grepl("^## ",    ee)) { h1=h1+1; b = c(b, sub("^## ",    paste0(h1, " "), ee));                           h2=0; h3=0; h4=0 }
                         else if (grepl("^### ",   ee)) { h2=h2+1; b = c(b, sub("^### ",   paste0(h1, ".", h2, " "), ee));                  h3=0; h4=0 }
                         else if (grepl("^#### ",  ee)) { h3=h3+1; b = c(b, sub("^#### ",  paste0(h1, ".", h2, ".", h3, " "), ee));         h4=0 }
                         else if (grepl("^##### ", ee)) { h4=h4+1; b = c(b, sub("^##### ", paste0(h1, ".", h2, ".", h3, ".", h4, " "), ee)) } }  
                      b } )
                                              
    a }

gaussian = function(x, mean, sd, size=1) { size * (1/(sd*sqrt(2*pi))) * exp(-0.5 * ((x-mean)/sd)^2) }

gaussian2 = function(grid, mean, cm, size=1) { size * plyr::aaply(1:nrow(grid), 1, function(i) { dmvnorm(c(grid[i,1],grid[i,2]), mean=mean, sigma=cm) } ) }

get_domains = function(d) { l = alply(colnames(d), 1, function(cn) { if (is.factor(d[,cn])) levels(d[,cn]) else NA } )
                            names(l) = colnames(d)
                            l }

get.cor = function(cm) { cm[1,2] / sqrt(cm[1,1]) / sqrt(cm[2,2]) }

get.sd = function(cm) { c(sqrt(cm[1,1]), sqrt(cm[2,2])) }

ggtree = function(model, cutoff=0.5, fill=PALETTE[1:2], topbump=0.15, bottombump=0.20, leftbump=0.15, rightbump=0.15)
  { segments     = dendro_data(model, uniform=TRUE)$segments
    split_labels = dendro_data(model, uniform=TRUE)$labels
    leaf_labels  = dendro_data(model, uniform=TRUE)$leaf_labels
                                                   
    i = model[["frame"]][,1] == "<leaf>"
    n = model[["frame"]][i,"n"]
    dev = model[["frame"]][i,"dev"]
                                        
    if (model$method=="class") 
      { level = attr(model, "ylevels")
        class.predicted = level[model[["frame"]][i,"yval"]]
        prob.predicted = (n-dev)/n
        prob.1 = ifelse(class.predicted==level[1], prob.predicted, 1-prob.predicted)
        prob.2 = ifelse(class.predicted==level[2], prob.predicted, 1-prob.predicted)
                           
        leaf_labels$label = sprintf("n=%s\n%s %0.2f  %s %0.2f", n, level[1], prob.1, level[2], prob.2)
        leaf_labels$fill = fill[as.numeric(!(prob.1 >= cutoff))+1] }
                                       
     if (model$method=="anova") 
       { leaf_labels$label = sprintf("n=%s\n%s", n, leaf_labels$label)
         leaf_labels$fill = fill[1] }
                                         
         ggplot() + 
         geom_segment(aes(x=x, y=y, xend=xend, yend=yend), data=segments) + 
         geom_label(aes(x=x, y=y, label=label), data=split_labels, size=2.5) +
         geom_label(aes(x=x, y=y, label=label, fill=fill), data=leaf_labels, size=2.5) +
         scale_y_continuous(expand=expand_scale(mult=c(bottombump,topbump))) +
         scale_x_continuous(expand=expand_scale(mult=c(leftbump,rightbump))) +
         theme.no_axes + theme.no_legend }

impute.factor = function(v, iv) { factor(plyr::aaply(as.character(v), 1, function(x) if (is.na(x)) as.character(iv) else x), levels(v)) }
impute.numeric = function(v, iv) { plyr::aaply(v, 1, function(x) if (is.na(x)) iv else x) }

impute = function(d)
  { l = alply(colnames(d), 1, function(cn) { v = d[, cn]
                                             m = if (is.numeric(v)) mean(v, na.rm=TRUE) else mode(v)
                                             v[is.na(v)] = m
                                             v } )
    d.impute = as.data.frame(l)
    colnames(d.impute) = colnames(d)
    d.impute }

get_impute = function(d)
  { ml = alply(colnames(d), 1, function(cn) { v = d[, cn]; m = if (is.numeric(v)) mean(v, na.rm=TRUE) else mode(v); m } )
    names(ml) = colnames(d)
    ml }

put_impute = function(d, ml)
  { l = alply(colnames(d), 1, function(cn) { v = d[, cn]
                                             if (is.factor(v)) v = factor(v, levels=union(v, ml[[cn]]))
                                             v[is.na(v)] = ml[[cn]]
                                             v } )
    d.impute = as.data.frame(l)
    colnames(d.impute) = colnames(d)
    d.impute }

inside = function(point, polygon)
  { P = list(x=point[,1], y=point[,2])
    A = list(x=polygon[,1], y=polygon[,2])
    as.logical(abs(pointinpolygon(P,A, eps=0.001))) }

kernel.gauss = function(x, mean, sd, n) { (1/(sd*sqrt(2*pi))) * exp(-0.5 * ((x-mean)/sd)^2) / n }

kernel.rect = function(x, center, halfwidth, n) { z=rep(0, length(x)); z[(x >= center-halfwidth) & (x <= center+halfwidth)] = 1 / (n*2*halfwidth); z }

kernel.tri = function(x, center, halfwidth, n)
  { hw = halfwidth;
    z = rep(0,length(x));
    i = which((x >= center) & (x <= center+hw));
    z[i] = plyr::aaply(i, 1, function(ii) {(((center+hw)-x[ii])/hw)*(1/hw)} );
    j = which((x >= center-hw) & (x < center));
    z[j] = plyr::aaply(j, 1, function(jj) {((x[jj]-(center-hw))/hw)*(1/hw)} );
    z = z/n 
    z }

layout = function(..., nrow=1, ncol=NULL, headings=NULL, padding=40)
  { data = list(...)
    
    n = length(data)
    if (is.null(ncol)) ncol = (n %/% nrow) + (n - ((n %/% nrow) * nrow))
    nempty = ncol*nrow - n
    data = append(data, as.list(rep("&nbsp;", nempty)))

    tr = ""
    
    if (!is.null(headings))
      { td = ""
       for (j in 1:ncol) 
          { td = paste0(td, "<td style=\"background-color:white; vertical-align:top; text-align:center;", 
                        if (j<ncol) paste0(" padding-right:", padding, "px;") else "", "\">", paste0("<b>",headings[j],"</b>\n<hr style=\"height: 1px;\">"), "</td>") }
        tr = paste0(tr, "<tr>", td, "</tr>") }
   
    k = 0
    for (i in 1:nrow)
      { td = ""
        for (j in 1:ncol) 
          { k = k+1
            if ((tail(class(data[[k]]),1)!="knitr_kable") & is.null(attr(data[[k]], "plain"))) data[[k]] = fmt(data[[k]], NA) 
            td = paste0(td, "<td style=\"background-color:white; vertical-align:top;", 
                        if (j<ncol) paste0(" padding-right:", padding, "px;") else "", "\">", data[[k]], "</td>") }
        tr = paste0(tr, "<tr>", td, "</tr>") }  
    
    IRdisplay::display_html(paste0("<table>", tr, "</table>")) }

lookahead = function(d, k, suffix=".LA")
  { h = d[(k+1):nrow(d), ]
    t = as.data.frame(matrix(NA, k, ncol(d))); names(t) = names(d)
    r = rbind(h, t); names(r) = paste0(names(d), suffix, k)
    r }
	
lookback = function(d, k, suffix=".LB")
  { h = as.data.frame(matrix(NA, k, ncol(d))); names(h) = names(d) 
    t = d[1:(nrow(d)-k), ]
    r = rbind(h, t); names(r) = paste0(names(d), suffix, k)
    r }

mandelbrot = function(xmin=-2, xmax=1, ymin=-1.5, ymax=1.5, resolution=500)
  { x = seq(xmin, xmax, length.out=resolution)
    y = seq(ymin, ymax, length.out=resolution)
    z = matrix(0.0, nrow=resolution, ncol=resolution)
    k = matrix(0.0, nrow=resolution, ncol=resolution)
    c = outer(x, y*1i, FUN="+")
    for (rep in 1:50) { i = Mod(z) < 2; z[i] = z[i]^2 + c[i]; k[i] = k[i] + 1 }
    m = list(); m[["matrix"]] = k; m[["x"]] = x; m[["y"]] = y
    m }

mode = function(v) { tbl = as.data.frame(table(v)); tbl[tbl[,2]==max(tbl[,2]),1][1] }

# obsolete this
my_predict = function(model, data) { p = if (sum(data[,1])>15) c(0.8,0.3,0.6,0.2,0.8) else if (sum(data[,1])>10) c(0.6,0.9,0.6,0.1,0.1) else c(0.4,0.3,0.6,0.2,0.7); p = p[1:nrow(data)]; d = data.frame(p, 1-p); colnames(d) = attr(model, "vars"); d }

# obsolete this
my_predictive_analytic_method = function(formula, data, hyperparameter) { x="A model constructed by my predictive analytic method"; attr(x, "vars") = unique(data$class); x }
my_classifier_construction_method = function(formula, data, hyperparameter) { structure(list(method="Special Classifier Construction Method",
                                                                                              parameter_1 = hyperparameter/2,
                                                                                              parameter_2 = mean(abs(data[,1])),
																							  # vars = as.character(formula)[-(1:2)],
																							  vars = strsplit(as.character(formula)[-(1:2)], " \\+ ")[[1]],
                                                                                              classes = unique(data$class)),
                                                                                        class = "my_classifier_construction_methodClass") }

my_regressor_construction_method = function(formula, data, hyperparameter) { structure(list(method="Special Regressor Construction Method",
                                                                                            parameter_1 = as.numeric(lm(outcome ~ x1, data)$coefficients[1]),
                                                                                            parameter_2 = as.numeric(lm(outcome ~ x1, data)$coefficients[2]),
																							vars = as.character(formula)[-(1:2)]),
																				        class = "my_regressor_construction_methodClass") }

mystery_process_A = c(0, 0.5, 0.5, 0, 0, 0)

mystery_process_B.function = function(x) { (-3.3003730075023 + 25.7489500997727*x + -14.8400555259565*x^2 + 3.15566230978222*x^3 + -0.308590022994962*x^4 + 0.014011451172589*x^5 + -0.000238775338979748*x^6 + 10) / 232.881735467096} 

mystery_process_B = c(plyr::aaply(0:19, 1, function(x) integrate(mystery_process_B.function,x,x+1)$value ), 0)

nosort = function(v) factor(v, v)

out = function(width=8, height=3) options(repr.plot.width=width, repr.plot.height=height)
output_size = function(width=8, height=3) options(repr.plot.width=width, repr.plot.height=height)

pc_constituents = function(pc)
  { pc_names = names(as.data.frame(pc$rotation))
    qual = as.data.frame(t(adply(pc_names, 1, function(j) row.names(pc$rotation)[rev(order(abs(pc$rotation[,j])))])[-1]))
    names(qual) = pc_names
    row.names(qual) = NULL
    qual }

# Obsolete this in favor of which.max.df
pick = function(d, f, labels=colnames(d)) { factor(plyr::aaply(d, 1, function(v) colnames(d)[v == max(v)], .expand=FALSE), levels=colnames(d), labels=labels) }

plain_var_names = function (d) { colnames(d) = gsub(" ", "_", colnames(d)); d }

preamble = function(...) { x = as.data.frame(list(...)); IRdisplay::display_html(as.character(fmt(x, NA))) }

predict.cf = function(data, similarity, threshold=2, alpha=1)
  { prediction = data
    
    for (i in 1:nrow(data))
      for (item in 1:ncol(data))
        { nn = which(rownames(data) %in% names(sort(similarity[-i,i], decreasing=TRUE))[1:threshold])
          weight = similarity[i,nn]
          rating = data[nn,item]
          mean_rating.nn = rowMeans(data[nn,], na.rm=TRUE)
          diff = rating - mean_rating.nn
          weighted_mean_part = weighted.mean(diff, weight, na.rm=TRUE)
        
          mean_rating.i = rowMeans(data[i,], na.rm=TRUE)
        
          prediction[i,item] = mean_rating.i + (alpha*weighted_mean_part) }

    prediction }

predict.my_classifier_construction_methodClass = function(model, new_data) { p = plyr::aaply(new_data[,1], 1, function(x) max(0,min(1,(abs(x)/model$parameter_2)*max(0,min(1, model$parameter_1*2))))) 
                                                                             d = data.frame(p, 1-p)
                                                                             colnames(d) = model$classes
                                                                             d }

predict.my_regressor_construction_methodClass = function(model, new_data) { y = plyr::aaply(new_data[,1], 1, function(x) model$parameter_1 + model$parameter_2*x) 
                                                                            y }

predict.random_interventionClass = function(model, new_data) { prob.risk = sample(0:1, nrow(new_data), replace=TRUE, prob=c(1-model$intervention_rate, model$intervention_rate))
                                                               prob.safe  = 1 - prob.risk
                                                               data.frame(safe=prob.safe, risk=prob.risk) }

random_intervention = function(formula, data, intervention_rate) { structure(list(method="Random Intervention",
                                                                                  intervention_rate = intervention_rate,
                                                                                  vars = unique(data$Class)),
                                                                                  class = "random_interventionClass") }

rbind.dup = function(data, n) { data.dup = data.frame(); for (i in 1:n) data.dup = rbind(data.dup, data); data.dup }

#obsolete this for make_columns_agree
regulate_columns = function(new.t, data.t) { i = intersect(colnames(data.t), colnames(new.t))
                                             j = setdiff(colnames(data.t), intersect(colnames(data.t), colnames(new.t)))
                                             d = as.data.frame(matrix(rep(0, nrow(new.t)*length(j)), nrow=nrow(new.t)))
                                             colnames(d) = j
                                             x = cbind(new.t[,i], d)
                                             x = x[,sort(colnames(x))]
                                             x }
make_columns_agree = function(new.t, data.t) { i = intersect(colnames(data.t), colnames(new.t))
                                               j = setdiff(colnames(data.t), intersect(colnames(data.t), colnames(new.t)))
                                               d = as.data.frame(matrix(rep(0, nrow(new.t)*length(j)), nrow=nrow(new.t)))
                                               colnames(d) = j
                                               x = cbind(new.t[,i], d)
                                               x = x[,sort(colnames(x))]
                                               x }

remove_na_rows = function(d) d[as.logical(alply(data.jobs, 1, function(x) any(plyr::aaply(1:6, 1, function(j) !(is.na(x[1,j])))))),]

removeSpecialChars = tm::content_transformer(function(x, chars) gsub(paste("[", chars, "]", sep=""), "", x))

round_robin = function(...) { tt = table(as.character(lapply(lapply(list(...), pick, max), as.character)))
                              factor(names(tt)[which.max(tt)], levels=unique(unlist(lapply(list(...), colnames)))) }

sd_columns = function(data) plyr::aaply(1:ncol(data), 1, function(j) sd(data[,j])) # obsolete this

# Obsolete these in favor of var_info...
select_if.index = function(data, f, ...) as.vector(which(plyr::aaply(names(data), 1, function(j) f(data[, j], ...))))              
select_if.name = function(data, f, ...) names(data)[plyr::aaply(names(data), 1, function(j) f(data[, j], ...))]
select_if_na.index = function(data, threshold) select_if.index(data, function(x) length(which(is.na(x)))/length(x) >= threshold)
select_if_na.name  = function(data, threshold) select_if.name(data, function(x) length(which(is.na(x)))/length(x) >= threshold)

selective = function(v, keep="class") { v = v[!(v %in% keep)]
                                        x = as.list(combinations(length(v),1,v))
                                        for (i in 1:length(x)) { x[[i]] = c(keep[-length(keep)], x[[i]], keep[length(keep)]) }
                                        x }

shift_down = function(v, i) { c(v[(i+1):length(v)], rep(NA, i)) }

shift_up = function(v, i) { c(rep(NA, i), v[1:(length(v)-i)])  }

size = function(data) data.frame(observations=dim(data)[1], variables=dim(data)[2])

sortby = function (v1, v2, decreasing = FALSE) factor(v1, v1[!duplicated(v1)][order(v2[!duplicated(v1)], decreasing = decreasing)])

stretch = function(values, n, each=1, last=NULL) { values.n = rep(values, each=each, length.out=n)
                                                   if (!is.null(last)) values.n[length(values.n)] = last 
                                                   values.n }

string2vector = function(s) unlist(strsplit(as.character(s), ", "))

sum_kernels.tri = function(x, x1, halfwidth, n) { plyr::aaply(x, 1, function(i) { sum(plyr::aaply(x1, 1, function(j) kernel.tri(i, j, halfwidth, n))) } ) }

sum_kernels.gauss = function(x, x1, sd, n) { plyr::aaply(x, 1, function(i) { sum(plyr::aaply(x1, 1, function(j) kernel.gauss(i, j, sd, n))) } ) }

sum_kernels.rect = function(x, x1, halfwidth, n) { plyr::aaply(x, 1, function(i) { sum(plyr::aaply(x1, 1, function(j) kernel.rect(i, j, halfwidth, n))) } ) }

svm_margin = function(edge.1, edge.2)
  { edge.1 = data.frame(edge.1); edge.2 = data.frame(edge.2)
    if (nrow(edge.1)==1) { temp = edge.1; edge.1 = edge.2; edge.2 = edge.1 }
    model = lm(as.formula(paste0(colnames(edge.1)[2], "~", colnames(edge.1)[1])),
               data=edge.1)
    i1 = as.numeric(model$coefficient[1])
    m = as.numeric(model$coefficient[2])
    i3 = as.numeric(edge.2[1,2] - m*edge.2[1,1])
    i2 = mean(c(i1, i3))
    list(i1=i1, i2=i2, i3=i3, m=m) }

table.df = function(x, col.names=NULL)
  { d = data.frame(names(table(path)), as.vector(table(path)))
    if (!is.null(col.names)) colnames(d) = col.names
    d }

table_rel.df = function(x, col.names=NULL)
  { d = data.frame(names(table(path)), as.vector(table(path))/sum(table(path)))
    if (!is.null(col.names)) colnames(d) = col.names
    d }

unibigrams = function(x) unlist(lapply(ngrams(words(x), c(1,2)), paste, collapse=" "))                                                                    

var_columns = function(data) aaply(1:ncol(data), 1, function(j) var(data[,j])) # obsolete this

# !!!!! change name var_info to traverse
var_info.sd = function(data, labels=TRUE) { x = plyr::aaply(colnames(data), 1, function(cn) if (is.numeric(data[,cn])) sd(data[,cn], na.rm=TRUE) else NA)
                                           names(x) = colnames(data)
                                           if (labels) x else as.vector(x) }

var_info.var = function(data, labels=TRUE) { x = plyr::aaply(colnames(data), 1, function(cn) if (is.numeric(data[,cn])) var(data[,cn], na.rm=TRUE) else NA)
                                             names(x) = colnames(data)
                                             if (labels) x else as.vector(x) }

var_info.na_count = function(data, labels=TRUE) { x = plyr::aaply(colnames(data), 1, function(cn) length(which(is.na(data[,cn]))))
                                                  names(x) = colnames(data)
                                                  if (labels) x else as.vector(x) }

var_info.type = function(data, labels=TRUE) { x = plyr::aaply(colnames(data), 1, function(cn) class(data[,cn]))
                                              names(x) = colnames(data)
                                              if (labels) x else as.vector(x) }									 

var_info.unique = function(data, labels=TRUE) { x = plyr::aaply(colnames(data), 1, function(cn) length(unique(data[,cn])))
                                                names(x) = colnames(data)
                                                if (labels) x else as.vector(x) }
												
												
traverse.prob_nonzero = function(data, labels=TRUE) { v = as.numeric(colwise(function(x) if (is.numeric(x)) length(which(x!=0))/length(x) else NA)(data)[1,])
                                                      names(v) = colnames(data)
                                                      if (labels) v else as.vector(v) }
										   
vector2string = function(v) paste(v, collapse=", ")

viz.tree = function(model, ...) prp(model, cex.main=0.7, uniform=TRUE, split.yshift=-4, yes.text="  yes  ", no.text="  no ",
                                    branch=1, under.percent=1, yspace=9, space=5, split.space=5, gap=10,
                                    extra=ifelse(model$method=="anova", 1, 104),
								    box.palette=if (model$method=="anova") PALETTE[1] else PALETTE[1:2],
									varlen=0, cex=0.6, 
                                    border.col=0, round=0, split.font=0, split.box.col="gray", roundint=FALSE, ...)

vote = function(...) { x = list(...); factor(levels(unlist(x))[which.max(table(unlist(x)))], levels=levels(unlist(x))) }

which.max.df = function(d) { p = factor(plyr::aaply(1:nrow(d), 1, function(i) names(which.max(d[i,]))), levels=colnames(d))
                             names(p) = NULL
                             p }

youtube = function(id, width=500, height=500) { IRdisplay::display_html(paste0('<iframe src =https://www.youtube.com/embed/', id, ' width=', width, ', height=', height, ' allowfullscreen></iframe> ' )) }

zoom = function(polygon, xlim=c(0,1), ylim=c(0,1))
  { mx = 0.001*abs(xlim[2]-xlim[1])
    my = 0.001*abs(ylim[2]-ylim[1])
    A = list(x=polygon[,1], y=polygon[,2])
    B = list(x=xlim[c(1,2,2,1)]+c(mx,-mx,-mx,mx), y=ylim[c(1,1,2,2)]+c(my,my,-my,-my))
    X = polyclip(A, B, op="intersection")
    polygon.zoom = as.data.frame(X[[1]]); names(polygon.zoom) = names(polygon)
    polygon.zoom }
	

# Set some visualization formatting defaults
out(restore)
ggplot2::update_geom_defaults("abline",     list(color="black", size=0.15))
ggplot2::update_geom_defaults("area",       list(color=NA, fill=PALETTE[1]))
ggplot2::update_geom_defaults("bar",        list(color=NA, fill=PALETTE[1], lwd=5))
ggplot2::update_geom_defaults("col",        list(fill=PALETTE[3], color=NA, lwd=5))
ggplot2::update_geom_defaults("density",    list(size=3, fill=PALETTE[1], colour=NA))
ggplot2::update_geom_defaults("density_2d", list(size=0.1, colour="black"))
ggplot2::update_geom_defaults("hline",      list(color="black", size=0.15))
ggplot2::update_geom_defaults("label",      list(colour="black"))
ggplot2::update_geom_defaults("line",       list(size=0.5, colour=PALETTE[1]))
ggplot2::update_geom_defaults("path",       list(size=0.5, colour=PALETTE[1]))
ggplot2::update_geom_defaults("point",      list(size=3, colour=PALETTE[1]))
ggplot2::update_geom_defaults("segment",    list(size=0.5, colour=PALETTE[1]))
ggplot2::update_geom_defaults("smooth",     list(color="black", size=0.15))
ggplot2::update_geom_defaults("text",       list(size=2.5, vjust=-1.5))
ggplot2::update_geom_defaults("violin",     list(color=NA, lwd=5))
ggplot2::update_geom_defaults("vline",      list(color="black", size=0.15))
#ggplot2::update_geom_defaults("vline",      list(size=0.1, colour="black"))
scale_colour_discrete = function(...) scale_colour_manual(..., values=PALETTE)
scale_fill_discrete   = function(...) scale_fill_manual(..., values=PALETTE)
# theme_update(plot.title=element_text(size=10, hjust=0.5), plot.subtitle=element_text(size=8, face="italic", hjust=0.5), axis.title=element_text(size=7), axis.text=element_text(size=7), strip.text=element_text(size=7), strip.text.y=element_text(angle=90), legend.title=element_blank(), legend.text=element_text(size=7))
ggplot2::theme_update(plot.title=ggplot2::element_text(size=9, hjust=0.5, face="plain"), plot.subtitle=ggplot2::element_text(size=7, face="italic", hjust=0.5), axis.title=ggplot2::element_text(size=7), axis.text=ggplot2::element_text(size=7), strip.text=ggplot2::element_text(size=7), strip.text.y=ggplot2::element_text(angle=90), legend.title=ggplot2::element_blank(), legend.text=ggplot2::element_text(size=7))


igraph::igraph_options(vertex.size=30, vertex.size2=15, vertex.shape="rectangle", vertex.frame.color="gray60",
               vertex.label.cex=0.6, vertex.label.color="black", vertex.label.family="sans",
               edge.color="gray40")

