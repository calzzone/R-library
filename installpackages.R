pacs <- readLines("/home/calzzone/Dropbox/Stats/R.packages.txt")
pacs <- sort(unique(pacs))
pacs

download.packages(pacs, "~/R/downloaded_packages/src/contrib/")
tools::write_PACKAGES("/home/calzzone/R/downloaded_packages/src/contrib/", type = "win.binary")

# sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev pandoc
# sudo apt install libudunits2-dev libmagick++-dev
# sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
# sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev 
# sudo apt-get install libgmp3-dev
# sudo apt-get install libpoppler-cpp-dev

# other repos:

if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")
devtools::install_github("ThomasSiegmund/D3TableFilter")
#install.packages("webshot")
webshot::install_phantomjs()
devtools::install_github("odeleongt/postr")
devtools::install_github("strengejacke/strengejacke")
devtools::install_github("benmarwick/wordcountaddin")
devtools::install_github("ropenscilabs/trackmd")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Icens")
BiocManager::install("graph")

# failed to download...
install.packages('maps', "png", "foreach", "nortest", "survey", "bit", "bit64", "blob", "RSQLite", "chron")


# install the other packages 

x <- length(pacs)
i <- 1
last.n <- 0
while (x > 0) {
  print (paste ("pass", i))
  x <- 0
  for (p in pacs) {
    if (!(p %in% installed.packages())) {
      print(p)
      install.packages(p, repos = "file:////home/calzzone/R/downloaded_packages", )
      x <- x+1
    }
  }
  print (paste("pass", i, "tried to install", x, "packages + their dependencies" ))
  if (x == last.n) break;
  last.n <- x
}
print("done")
    

