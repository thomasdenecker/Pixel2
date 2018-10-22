FROM rocker/shiny
MAINTAINER Thomas DENECKER (thomas.denecker@gmail.com)

## install R package dependencies (and clean up)
RUN apt-get update && apt-get install -y gnupg2 \
    libssl-dev \
    libpq-dev \
    libv8-dev \
    default-jre \
    r-cran-rjava \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## install packages from CRAN (and clean up)

RUN Rscript -e "install.packages(installed.packages()[,'Package'])"
RUN Rscript -e "install.packages(c('shiny','shinyjs','shinyFiles','evobiR','plotly','ape', 'bPeaks', 'RPostgreSQL', 'rJava', 'mailR', 'shinyalert', 'googleVis', 'shinytest', 'packrat', 'testthat'), repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages from github (and clean up)
RUN Rscript -e "devtools::install_github('rstudio/shinytest','rstudio/webdriver')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install phantomjs
RUN Rscript -e "webdriver::install_phantomjs()"

RUN Rscript -e "install.packages('V8', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('seqinr', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('shinydashboard', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('DT', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('shinyWidgets', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('xlsx', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages('UpSetR', repos='https://cran.rstudio.com/', dependencies = TRUE)" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
