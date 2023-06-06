# Base docker image
FROM --platform=linux/amd64 rocker/shiny:4.1.3


RUN apt-get update && apt-get install -y \
	software-properties-common

CMD Rscript r/run_analysis.R Data/walis_merging.csv 200 F
RUN add-apt-repository ppa:ubuntugis/ppa 

RUN apt-get update

# Install required libraryes including GDAL development libraries
RUN apt-get update && apt-get install -y \
	sudo\
	python3.6\
	python3-pip\
  	python3-gdal \
	gdal-bin\
	libgdal-dev\
	libudunits2-dev\
    	libcurl4-gnutls-dev \
    	libssl-dev

RUN ogrinfo --version

RUN pip3 install --upgrade setuptools pip

# Install required libraryes including geoextent and GDAL
RUN pip3 install pygdal==$(gdal-config --version).* 


# Define RENV version
ENV RENV_VERSION 0.14.0

# Expose 

EXPOSE 80

# Install renv

RUN R -e 'install.packages("renv")'

# Copy content

COPY . shiny-server

CMD ls

# Define Workdir

WORKDIR shiny-server

# Start shiny=server as the 'shiny' user rather than root

# Set an environment variable to tell shiny-server to log errors

ENV SHINY_LOG_STDERR=1

CMD ls

RUN R -e "renv::restore()"

CMD ls

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]

