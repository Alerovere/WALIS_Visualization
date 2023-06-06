# WALIS DOCKER

This docker container image allows you to reproduce the merging process of sea-level index points. All datasets and scripts to replicate the code are located in the zip file (`walis_docker.zip`) that you downloaded from the WALIS-Explorer app.

To reproduce the process to create the relative sea-level/age point cloud using Docker, follow these steps:

# 0. Prerequisite:

Install [Docker](https://docs.docker.com/get-docker/)

# 1. Access the `WALIS_docker` folder using the command line interface:

```
cd WALIS_docker
```
# 2. Build the docker image:

```
docker build -f Dockerfile_container -t 'walis-ok' .
```
You can change `walis-ok` for any other name to identify the Docker image


# 3. Run the container:

```
docker run -p 80:80 walis-ok
```
After running the container, the processing would be over.

# 4. Extract the file output

To access the results of the processing, you need to copy the `pointcloud_output.csv` file from the container. For this, you need to identify the `CONTAINER ID`.


```
docker ps -a  

```

This command will show you a list of containers running. Identify the 10-character string `{CONTAINER ID}` that corresponds to the `walis-ok` image.

Then extract the file using the following command:

```
docker cp {CONTAINER ID}:/merging/pointcloud_output.csv .

```

At this point, the `pointcloud_output.csv` file will be available in the `walis_merging` folder.

## Additional analysis

If you want to re-run the analysis with other merging parameters (i.e.,`points per SLIP` and `Sampling strategy`) change the last line of the `Dockerfile`. `Number of points` is an integer. `Sampling strategy` could have multiple values : `T` corresponds to `Peak Sampling`, any other value would result in a `Regular sampling`.

```
CMD Rscript run_analysis.R walis_merging.csv {number of points} {Sampling strategy}
```

## How to cite WALIS

If you use WALIS, we kindly ask you to follow these simple rules to acknowledge those who worked on it:

1) Cite the original authors - Please maintain the original citations of each data point.

2) Acknowledge the database contributor - The name of each contributor is listed in all public data points.

3) Acknowledge the database structure creators - The database template used in this study was developed by the ERC Starting Grant "Warmcoasts" (ERC-StG-802414) and is a community effort under the PALSEA (PAGES / INQUA) working group.

Example of acknowledgments: 

>The data used in this study were [extracted from / compiled in] WALIS, a sea level database developed by the ERC Starting Grant "Warmcoasts" (ERC-StG-802414) and PALSEA (PAGES / INQUA) working group. The database structure was designed by A. Rovere, D. Ryan, T. Lorscheid, A. Dutton, P. Chutcharavan, D. Brill, N. Jankowski, D. Mueller, M. Bartz. The data points used in this study were contributed to WALIS by [list names of contributors here].


## How to cite this tool:

### WALIS visualization interface

> Garzón, Sebastián, & Rovere, Alessio. (2021). WALIS visualization interface (v1.0). Zenodo. https://doi.org/10.5281/zenodo.4943541

### Merging code

> Garzón, Sebastián. 2022. A methodology to compare sea level-index points and sea-level models. MSc Dissertation, Westfälische Wilhelms-Universität Münster. 
