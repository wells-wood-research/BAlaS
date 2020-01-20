# BUDE Alanine Scan

This repository contains the code for the BAlaS web application. The application
consists of three services: the front-end server, a service that runs the BudeAlaScan
jobs and a database for the job queue and holding the results. Each of these services
run inside of a Docker container.

## References

[Wood CW et al (2020) BAlaS: fast, interactive and accessible computational alanine-scanning 
using BudeAlaScan, Bioinformatics, btaa026.](https://doi.org/10.1093/bioinformatics/btaa026)

[Ibarra AA et al (2019) Predicting and Experimentally Validating Hot-Spot Residues at Protein
–Protein Interfaces, ACS Chem. Biol., 14, 2252-2263.](https://doi.org/10.1021/acschembio.9b00
560)

## Building and Running

To build and run the application (in development mode), you need to have
[Docker](https://www.docker.com/) installed on your computer. Once Docker is installed,
there are a few steps required to run the application:

1. Clone the repository to your computer (or fork it and clone if you plan to make
   alterations!)
1. Download the Linux version of Scwrl 4 and add it to the `dependencies\_for\_isambard/`
   folder.
1. Download and install the Elm compiler: `npm install -g elm`
1. `cd web/elm-src`
1. `elm make Main.elm --debug --output=../static/elm/bals.js`
1. Run `docker-compose up --build`.

Once these steps have been taken the application should be available on `0.0.0.0:3803`.
This docker-compose.yml creates a shared volume between your computer and the Docker
container, which means that you can modify the web app, recompile the Elm code and fully
refresh your browser (`ctrl+shift+r` generally) and the changes will be live. If you
make changes to the back-end code restart the Docker containers.

Good luck, have fun and feel free to report any issues on the GitHub page.
