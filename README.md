# omnom

Omnom the eater of APIs

![Alt Text](https://media.giphy.com/media/jgUG5cnss7T9K/giphy.gif)


## Overview

A HATEOAS API browsing webapp written in Clojurescript.


## Features

* Sends simple GET and POST requests
* Displays/pretty prints responses in a mixture of tables and lists
* Builds requests using an analysis of the API specification


## Dependencies

* Latest version of Google Chrome or Safari
* A [Protean](https://github.com/passivsystems/protean) instance running on port 3000/3001
* A HAL HATEOAS API


## Usage

Figwheel build : lein figwheel dev

Production build : lein cljsbuild once prod
