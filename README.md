# A shiny app to visualize rolls in DnD 5e

This repository contains code for a Shiny app that I built to visualize simulated outcomes of attack rolls as a barbarian in DnD 5e. Based on the character's abilities and those of their opponent, a simulation is performed and the character's mean and median damage values are returned alongside a histogram of their damage rolls and an accompanying table of hit statistics.

Very basic stats for the character's opponent are provided with a dropdown menu using data originally drawn from the [Open5e](https://open5e.com/) API. These are stored in the data subdirectory. An external function (attack_function.R) is used to perform attack and damage rolls, and is sourced by the main app.R file.

You can try out the app [here](https://cactusoxbird.shinyapps.io/dd-shiny-sim/). I also wrote an alternative version using `purrr` for the simulation step. The repo for that app is located [here](https://github.com/mbrousil/5e-shiny-sim-purrr).
