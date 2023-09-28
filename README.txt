
There is a default.nix file in the src dir which will make all the relevant packages available in your shell. You have to have nix installed (https://nixos.org/download).

The file src/default.nix will also temporarily move your ~/.R/Makevars file and restore it once you exit the shell. This is because, there were some issues with the recommended Makevars settings and the brms version used for this project. To make sure it works as intended, check if the file src/shellExitHook.sh is executable. After you did all that, in the src directory type:

nix-shell --pure

(this can take a while on its first run)

To recreate the project run in the nix-shell (all the source files are in the src dir):

Rscript 01-prepare.R
Rscript 02-regions.R
Rscript 03-models.R


- plots.R -- requires models to be in the dir models (i.e. 03-models.R has to be run)
