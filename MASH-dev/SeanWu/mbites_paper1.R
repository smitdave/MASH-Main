###############################################################################
#           __  ___      ____  _____________________
#         /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \ 
#     / /  / /_____/ /_/ // /  / / / /___ ___/ / 
#   /_/  /_/     /_____/___/ /_/ /_____//____/  
#
#   M-BITES Manuscript File 1
#   Simulations on pointsets
#   MASH Team
#   January 2018
#
###############################################################################

rm(list=ls());gc()
set.seed(42L)

files_dir = "/Users/slwu89/Desktop/git/MASH-Main-slwu89/MASH-dev/DavidSmith/MBITES-Demo/"
files_inDir = system(command = paste0("ls ",files_dir),intern = TRUE)
f_files = files_inDir[grep(pattern = "peridom.f",x = files_inDir)]
l_files = files_inDir[grep(pattern = "peridom.l",x = files_inDir)]

xy_lscape = vector(mode = "list",length = length(f_files))

for(i in 1:length(xy_lscape)){
  xy_lscape[[i]]$f = read.table(file = paste0(files_dir,f_files[i]),header = TRUE,sep = ",",stringsAsFactors = FALSE)
  xy_lscape[[i]]$l = read.table(file = paste0(files_dir,l_files[i]),header = TRUE,sep = ",",stringsAsFactors = FALSE)
}


