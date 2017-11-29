# Demonstrate how to pass arguments to an R script when executing from the command line
# Try executing in the command line: r < foo.r 32
# The script will print out the command line arguments, 
# an output filename subscripted with "seed", 
# and create a blank file with that name

print(commandArgs())

# Read in the seed from the command line
seed <- as.integer(commandArgs()[4])
if (!is.na(seed)) {
  print(seed) } else {
    print("Not an integer")
    seed <- 0 # set default seed
  }

outfilename = paste0("foo_outputs/foo_output_", as.character(seed),".rds")

# Print this out
print(outfilename)

# Check that the output file appeared
saveRDS("Blank output", outfilename)
