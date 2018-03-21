# # do necessary assignments to method functions in mosquitoes.
# # update necessary things based on user input in MBITES:::Parameters
# # in c++ this would be accomplished by pimpl (seems easier than all the possible ways to do inheritance and virtual functions ...)
#
# # oogenesis!!!
#
# MBITES_Setup <- function(
#   # Oogenesis
#   oogenesis_model = 1L,
# ){
#
#   # set flag
#   MBITES:::Globals$set_SETUP(TRUE)
#
#
#   # Oogenesis
#   switch(oogenesis_model,
#     "1" = {
#       # model
#       Mosquito_Female$set(which = "public",name = "oogenesis",
#                 value = mbites_oogenesis1, overwrite = TRUE
#       )
#     },
#     "2" = {
#       # model
#       Mosquito_Female$set(which = "public",name = "oogenesis",
#                 value = mbites_oogenesis2, overwrite = TRUE
#       )
#       # egg provision field
#       Mosquito_Female$set(which = "private",name = "eggP",
#                 value = numeric(1), overwrite = TRUE
#       )
#     },
#     {stop("invalid entry for 'oogenesis_model'\n")}
#   )
#
# }
