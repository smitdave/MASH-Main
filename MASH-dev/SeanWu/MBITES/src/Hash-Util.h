/* ################################################################################
#       __  __           __    __  ___
#      / / / /___ ______/ /_  /  |/  /___ _____
#     / /_/ / __ `/ ___/ __ \/ /|_/ / __ `/ __ \
#    / __  / /_/ (__  ) / / / /  / / /_/ / /_/ /
#   /_/ /_/\__,_/____/_/ /_/_/  /_/\__,_/ .___/
#                                      /_/
#     Hash Table
#     MBITES Team
#     March 2018
#
################################################################################ */

#ifndef HASH_UTIL
#define HASH_UTIL

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* ################################################################################
 * BEGIN UTILITY FUNCTIONS
 * copied from: https://github.com/wch/r-source/blob/trunk/src/main/envir.c
 * because they are not part of the user-facing C API for R
################################################################################ */

#define NONEMPTY_(_FRAME_) \
    CHAR(PRINTNAME(TAG(_FRAME_)))[0] != '.' && CAR(_FRAME_) != R_UnboundValue

static int FrameSize(SEXP frame, int all)
{
    int count = 0;
    if (all) {
	while (frame != R_NilValue) {
	    count += 1;
	    frame = CDR(frame);
	}
    } else {
	while (frame != R_NilValue) {
	    if (NONEMPTY_(frame))
		count += 1;
	    frame = CDR(frame);
	}
    }
    return count;
}


#define CHECK_HASH_TABLE(table) do {		\
	if (TYPEOF(table) != VECSXP)		\
	    error("bad hash table contents");	\
    } while (0)

static int HashTableSize(SEXP table, int all)
{
    CHECK_HASH_TABLE(table);
    int count = 0;
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	count += FrameSize(VECTOR_ELT(table, i), all);
    return count;
}

static void FrameValues(SEXP frame, int all, SEXP values, int *indx)
{
    if (all) {
	while (frame != R_NilValue) {
#         define DO_FrameValues						\
	    SEXP value = CAR(frame);					\
	    if (TYPEOF(value) == PROMSXP) {				\
		PROTECT(value);						\
		value = eval(value, R_GlobalEnv);			\
		UNPROTECT(1);						\
	    }								\
	    SET_VECTOR_ELT(values, *indx, lazy_duplicate(value));	\
	    (*indx)++

	    DO_FrameValues;
	    frame = CDR(frame);
	}
    } else {
	while (frame != R_NilValue) {
	    if (NONEMPTY_(frame)) {
		DO_FrameValues;
	    }
	    frame = CDR(frame);
	}
    }
}
#undef DO_FrameValues

static void HashTableValues(SEXP table, int all, SEXP values, int *indx)
{
    CHECK_HASH_TABLE(table);
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameValues(VECTOR_ELT(table, i), all, values, indx);
}

/* ################################################################################
 * END UTILITY FUNCTIONS
################################################################################ */

#endif
