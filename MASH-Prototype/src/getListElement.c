///////////////////////////////////////////////////////////////////////////
//      ______                 __     ____
//     / ____/   _____  ____  / /_   / __ \__  _____  __  _____
//    / __/ | | / / _ \/ __ \/ __/  / / / / / / / _ \/ / / / _ \
//   / /___ | |/ /  __/ / / / /_   / /_/ / /_/ /  __/ /_/ /  __/
//  /_____/ |___/\___/_/ /_/\__/   \___\_\__,_/\___/\__,_/\___/
//
//  Sean Wu
//  August 18, 2017
//  A specialized data structure for the MASH project
//  Fork of 'dequer' package for R by Drew Schmidt (license below)
//
///////////////////////////////////////////////////////////////////////////

/*  Copyright (c) 2015, Schmidt
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdio.h>
#include "getListElement.h"

SEXP getListElement(SEXP list, char *str) {
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;
    for ( i = 0; i < length(list); i++ )
        if ( strcmp(CHAR(STRING_ELT(names, i)), str) == 0 ) {
            elmt = VECTOR_ELT(list, i);
            break;
        }

    if ( elmt == R_NilValue )
        error("%s missing from list", str);

    return elmt;
}
