/* 
 * File:   main.cpp
 * Author: amit
 *
 * Created on September 21, 2011, 2:41 PM
 */

#include <cstdlib>
#include <iostream>
#include <cstring>
#include <ctime>

#include "MacroSimulator.h"

const char* getDirAppend() {
	const char* dirappend;
#if defined(_WIN32)
        dirappend = "\\";
#elif defined(__MACH__)
        dirappend = "/";
#elif defined(__UNIX__)
        dirappend = "/";
#else
		dirappend = "/";
#endif
return dirappend;
}


using namespace std;

/*
 * 
 */
int main(int argc, char** argv) {
    srand(time(0));
    char *configFileName;
	char *strAppend;

    if (argc < 2) { // Check for number of input arguments
        cout << "\nError! Not enough arguments. Please specify the configuration file.";
        exit(1); // Exit if not enough arguments
    }
    else {
        configFileName = argv [1];
		if (argc == 3)
			strAppend = argv[2];
		else strAppend = "";
    }

    MacroSimulator *macrosim = new MacroSimulator((char*)configFileName, (char*)getDirAppend(), strAppend);
    macrosim->readSimControlParameters();
    //cout << "\n\nJust made the world!\n++++++++++++++++++++++++++++++++++++++++++++\n\n\n";
    //macrosim->printObjects();
    macrosim->simulate();

    cout << "\n\n\n\tSimulation Completed!\n\n\n\n\n\n";

    return 0;
}

