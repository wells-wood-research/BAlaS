/*
 * This file main.cpp belongs to Project alaMulModel
 *
 *  Created on: 31 Aug 2016  13:14:07
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"

using namespace std;

int main(int argc, const char* argv[]) {

  if (argc != 2) {
    cerr << "\nFATAL ERROR: The file name with the list of BUDE Alanine Scan results"

        "\nmust be given.\n\n";

    displayHelp(argv[0]);

    exit(1);
  }

  string listFile = argv[1];

  if (listFile == "-h" || listFile == "-H" || listFile == "--help"){
    displayHelp(argv[0]);
    exit(0);
  }



  vector<string> myFileNames;

  readFile(myFileNames, listFile);

  processResults(myFileNames);


  return 0;
}
