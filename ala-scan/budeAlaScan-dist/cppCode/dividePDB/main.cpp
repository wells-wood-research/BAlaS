/*
 * This file main.cpp belongs to Project cppProj
 *
 *  Created on: 11 Apr 2016  15:30:25
 *      Author: Amaurys Ávila Ibarra
 */
#include <iostream>
#include <string>

#define EXECUTABLE 0

using namespace std;

void printHelp(const string executable);
void dividePDB(const string executable);

int main(int argc, char **argv) {

  if (argc == 1) {
    printHelp(argv[EXECUTABLE]);
  } else {

    for (int i = 1; i < argc; i++) {

      dividePDB(argv[i]);

    }
  }

  return 0;
}

void printHelp(const string executable) {

  cout << "This program '" << executable
      << "' is expecting at least a PDB file.\n"
          "It will create individual files for each model chain combination.\n"
          "Only PDB files can be given to this utility.\n\ne.g: "
      << executable << " file1.pdb fileN.pdb\n\n"
      ;

}
