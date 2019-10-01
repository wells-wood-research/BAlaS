/*
 * This file help.cpp belongs to Project colourBySD
 *
 *  Created on: 5 Sep 2016  12:22:47
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"

/**
 * Check that the arguments contains any of the help options.
 *
 * @param argc
 *      Number of arguments.
 * @param argv
 *      Arguments.
 * @return
 *      True if any help option is found.
 */
bool isHelpOption(const int argc, const char* argv[]) {

  bool isHelp = false;

  for (int i = 0; i < argc && !isHelp; i++) {
    if (argv[i] == helpOption1 || argv[i] == helpOption2
        || argv[i] == helpOption3)
      isHelp = true;
  }

  return isHelp;

}

void printHelp(const string executable) {

  cout << "\nThis program will create a Chimera script for colouring\n"
      "residues according to Standard Deviation SD and average ddG.\n\n"
      "Usage: " << executable << " [OPTION] [VALUE]\n\neg:\n\n" << executable
      << ' ' << helpOption1 << "\n"
      << executable << ' ' << pdbFileOption << ' ' << "pdbFileName "
      << balsFileOption << ' ' << "rmsfFileName" << "\n\n"

          "Options:\n" << helpOption1 << ", " << helpOption2 << ", "
      << helpOption3 << "\n\tPrints help and exit.\n" << pdbFileOption << "\n\t"
      << "PDB file name.\n" << balsFileOption << "\n\t"
      << "BUDE's bals-SD file name.\n"
      ;

}
