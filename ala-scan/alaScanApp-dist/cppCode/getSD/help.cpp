/*
 * This file help.cpp belongs to Project alaMulModel
 *
 *  Created on: 31 Aug 2016  13:25:49
 *      Author: Amaurys Ávila Ibarra
 */

#include "includes.hpp"


void displayHelp(std::string executable){

  cout << "This utility expects a list of BUDE Alanine Scanning results file.\n"
      "It takes the one argument.\n"

      "\nUsage:\n"
      << executable << " alanineScanResults.list.\n\n";

}
