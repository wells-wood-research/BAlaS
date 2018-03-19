/*
 * This file createDDG_script.cpp belongs to Project colourBySD
 *
 *  Created on: 5 Sep 2016  15:56:20
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"

void sortDataValues(const std::vector<SD_ddG_Data>& myRmsfData,
    std::vector<SD_ddG_Data>& positiveValues,
    std::vector<SD_ddG_Data>& negativeValues,
    double& absoluteMax, double& maxSD) {

  double maxPositive(0.0), minNegative(0.0);

  for (std::vector<SD_ddG_Data>::const_iterator residue = myRmsfData.begin();
      residue < myRmsfData.end(); residue++) {

    if (residue->sd > maxSD)
      maxSD = residue->sd;

    // Let's divide them in positive and negative values.
    if (residue->ddG > myZeroValue) {

      positiveValues.push_back(*residue);

    } else if (residue->ddG < -myZeroValue) {

      negativeValues.push_back(*residue);

    }

    // Let's get maximun positive and minimum negative.
    if (residue->ddG > maxPositive) {

      maxPositive = residue->ddG;

    } else if (residue->ddG < minNegative) {

      minNegative = residue->ddG;

    }

  }

  // Let's use the maximum of the modulus |X| of maxNegative or minNegative,
  // Not the reminder.
  absoluteMax = -minNegative;

  if (absoluteMax < maxPositive)
    absoluteMax = maxPositive;

}

void printScriptData(std::ostream &myOutput,
    const std::vector<SD_ddG_Data>& myRmsfData, const double& absMax,
    const bool isPositive) {

  for (std::vector<SD_ddG_Data>::const_iterator residue = myRmsfData.begin();
      residue < myRmsfData.end(); residue++) {

    myOutput << "color " << std::fixed << std::setprecision(4);

    if (isPositive) {

      double ratio = 1.0 - (residue->ddG / absMax);
      myOutput << 1.0 << "," << ratio << "," << ratio;

    } else {

      double ratio = 1.0 + (residue->ddG / absMax);
      myOutput << ratio << "," << ratio << "," << 1.0;

    }

    myOutput << " #0:" << residue->number;

    if (residue->icode >= 'A' && residue->icode <= 'Z') {
      myOutput << residue->icode;
    }

    if (residue->chain >= 'A' && residue->chain <= 'Z') {
      myOutput << "." << residue->chain;
    }

    myOutput << "\n";
  }

}

void createChimeraScript(const string& pdbFileName,
    const string& rmsfFileName,
    const vector<SD_ddG_Data>& myData) {



  vector<SD_ddG_Data> positiveValues, negativeValues;
  bool isPositive(true);
  double absoluteMax(0.0), maxSD(0.0);

  string scriptName(pdbFileName.substr(pdbFileName.find_last_of("\\/")+1));
  replace(scriptName.begin(), scriptName.end(), '.', '_');


  scriptName += "ByddG_SD.com";

  sortDataValues(myData, positiveValues, negativeValues, absoluteMax,
      maxSD);

  ofstream myScriptFile;

  myScriptFile.open(scriptName.c_str());

  if (myScriptFile.is_open()) {

    myScriptFile << "# Chimera script for colouring residues by RMSF.\n"
        "\n# PDB file: pdbFileName.\n# RMSF file: rmsfFileName.\n\n"
        "background solid " << chimeraWhiteColour << "\n\nopen " << pdbFileName
        << "\nopen " << pdbFileName
        << "\n\ncolor " << chimeraWhiteColour << " #0"
        << "\ncolor " << chimeraWhiteColour << " #1\n\n# Positive Values:\n";

    printScriptData(myScriptFile, positiveValues, absoluteMax, isPositive);

    myScriptFile << "\n\n# Negative Values:\n";

    printScriptData(myScriptFile, negativeValues, absoluteMax, !isPositive);



    myScriptFile << "\n\n# SD colouring:\n";

    printScriptDataSD(myScriptFile, myData, maxSD);

    myScriptFile << '\n';

    myScriptFile.close();

  } else {

    cerr << "FATAL ERROR: File: (" << scriptName
        << ") could not be opened for writing.\n";
    exit(EXIT_FAILURE);

  }

  cout << "The Chimera script was written to: " << scriptName << "\n\n";

}

void printScriptDataSD(std::ostream &myOutput,
    const std::vector<SD_ddG_Data>& myData, const double& maxSD) {

  for (std::vector<SD_ddG_Data>::const_iterator residue = myData.begin();
      residue < myData.end(); residue++) {

    if (residue->name == "ALA" || residue->name == "GLY")
      continue;

    double ratio =  (residue->sd / maxSD);
//    ratio += 1;

    myOutput << "color " << std::fixed << std::setprecision(4);
//    myOutput << ratio << "," << 0.0 << "," << ratio ;
    myOutput << 1.0 << "," << ratio << "," << 1.0 ;


    myOutput << " #1:" << residue->number;

    if (residue->icode >= 'A' && residue->icode <= 'Z') {
      myOutput << residue->icode;
    }

    if (residue->chain >= 'A' && residue->chain <= 'Z') {
      myOutput << "." << residue->chain;
    }

    myOutput << "\n";
  }

  myOutput << "color dim grey #1:ala,gly\n";


}
