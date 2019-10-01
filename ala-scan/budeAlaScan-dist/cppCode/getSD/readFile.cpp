/*
 * This file readFile.cpp belongs to Project alaMulModel
 *
 *  Created on: 31 Aug 2016  14:16:05
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"

/**
 * Reads the content of fileName and returns it into fileContent.
 *
 * @param fileContent
 *      Vector holding all line of fileName.
 * @param fileName
 *      Name of the file to read.
 */
void readFile(vector<string>&fileContent, const string fileName) {

  ifstream myFile;

  myFile.open(fileName.c_str());

  if (myFile.is_open()) {
    string myLine;
    while (getline(myFile, myLine)) {
      fileContent.push_back(myLine);
    }
    myFile.close();
  }

}

void printVector(const vector<string>& myVector) {
  for (vector<string>::const_iterator line = myVector.begin();
      line < myVector.end(); line++) {
    cout << *line << "\n";
  }
}

void processResults(const vector<string>& results) {

  vector<string> header;
  string columnsTitle;

  const string interTag("# WT InterDG:"), intraTag("# WT IntraDG:");
  unsigned fileCounter = 0;
  double inter_dG(0.0), intra_dG(0.0);

  vector<AlanineScanRes> avgData;
  vector<vector<double> > models_ddG;
  vector<double> stdDeviation;

  for (vector<string>::const_iterator result = results.begin();
      result < results.end(); result++) {

    vector<string> resultContent;
    readFile(resultContent, *result);

    // A safety catch.
    if (resultContent.size() < COL_DEFINITIONS) {
      cerr << "Result file: " << *result << " has fewer lines than "
          << COL_DEFINITIONS
          << ".\nIt will NOT be process.\n";
      continue;
    }

    unsigned fileIndex = result - results.begin();
    fileCounter++;

    if (fileIndex == 0) {

      columnsTitle = resultContent.at(COL_DEFINITIONS);
      header.insert(header.end(), resultContent.begin(),
          resultContent.begin() + HEADER_END_ROW);

    }

    //------------------------- inter and intra delta G -----------------------
    size_t found = resultContent.at(INTER_ENERGY_ROW).find_last_of(interTag);

    double tmpEnergy(0.0);

    if (found != string::npos) {
      if (!from_string<double>(tmpEnergy,
          resultContent.at(INTER_ENERGY_ROW).substr(found + 1), std::dec)) {
        cerr << "FATAL ERROR: we could read " << interTag << "\n";
        exit(1);
      }
      inter_dG += tmpEnergy;
    }

    found = resultContent.at(INTRA_ENERGY_ROW).find_last_of(intraTag);

    if (found != string::npos) {
      if (!from_string<double>(tmpEnergy,
          resultContent.at(INTRA_ENERGY_ROW).substr(found + 1), std::dec)) {
        cerr << "FATAL ERROR: we could read " << intraTag << "\n";
        exit(1);
      }
      intra_dG += tmpEnergy;
    }

    //---------------------- END inter and intra delta G ----------------------

    processResultData(avgData, models_ddG, resultContent, fileIndex);

  }

  inter_dG /= fileCounter;
  intra_dG /= fileCounter;

  for (vector<AlanineScanRes>::iterator residue = avgData.begin();
      residue < avgData.end(); residue++) {
    *residue /= fileCounter;
  }

  calculateSD(stdDeviation, avgData, models_ddG);

  printVector(header);

  cout << "#\n" << interTag << fixed << setprecision(4)
      << setw(11) << inter_dG << "\n" << intraTag
      << fixed << setprecision(4)
      << setw(11) << intra_dG << "\n#\n" << columnsTitle <<
      setw(7) << "SD" << "\n";

  printAlaScanData(avgData, stdDeviation, cout);

}

