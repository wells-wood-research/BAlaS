/*
 * This file processLine.cpp belongs to Project alaMulModel
 *
 *  Created on: 31 Aug 2016  16:48:28
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"

AlanineScanRes processLine(vector<string>::const_iterator line) {

  std::stringstream stringData;

  string index, number, chain, interDG, interDDG, normTerDDG, intraDG,
      intraDDG, normTraDDG, chainAtoms;

  AlanineScanRes myData;

  stringData.str(*line);

  stringData >> index >> number >> myData.name >> chain >> interDG >> interDDG
      >> normTerDDG >> intraDG >> intraDDG >> normTraDDG >> chainAtoms;

  if (!from_string<unsigned>(myData.index, index, std::dec)) {
    cerr << "FATAL ERROR: we could read first.\n";
    exit(1);
  }
  if (!from_string<unsigned>(myData.number, number, std::dec)) {
    cerr << "FATAL ERROR: we could read number.\n";
    exit(1);
  }

  if (!from_string<unsigned char>(myData.chain, chain, std::dec)) {
    cerr << "FATAL ERROR: we could read chain.\n";
    exit(1);
  }

  if (!from_string<double>(myData.interDeltaG, interDG, std::dec)) {
    cerr << "FATAL ERROR: we could read interDG.\n";
    exit(1);
  }
  if (!from_string<double>(myData.inter_ddG, interDDG, std::dec)) {
    cerr << "FATAL ERROR: we could read interDDG.\n";
    exit(1);
  }
  if (!from_string<double>(myData.normInter_ddG, normTerDDG, std::dec)) {
    cerr << "FATAL ERROR: we could read normTerDDG.\n";
    exit(1);
  }
  if (!from_string<double>(myData.intraDeltaG, intraDG, std::dec)) {
    cerr << "FATAL ERROR: we could read intraDG.\n";
    exit(1);
  }
  if (!from_string<double>(myData.intra_ddG, intraDDG, std::dec)) {
    cerr << "FATAL ERROR: we could read intraDDG.\n";
    exit(1);
  }
  if (!from_string<double>(myData.normIntra_ddG, normTraDDG, std::dec)) {
    cerr << "FATAL ERROR: we could read normTraDDG.\n";
    exit(1);
  }
  if (!from_string<short>(myData.sidechainAtomsNumber, chainAtoms, std::dec)) {
    cerr << "FATAL ERROR: we could read sidechainAtomsNumber.\n";
    exit(1);
  }
  return myData;
}

void processResultData(vector<AlanineScanRes>& avgData,
    vector<vector<double> >& models_ddG, const vector<string> &results,
    const unsigned& fileIndex) {

  vector<string>::const_iterator line, start = results.begin() + DATA_START_ROW;

  vector<double> model_ddG;

  for (line = start; line < results.end(); line++) {
    int dataIndex = line - start;

//    cout << *line << "\n";

    AlanineScanRes myData = processLine(line);

    model_ddG.push_back(myData.inter_ddG);

    if (fileIndex == 0) {
      //we will push it into the vector.
      avgData.push_back(myData);

    } else {

      //Let's check that residue number and name are the same.
      if (myData.number != avgData.at(dataIndex).number
          || myData.name != avgData.at(dataIndex).name) {

        cerr
        << "\n\n\nFATAL ERROR: The models do not agree with the atoms.\n\n\n";
        exit(1);
      } else {
        avgData.at(dataIndex) += myData;
      }
    }
  }
  models_ddG.push_back(model_ddG);
//  cout << "\n\n";

}
