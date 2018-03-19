/*
 * This file readData.cpp belongs to Project colourBySD
 *
 *  Created on: 5 Sep 2016  14:09:30
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"


template<typename T>
bool from_string(T& t, const std::string& s,
    std::ios_base& (*f)(std::ios_base&)) {
  std::istringstream iss(s);
  return (!(iss >> f >> t).fail());
}

void processLine(string& line, SD_ddG_Data& data) {

  string index, number, ddG, sd, chain;

  stringstream myDataLine(line);

  // Order of the data in file.
  // Index Number Name Chain InterDG InterDDG NormTerDDG IntraDG IntraDDG
  // NormTraDDG ChainAtoms SD

  myDataLine >> index >> number >> data.name >> chain >> ddG >> ddG
      >> sd >> sd >> sd >> sd >> sd >> sd;

  if (number.empty() || ddG.empty() ||data.name.empty()|| chain.empty() || sd.empty()) {
    cerr << "FATAL ERROR: Line has incomplete data.\n\""
        << line <<
        "\"\nThe residue number, the InterDDG, the chain or SD is missing.\n\n";
    exit(EXIT_FAILURE);
  }

  if (!from_string<unsigned>(data.index, index, dec)) {
    cerr << "FATAL ERROR: Line has wrong value for residue number.\n\""
        << line <<
        "\"\nThe index must an integer.\n\n";
    exit(EXIT_FAILURE);
  }

  if (!from_string<unsigned>(data.number, number, dec)) {
    cerr << "FATAL ERROR: Line has wrong value for residue number.\n\""
        << line <<
        "\"\nThe residue number must an integer.\n\n";
    exit(EXIT_FAILURE);
  }

  if (!from_string<double>(data.ddG, ddG, dec)) {
    cerr << "FATAL ERROR: Line has wrong value for ddG value.\n\""
        << line <<
        "\"\nThe ddG value must a floating point number.\n\n";
    exit(EXIT_FAILURE);
  }


  if (!from_string<double>(data.sd, sd, dec)) {
    cerr << "FATAL ERROR: Line has wrong value for SD value.\n\""
        << line <<
        "\"\nThe SD value must a floating point number.\n\n";
    exit(EXIT_FAILURE);
  }

  if (chain.at(0) >= 'A' || chain.at(0) <= 'Z')
    data.chain = chain.at(0);

//  if (!icode.empty()) {
//    if (icode.at(0) >= 'A' || icode.at(0) <= 'Z')
//      data.icode = icode.at(0);
//
//  }
}

void readData(vector<SD_ddG_Data>& myData, const string fileName) {

  ifstream myFile;
  string myLine;

  myFile.open(fileName.c_str());

  if (myFile.is_open()) {

    while (getline(myFile, myLine))
    {

      if (myLine.empty() || myLine.at(0) == '#')
        continue;

      SD_ddG_Data aDataSet;

      processLine(myLine, aDataSet);
      myData.push_back(aDataSet);

    }
    myFile.close();

  } else {
    cerr << "FATAL ERROR: File: (" << fileName
        << ") could not be opened.\n";
    exit(EXIT_FAILURE);
  }
}
