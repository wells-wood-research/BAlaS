/*
 * This file main.cpp belongs to Project colourBySD
 *
 *  Created on: 5 Sep 2016  12:22:37
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"

void readMyFile(vector<string>& fileContent, const string fileName) {

  ifstream myFile;
  string myLine;

  myFile.open(fileName.c_str());

  if (myFile.is_open()) {

    while (getline(myFile, myLine))
    {

      if (myLine.empty() || myLine.at(0) == '#')
        continue;

      fileContent.push_back(myLine);
    }
    myFile.close();

  } else {
    cerr << "FATAL ERROR: File: (" << fileName
        << ") could not be opened.\n";
    exit(EXIT_FAILURE);
  }
}


bool findMyEntry(vector<string>::const_iterator line1, const vector<string>& myFile){

  bool foundIt= false;

  for (vector<string>::const_iterator line = myFile.begin();
       line < myFile.end() && !foundIt; line++){
    if(*line1 == *line){
      foundIt = true;
    }

  }

  return foundIt;
}

int main(const int argc, const char* argv[]) {
//
//  vector<string> myFile1;
//  vector<string> myFile2;
//
////  bool notFound = true;
//
//  string fName1, fName2;
//
//  fName1 = argv[1];
//  fName2 = argv[2];
//
//
//
//  readMyFile(myFile1, fName1);
//  readMyFile(myFile2, fName2);
//
//  cout << fName1 << "\t" << myFile1.size() << "\n" << fName2 << "\t" << myFile2.size()<< "\n";
//
//
//  for (vector<string>::const_iterator line1 = myFile1.begin();
//      line1 < myFile1.end(); line1++) {
//
//    bool flag = findMyEntry(line1, myFile2);
//
//    if(flag){
//      continue;
//    }else{
//      cout << *line1 << "\n";
//    }
//
//  }
//
//  return 0;

  string pdbFileName, SD_fileName;

  vector<SD_ddG_Data> mySDddG;

  if (isHelpOption(argc, argv)) {
    printHelp(argv[0]);
    return EXIT_SUCCESS;
  }

  parseArguments(argc, argv, pdbFileName, SD_fileName);

  readData(mySDddG, SD_fileName);

  createChimeraScript(pdbFileName, SD_fileName, mySDddG);
//  for (vector<SD_ddG_Data>::const_iterator res = mySDddG.begin();
//      res < mySDddG.end(); res++) {
//
//    cout << res->chain << '\t' << res->ddG << '\t'<< res->index
//        << '\t'<< res->number << '\t'<< res->sd << "\n";
//  }

  return EXIT_SUCCESS;
}

/**
 * Check arguments for correct options and assign the proper value to the
 * variable.
 *
 * @param argc
 *      Number of arguments.
 * @param argv
 *      Arguments.
 * @param pdbFileName
 *      Variable for PDB file name.
 * @param rmsfFileName
 *      Variable for RMSF file Name.
 */
void parseArguments(const int argc, const char* argv[],
    std::string& pdbFileName, std::string& SD_FileName) {

  bool isWrongOption = false;

  for (int i = 1; i < argc && !isWrongOption; i++) {

    if (argv[i] == pdbFileOption) {
      pdbFileName = argv[++i];

    } else if (argv[i] == balsFileOption) {
      SD_FileName = argv[++i];
    } else {
      isWrongOption = true;
      std::cerr << "FATAL ERROR: Unknown Option: (" << argv[i] << ")\n";
      printHelp(argv[0]);
    }

  }

  if (isWrongOption)
    exit(EXIT_FAILURE);

}
