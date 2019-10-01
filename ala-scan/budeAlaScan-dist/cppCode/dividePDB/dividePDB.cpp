/*
 * This file dividePDB.cpp belongs to Project cppProj
 *
 *  Created on: 25 Apr 2016  11:36:03
 *      Author: Amaurys Ávila Ibarra
 */
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <iomanip>

#include <iostream>

using namespace std;

const string modelTag("MODEL ");
const string endModelTag("ENDMDL");
const string atomTag("ATOM  ");
const string endTag("END");
const unsigned short tagLength(6);
const unsigned short chainIndex = 21;

vector<string> getFileContent(const string &fileName) {

  ifstream myPDB;
  string myLine;
  vector<string> pdbContent;

  myPDB.open(fileName.c_str());

  if (myPDB.is_open()) {
    while (getline(myPDB, myLine)) {
      pdbContent.push_back(myLine);
    }
  } else {
    cerr << "[" << fileName << "] could not be opened for reading.\n";
  }

  return pdbContent;
}

vector<vector<string> > getModels(const vector<string>& pdbContent) {

  bool seenModelTag(false), seenAtomTag(false); //, seenEndModelTag(false);

  vector<vector<string> > myModels;

  vector<string> aModel;
  //cout << "seen model " << seenModelTag << "\n";
  for (vector<string>::const_iterator aLine = pdbContent.begin();
      aLine < pdbContent.end(); aLine++) {

    string tag = aLine->substr(0, tagLength);

    //cout << (aLine - pdbContent.begin()) + 1 << ": " << seenModelTag << "\n";

    if (tag == modelTag) {
      seenModelTag = true;
      continue;
    }

    if (!seenModelTag && tag == atomTag) {
      //cout << "seen model " << seenModelTag << "\n";
      seenAtomTag = seenModelTag = true;

      //continue;
    }

    if (seenModelTag && tag == atomTag) {
      aModel.push_back(*aLine);
    }

    if (tag == endModelTag) {
      seenModelTag = false;
      myModels.push_back(aModel);
      aModel.clear();
      continue;
    }

  }

  if (seenAtomTag) {
//    cout << "We have seen atom tag\n";
    myModels.push_back(aModel);
  }

  return myModels;
}

void printOneChain(vector<string> &model, string partialName) {

  char chain = '\0';

  bool chainDone = false;

  ofstream myChainFile;

  for (vector<string>::iterator atom = model.begin(); atom < model.end();
      atom++) {

    if (atom->at(chainIndex) != chain) {

      chain = atom->at(chainIndex);
      string chainName = partialName + "_Ch" + chain + ".pdb";

      if (chainDone) {

        if (myChainFile.is_open()) {

          myChainFile << left << setfill(' ') << setw(80) << endTag << "\n";
          myChainFile.close();

        } else {
          cout << left << setfill(' ') << setw(80) << endTag << "\n";
        }

      }

      chainDone = true;

      myChainFile.open(chainName.c_str());
    }

    if (myChainFile.is_open()) {
      myChainFile << *atom << "\n";
    } else {
      cout << *atom << "\n";
    }

  }
  if (myChainFile.is_open()) {
    myChainFile << left << setfill(' ') << setw(80) << endTag << "\n";
    myChainFile.close();
  }

}

void dividePDB(const string fileName) {

  string partialName;

  size_t dotPos = fileName.find_last_of('.');

  partialName = dotPos != string::npos ? fileName.substr(0, dotPos) : fileName;

  vector<string> pdbContent = getFileContent(fileName);
  vector<vector<string> > myModels = getModels(pdbContent);

  if (myModels.size() == 1) {

  }
  for (vector<vector<string> >::iterator aModel = myModels.begin();
      aModel < myModels.end(); aModel++) {

    ostringstream modelSuffix;

    unsigned short modelNumber = aModel - myModels.begin() + 1;

    modelSuffix << "_M";

    if (modelNumber < 10)
      modelSuffix << 0;

    modelSuffix << modelNumber;

    printOneChain(*aModel,
        myModels.size() == 1 ? partialName : partialName + modelSuffix.str());

  }

}
