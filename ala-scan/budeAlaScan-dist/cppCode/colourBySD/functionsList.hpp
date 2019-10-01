/*
 * This file functionsList.hpp belongs to Project colourBySD
 *
 *  Created on: 5 Sep 2016  12:51:25
 *      Author: Amaurys Ávila Ibarra
 */
#ifndef SRC_FUNCTIONSLIST_HPP_
#define SRC_FUNCTIONSLIST_HPP_

#include <iostream>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <iomanip>

#include "define_const.hpp"


bool isHelpOption(const int argc, const char* argv[]);

void printHelp(const string executable);

void parseArguments(const int argc, const char* argv[],
    std::string& pdbFileName, std::string& SD_FileName);

void readData(vector<SD_ddG_Data>& myData, const string fileName);


void createChimeraScript(const string& pdbFileName,
    const string& rmsfFileName,
    const vector<SD_ddG_Data>& myRmsfData);

void printScriptDataSD(std::ostream &myOutput,
    const std::vector<SD_ddG_Data>& myData, const double& maxSD);

#endif /* SRC_FUNCTIONSLIST_HPP_ */
