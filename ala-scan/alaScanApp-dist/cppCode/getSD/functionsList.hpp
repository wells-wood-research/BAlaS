/*
 * This file functionsList.hpp belongs to Project alaMulModel
 *
 *  Created on: 31 Aug 2016  13:14:29
 *      Author: Amaurys Ávila Ibarra
 */
#ifndef FUNCTIONSLIST_HPP_
#define FUNCTIONSLIST_HPP_

#include "includes.hpp"

void displayHelp(std::string executable);

void readFile(vector<string>&fileContent, const string fileName);

void processResults(const vector<string>& results);

void processResultData(vector<AlanineScanRes>& avgData,
    vector<vector<double> >& modelsddG, const vector<string> &results,
    const unsigned& fileIndex);

void printAlaScanData(const std::vector<AlanineScanRes> &moleculeData,
    const vector<double>& stdDeviation, std::ostream &myStream);

void calculateSD(vector<double> &stdDeviation,
    const vector<AlanineScanRes> & avgData,
    const vector<vector<double> > &models_ddG);

#endif /* FUNCTIONSLIST_HPP_ */
