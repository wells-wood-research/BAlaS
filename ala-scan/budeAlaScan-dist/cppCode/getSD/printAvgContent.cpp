/*
 * This file printAvgContent.cpp belongs to Project alaMulModel
 *
 *  Created on: 1 Sep 2016  14:00:27
 *      Author: Amaurys Ávila Ibarra
 */

#include "functionsList.hpp"

void printAlaScanData(const vector<AlanineScanRes> &moleculeData,
    const vector<double>& stdDeviation, ostream &myStream) {

  for (vector<AlanineScanRes>::const_iterator resData =
      moleculeData.begin();
      resData < moleculeData.end(); resData++) {

    string chain("");

    if (resData->chain == ' ')
      chain = "XX";
    else
      chain.push_back(resData->chain);

    myStream << setfill(' ') << right << setw(6)
        << resData->index << " "
        << setw(6) << resData->number << " "
        << setw(4) << resData->name << " "
        << setw(5) << chain << " "
        << fixed << setprecision(4) << setw(11)
        << resData->interDeltaG << " "
        << fixed << setprecision(4) << setw(11)
        << resData->inter_ddG << " "
        << fixed << setprecision(4) << setw(11)
        << resData->normInter_ddG << " "
        << fixed << setprecision(4) << setw(11)
        << resData->intraDeltaG << " "
        << fixed << setprecision(4) << setw(11)
        << resData->intra_ddG << " "
        << fixed << setprecision(4) << setw(11)
        << resData->normIntra_ddG << " "
        << setw(10) << resData->sidechainAtomsNumber
        << fixed << setprecision(4) << setw(11)
        << stdDeviation.at(resData - moleculeData.begin())
        << "\n";

  }
}
