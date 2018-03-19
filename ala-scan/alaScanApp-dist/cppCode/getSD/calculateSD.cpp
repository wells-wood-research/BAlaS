/*
 * This file calculateSD.cpp belongs to Project alaMulModel
 *
 *  Created on: 2 Sep 2016  15:35:10
 *      Author: Amaurys Ávila Ibarra
 */

#include <cmath>

#include "functionsList.hpp"

void calculateSD(vector<double> &stdDeviation,
    const vector<AlanineScanRes> & avgData,
    const vector<vector<double> > &models_ddG) {

  unsigned modelsNumber = models_ddG.size();

  for (vector<AlanineScanRes>::const_iterator residueData = avgData.begin();
      residueData < avgData.end(); residueData++) {

    unsigned residueIndex = residueData - avgData.begin();

    double sumSD(0.0);

    for (unsigned i = 0; i < modelsNumber; i++) {

      sumSD += pow((models_ddG.at(i).at(residueIndex) - residueData->inter_ddG),
          2);

    }

    stdDeviation.push_back(sqrt((sumSD / modelsNumber)));

  }

}
