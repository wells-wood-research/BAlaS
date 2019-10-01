/*
 * This file operators.cpp belongs to Project alaMulModel
 *
 *  Created on: 1 Sep 2016  10:46:10
 *      Author: Amaurys Ávila Ibarra
 */

#include "includes.hpp"

AlanineScanRes AlanineScanRes::operator +(AlanineScanRes myData) const {

  AlanineScanRes myAla = *this;

  myAla.interDeltaG += myData.interDeltaG;
  myAla.inter_ddG += myData.inter_ddG;
  myAla.intraDeltaG += myData.intraDeltaG;
  myAla.intra_ddG += myData.intra_ddG;
  myAla.normInter_ddG += myData.normInter_ddG;
  myAla.normIntra_ddG += myData.normIntra_ddG;

  return myAla;
}

AlanineScanRes AlanineScanRes::operator +=(AlanineScanRes myData) {



  this->interDeltaG += myData.interDeltaG;
  this->inter_ddG += myData.inter_ddG;
  this->intraDeltaG += myData.intraDeltaG;
  this->intra_ddG += myData.intra_ddG;
  this->normInter_ddG += myData.normInter_ddG;
  this->normIntra_ddG += myData.normIntra_ddG;

  return *this;
}

AlanineScanRes AlanineScanRes::operator /(int myInt) const {

  AlanineScanRes myAla = *this;

  myAla.interDeltaG /= myInt;
  myAla.inter_ddG /= myInt;
  myAla.intraDeltaG /= myInt;
  myAla.intra_ddG /= myInt;
  myAla.normInter_ddG /= myInt;
  myAla.normIntra_ddG /= myInt;

  return myAla;
}


AlanineScanRes AlanineScanRes::operator /=(int myInt) {



  this->interDeltaG /= myInt;
  this->inter_ddG /= myInt;
  this->intraDeltaG /= myInt;
  this->intra_ddG /= myInt;
  this->normInter_ddG /= myInt;
  this->normIntra_ddG /= myInt;

  return *this;
}
