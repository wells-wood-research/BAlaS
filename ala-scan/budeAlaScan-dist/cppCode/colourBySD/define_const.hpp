/*
 * This file define_const.hpp belongs to Project colourBySD
 *
 *  Created on: 5 Sep 2016  13:26:43
 *      Author: Amaurys Ávila Ibarra
 */
#ifndef SRC_DEFINE_CONST_HPP_
#define SRC_DEFINE_CONST_HPP_

#include <string>

using namespace std;

const double myZeroValue(.00001);

const string helpOption1("-h");
const string helpOption2("-H");
const string helpOption3("--help");
const string pdbFileOption("-p");
const string balsFileOption("-r");

const string chimeraWhiteColour("1.0,1.0,1.0");

struct SD_ddG_Data{

  // Index in which the residue occurs in the structural file.
  unsigned index;
  // Residue number in the structural file.
  unsigned number;
  // Delta delta G
  double ddG;
  double sd;
  // Chain the residue belong to.
  unsigned char chain;
  // icode for the residue. We should leave it for now.
  unsigned char icode;

  string name;


  SD_ddG_Data() :
      index(0), number(0), ddG(0.0), sd(0.0), chain(' '), icode(' ') {

  }

  ~SD_ddG_Data() {

  }
};

#endif /* SRC_DEFINE_CONST_HPP_ */
