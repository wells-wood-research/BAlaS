/*
 * This file includes.hpp belongs to Project alaMulModel
 *
 *  Created on: 31 Aug 2016  13:23:29
 *      Author: Amaurys Ávila Ibarra
 */
#ifndef INCLUDES_HPP_
#define INCLUDES_HPP_

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <iomanip>

using namespace std;

/**
 * This template will convert from a string into a another type. It will return
 * true if the conversion is successful otherwise false.
 *
 * @param t
 *    Variable where the value from the string will be converted to.
 * @param s
 *    The string in question holding a value.
 * @param f
 *    Base to which it should be converted.
 * @return
 *    Whether the conversion is successful or not.
 */
template<typename T>
bool from_string(T& t, const std::string& s,
    std::ios_base& (*f)(std::ios_base&)) {
  std::istringstream iss(s);
  return (!(iss >> f >> t).fail());
}

#define COL_DEFINITIONS 19
#define INTER_ENERGY_ROW 16
#define INTRA_ENERGY_ROW 17
#define HEADER_END_ROW 15
#define DATA_START_ROW 20

/**
 * Structure for Alanine Scanning and Internal energies for a residue.
 */
struct AlanineScanRes {

  // The number of atoms in the side chain
  short sidechainAtomsNumber;
  // Index in which the residue occurs in the structural file.
  unsigned index;
  // Residue number in the structural file.
  unsigned number;
  // Three-letters code for the residue.
  std::string name;
  // Chain the residue belong to.
  unsigned char chain;
  // icode for the residue.
  unsigned char icode;
  // Delta G for receptor and ligand.
  double interDeltaG;
  // Delta-Delta G for the mutant molecule-Wild Type and Wild Type-Wild type
  double inter_ddG;
  // Normalised ddg for the number of atoms in side chain.
  double normInter_ddG;
  // Internal Energy for mutant molecule.
  double intraDeltaG;
  // Delta-Delta G for mutant and wild type molecule.
  double intra_ddG;
  // Normalised ddg for mutant molecule by the number of atoms in side chain.
  double normIntra_ddG;
  AlanineScanRes operator +(AlanineScanRes myData) const;
  AlanineScanRes operator +=(AlanineScanRes myData);
  AlanineScanRes operator /(int myInt) const;
  AlanineScanRes operator /=(int myInt);

  AlanineScanRes() :
      sidechainAtomsNumber(0), index(0), number(0), name(""), chain(' '),
          icode(' '), interDeltaG(0.0), inter_ddG(0.0), normInter_ddG(0.0),
          intraDeltaG(0.0), intra_ddG(0.0), normIntra_ddG(0.0) {

  }
  ~AlanineScanRes() {

  }

};

#endif /* INCLUDES_HPP_ */
