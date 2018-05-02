#!/usr/bin/env bash
# encoding: utf-8

BYSD_DIR="colourBySD"
DIV_DIR="dividePDB"
GETSD="getSD"

function compUtil {

  cd ${1}
  make clean
  make
  cd ../
  
}

function cleanUtil {

  cd ${1}
  make clean
  cd ../
  
}

function comp_util {

  echo "Compiling ${GETSD}"
  compUtil ${GETSD}

  echo "Compiling ${BYSD_DIR}"
  compUtil ${BYSD_DIR}

  echo "Compiling ${DIV_DIR}"
  compUtil ${DIV_DIR}

  
}

function clean_util {

  echo "Cleaning ${GETSD}"
  cleanUtil ${GETSD}

  echo "Cleaning ${BYSD_DIR}"
  cleanUtil ${BYSD_DIR}

  echo "Cleaning ${DIV_DIR}"
  cleanUtil ${DIV_DIR}

  
}
clean_util
#comp_util
