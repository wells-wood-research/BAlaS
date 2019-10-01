#!/usr/bin/python3
# encoding: utf-8
'''
Script to process the BUDE Alanine Scanning results. It will take one argument.
a File with a list of the results' file names
'''

import sys
from os.path import basename


class ReadFile(object):
    '''
    Read a file and store it into an array one line per element.
    '''
   
    def readMyFile(self):
        self.myFileContent = []
#         f = open(self.fileName, 'r')
#         self.myFileContent = f.readlines()
        with open(self.fileName) as myFile:
            for line in myFile:
                self.myFileContent.append(line.rstrip("\n"))

    def __init__(self, fileName):
        '''
        Constructor
        '''
        self.fileName = fileName
        
class ProcessAlaResult(object):
    '''
    This clas will take a BUDE Alanine Scanning result file and extract relevant data.
    '''
    def __init__(self, fileName):
        
        self.file_name = fileName
        fileName = basename(fileName.rstrip(".bals"))
        aList = fileName.split('_')
        self.pdb_id = aList[0]
        self.chain_id = aList[2]
        self.dock_id = aList[4]
        self.model_id = aList[1]
#         print( aList)
#         print("%s\t%s\t%a" %(self.pdb_id, self.chain_id, self.dock_id))
        
    def __processResult(self):
        
        self.molecule_results = []
        
        with open(self.file_name) as myResults:
            
            for line in myResults:
                line = line.rstrip()
                
                residue_result = [self.pdb_id, self.chain_id, self.dock_id, self.model_id]
                
                if line.startswith("#"):
                    continue
                
                residue_result.extend(self.__processLine(line))
                
#                 print(residue_result)
                
                self.molecule_results.append(residue_result)
   
    
    def __processLine(self, line):
        my_data = line.split()
        # Index Number Name Chain     InterDG    InterDDG  NormTerDDG
        # IntraDG    IntraDDG  NormTraDDG ChainAtoms
        
#         seq_idx = 0
        resNum_idx = 1
        pdbID_idx = 2
        chain_idx = 3
#         iter_dg_idx = 4
        iter_ddg_idx = 5
        nter_ddg_idx = 6
#         itra_dg_idx = 7
#         itra_ddg_idx = 8
#         ntra_ddg_idx = 9
        atoms_idx = 10

        relevant_data = [my_data[resNum_idx], my_data[pdbID_idx],
                         my_data[chain_idx], my_data[iter_ddg_idx],
                         my_data[nter_ddg_idx], my_data[atoms_idx]]
        
        return relevant_data
  
    
    def getMyResults(self):
        self.__processResult()
        return self.molecule_results    

def main(argv=None):  # IGNORE:C0111
    '''Command line options.'''

def readList(fileName):
    fileList = ReadFile(fileName)
    fileList.readMyFile()
    return fileList.myFileContent


def getAlaScanResults(resultsList):
    
    my_results = []
    for result_file in resultsList:
        a_res = ProcessAlaResult(result_file)
        my_results.append(a_res.getMyResults())
#         print (result_file)
    return my_results

def printMoleculesResults(molecules):

    print("\"PDB_ID\"\t\"Chain\"\t\"DockPos\"\t\"M_Number\"\t\"ResNum\"\t\"ResName\"\t\"ChainID\"\t\"IterDDG\"\t\"NterDDG\"\t\"AtomsSchain\"")
#    ['1BMO', 'ChB', 'L00001', '262', 'LYS', 'B', '0.0000', '0.0000', '4']
    for molecule in molecules:
        for line in molecule:
            print("\"%s\"\t\"%s\"\t\"%s\"\t\"%s\"\t\"%s\"\t\"%s\"\t\"%s\"\t%s\t%s\t%s" % 
                  (line[0], line[1], line[2], line[3], line[4], line[5],
                   line[6], line[7],line[8], line[9]))

if __name__ == "__main__":

    f_list = readList(sys.argv[1]);
    molecules_results = getAlaScanResults(f_list)
    printMoleculesResults(molecules_results)

    sys.exit(main())
