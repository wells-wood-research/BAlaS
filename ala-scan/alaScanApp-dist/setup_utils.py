#!/usr/bin/env python3
# encoding: utf-8
'''
Created on 21 Jul 2017

@author: Amaurys Ávila Ibarra
'''

from configparser import ConfigParser, ExtendedInterpolation
import os, sys
import shutil
from time import strftime, localtime

from plotly.api.v2.users import current


try:
    assert sys.version_info >= (3, 5)
except Exception as e:
    print("We shall NOT continue.\nWe need python 3.5 for using the Alanine Scanning App.\n")
    print(sys.version)
    print("\n")
    sys.exit(2)



cpp_dir = "cppCode"
bysd_dir = "colourBySD"
div_dir = "dividePDB"
build_dir = "build"
getsd_dir = "getSD"
bude_dir = "BUDE-1.2.9"
tgz_ext = ".tar.gz"
bude_tgz = bude_dir + tgz_ext
app_tgz = "alaScanApp.tar.gz"

tar_opt = "-xzvf"

current_dir = os.getcwd()

ala_scan_name = "ALAscanApp.py"
replot_py_name = "replotAlaScan.py"

scan_exe = "%s/alaScanApp/%s" % (current_dir, ala_scan_name)
replot_exe = "%s/alaScanApp/%s" % (current_dir, replot_py_name)

# These variables are not meant to be changed
__ini_file = "%s/.alascanapp.ini" % (os.path.expanduser("~"))

# Ini file sections
__exe_sect = 'Executables'
__dir_sect = 'Directories'
__names_sect = 'File Names'
__general_sect = 'General'

# Ini file General options.
__gen_max_auto_num_opt = "MaxAutoNumber"
__gen_max_auto_num_val = '20'

# ini file dir options
__dir_wrk_opt = "Work"
__dir_wrk_val = "%s/ALAscanApp" % (os.path.expanduser("~"))
__dir_scan_opt = "Scan"
__dir_scan_val = "alaScan"

__dir_blibs_opt = "BudeLibs"
__dir_blibs_val = "libs"
__dir_rslt_opt = "BudeResults"
__dir_rslt_val = "results"

__dir_src_opt = "PdbSources"
__dir_src_val = "sources"

__dir_chi_opt = "Chimera"
__dir_chi_val = "chimeraScripts"

__dir_l_src_opt = "LongPdbSources"
__dir_l_src_val = "${%s}/${%s}" % (__dir_scan_opt, __dir_src_opt)

__dir_l_blibs_opt = "LongBudeLibs"
__dir_l_blibs_val = "${%s}/${%s}" % (__dir_scan_opt, __dir_blibs_opt)

__dir_l_rslt_opt = "LongResults"
__dir_l_rslt_val = "${%s}/${%s}" % (__dir_scan_opt, __dir_rslt_opt)

__dir_l_chi_opt = "LongChimera"
__dir_l_chi_val = "${%s}/${%s}" % (__dir_scan_opt, __dir_chi_opt)

__dir_cpp_opt = "CppCode"
__dir_cpp_val = "%s/%s" % (current_dir, cpp_dir) 

__dir_bude_opt = "BUDE"
__dir_bude_val = "${%s}/%s/%s/bin" % (__dir_cpp_opt, bude_dir, build_dir)

__dir_getsd_opt = "GetSD"
__dir_getsd_val = "${%s}/%s" % (__dir_cpp_opt, getsd_dir)

__dir_colsd_opt = "ColourBySD"
__dir_colsd_val = "${%s}/%s" % (__dir_cpp_opt, bysd_dir)

# Ini file executable options
__exe_bude_opt = "BUDE"
__exe_bude_val = "budeScan"
 
__exe_colsd_opt = "ColouBySD"
__exe_colsd_val = "colourBySD"
 
__exe_getsd_opt = "GetSD"
__exe_getsd_val = "getSD"

__exe_fbude_opt = "FullBude"
__exe_fbude_val = "${%s:%s}/${%s}" % (__dir_sect, __dir_bude_opt, __exe_bude_opt)
 
__exe_fcolsd_opt = "FullColourSD"
__exe_fcolsd_val = "${%s:%s}/${%s}" % (__dir_sect, __dir_colsd_opt, __exe_colsd_opt)
 
__exe_fgetsd_opt = "FullGetSD"
__exe_fgetsd_val = "${%s:%s}/${%s}" % (__dir_sect, __dir_getsd_opt, __exe_getsd_opt)

# Ini file names options
__names_b_bmc_opt = "BudeBMC"
__names_b_bmc_val = "alaScanApp.bemc"
__names_b_ctrl_opt = "BudeCtrl"
__names_b_ctrl_val = "alaScan.bctl"
__names_b_gen_zero_opt = "BudeGenZero"
__names_b_gen_zero_val = "zeroGeneration.bzgn"
__names_b_ff_opt = "BudeForcefield"
__names_b_ff_val = "forcefield.bhff"
__names_b_transf_opt = "BudeTransformations"
__names_b_transf_val = "transformations.bltr"
__names_ligs_list_opt = "LigandsList"
__names_ligs_list_val = "myLigands.list"
__names_rcpts_list_opt = "ReceptorsList"
__names_rcpts_list_val = "myReceptors.list"
__names_rslt_lig_list_opt = "LigandResultsList"
__names_rslt_lig_list_val = "resultsLigands.list"
__names_rslt_rcpt_list_opt = "ReceptorResultsList"
__names_rslt_rcpt_list_val = "resultsReceptors.list"
__names_lig_sd_opt = "LigandsAvgSD"
__names_lig_sd_val = "ligandsAvgSD.bals"
__names_rcpt_sd_opt = "ReceptorsAvgSD"
__names_rcpt_sd_val = "receptorsAvgSD.bals"


__config_sections = {
    __general_sect:
     {
        __gen_max_auto_num_opt: __gen_max_auto_num_val
     }
    ,
    __exe_sect:
     {
        __exe_bude_opt: __exe_bude_val,
        __exe_colsd_opt: __exe_colsd_val,
        __exe_getsd_opt: __exe_getsd_val,
        __exe_fbude_opt: __exe_fbude_val,
        __exe_fcolsd_opt: __exe_fcolsd_val,
        __exe_fgetsd_opt: __exe_fgetsd_val
     }
    ,
    __dir_sect:
     {
        __dir_wrk_opt: __dir_wrk_val, __dir_scan_opt: __dir_scan_val,
        __dir_blibs_opt: __dir_blibs_val, __dir_rslt_opt: __dir_rslt_val,
        __dir_src_opt: __dir_src_val, __dir_chi_opt: __dir_chi_val,
        __dir_l_src_opt: __dir_l_src_val, __dir_l_blibs_opt: __dir_l_blibs_val,
        __dir_l_rslt_opt: __dir_l_rslt_val, __dir_l_chi_opt: __dir_l_chi_val,
        __dir_cpp_opt: __dir_cpp_val, __dir_bude_opt: __dir_bude_val,
        __dir_getsd_opt: __dir_getsd_val, __dir_colsd_opt: __dir_colsd_val
     }
     ,
    __names_sect:
     {

        __names_b_bmc_opt: __names_b_bmc_val, __names_b_ctrl_opt: __names_b_ctrl_val,
        __names_b_gen_zero_opt: __names_b_gen_zero_val, __names_b_ff_opt: __names_b_ff_val,
        __names_b_transf_opt: __names_b_transf_val, __names_ligs_list_opt: __names_ligs_list_val,
        __names_rcpts_list_opt: __names_rcpts_list_val,
        __names_rslt_lig_list_opt: __names_rslt_lig_list_val,
        __names_rslt_rcpt_list_opt: __names_rslt_rcpt_list_val,
        __names_lig_sd_opt: __names_lig_sd_val, __names_rcpt_sd_opt: __names_rcpt_sd_val
     }

    }

def unpack_app(tar_fname, opts):
    """
    Unpack and decompress an archive.
    
    tar_fname -- File name of the archive that have been compressed.
    opts -- Options to pass to tar to unpack and decompress the archive.
    
    """
    print("Unpacking [%s]" % (tar_fname))
    cmd = "tar %s %s" % (opts, tar_fname)
    print(cmd)
    os.system(cmd)
    return

def comp_util(my_dir):
    """
    Compile an executable by using make.
    There must be a make file in the in the directory where
    the compilation will take place.
    
    my_dir -- Directory that contains the source and make file.
    """
    os.chdir(my_dir)
    os.system('make clean')
    os.system('make')
    os.chdir('../')
    return
    
def do_cpp():
    """
    Compile all programs written in C++.
    """
    os.chdir(cpp_dir)
    comp_util(bysd_dir)
    comp_util(div_dir)
    comp_util(getsd_dir)
    
    # Unpacking BUDE
    unpack_app(bude_tgz, tar_opt)
    os.chdir(bude_dir)
    
    if not os.path.isdir(build_dir):
        os.makedirs(build_dir, 0o755)

    os.chdir(build_dir)
    os.system("../configure --prefix=$HOME")
    os.system('make')
    os.chdir('../../../')
    return
    
def read_ini():
    """
    Read an ini file and creates a ConfigParser object from it.
    
    Returns the ConfigParser object.
    """
    my_cfg = ConfigParser(interpolation=ExtendedInterpolation())
    my_cfg.optionxform = str
    my_cfg.read(__ini_file)
    
    return my_cfg

def create_config(my_sections):
    '''
    Creates a ConfigParser object with all sections and
    options and values using the default values from my_sections
    
    my_sections -- Dictionary holding all sections, options and values.
    
    Returns the ConfigParser object.
    '''

    my_cfg = ConfigParser(interpolation=ExtendedInterpolation())
    my_cfg.optionxform = str
    
    for a_section in my_sections:
        my_cfg.add_section(a_section)
        for an_option in my_sections[a_section]:
            my_cfg.set(a_section, an_option, my_sections[a_section][an_option])

    return my_cfg

def update_ini(my_cfg):
    '''
    Update the executable and directories sections for 
    the CPP utilities that were compiled.
    
    my_cfg -- configparser object that was read from an existent ini file 
    '''
    for a_section in __config_sections:
        if not my_cfg.has_section(a_section):
            my_cfg.add_section(a_section)
            
    # Update directories, work, ccp bude getsd colsd code        
    my_cfg.set(__dir_sect, __dir_wrk_opt, __dir_wrk_val)
    my_cfg.set(__dir_sect, __dir_cpp_opt, __dir_cpp_val)
    my_cfg.set(__dir_sect, __dir_bude_opt, __dir_bude_val)
    my_cfg.set(__dir_sect, __dir_getsd_opt, __dir_getsd_val)
    my_cfg.set(__dir_sect, __dir_colsd_opt, __dir_colsd_val)
      
    # Update all executables.
    my_cfg.set(__exe_sect, __exe_bude_opt, __exe_bude_val)
    my_cfg.set(__exe_sect, __exe_colsd_opt, __exe_colsd_val)
    my_cfg.set(__exe_sect, __exe_getsd_opt, __exe_getsd_val)
    
    return

def check_ini():
    """
    Check if there is an ini file, if so it will update it
    otherwise it will create a new one.
    """
    
    if os.path.isfile(__ini_file):
        my_cfg = read_ini()
        update_ini(my_cfg)
        f_up = open(__ini_file, 'w')
        my_cfg.write(f_up)
        
        print("\nThe file [%s] was updated.\n" % (__ini_file))

    else:
        my_cfg = create_config(__config_sections)
        f_new = open(__ini_file, 'w')
        my_cfg.write(f_new)
        print("\nThe file [%s] was created.\n" % (__ini_file))
    
    work_dir = my_cfg.get(__dir_sect, __dir_wrk_opt)
    
    print(work_dir)
    
    if not os.path.isdir(work_dir):
        os.makedirs(work_dir, 0o755)
        
    print("We also updated the working directory [%s]." % (work_dir))
    print("You can put your PDBs in this directory to do the Alanine Scanning.")
    print("If you use another directory 'ALAscanApp.py' will issue a warning.")
    print("Manually update the ini file [%s] " % (__ini_file))
    print("Change the value of the option '%s' in section [%s]" % (__dir_wrk_opt, __dir_sect))
    print("to whatever directory you wish to work from.\n")
    print("If the new directory is [/HOME/My/Dir]\n")
    print("Change from\n[%s]\n%s = %s\n\nTo:" % (__dir_sect, __dir_wrk_opt, work_dir))
    print("[%s]\n%s = /HOME/My/Dir\n" % (__dir_sect, __dir_wrk_opt))

    return

def link_exes(my_bin):
    
    time_stamp = strftime("%Y-%m-%d_%H-%M-%S", localtime())
    
    os.chdir(my_bin)
    
    if os.path.isfile(ala_scan_name):
        backup_name = "%s_%s" % (ala_scan_name, time_stamp)
        shutil.move(ala_scan_name, backup_name)
        print("Warning: We found a file named [%s] in [%s]." % (ala_scan_name, my_bin))
        print("\t It was backup to: [%s].\n" % (backup_name)) 
        os.system("ln -s %s" % (scan_exe))
    else:
        os.system("ln -s %s" % (scan_exe))
        
    if os.path.isfile(replot_py_name):
        backup_name = "%s_%s" % (replot_py_name, time_stamp)
        shutil.move(replot_py_name, backup_name)
        print("Warning: We found a file named [%s] in [%s]." % (replot_py_name, my_bin))
        print("\t It was backup to: [%s].\n" % (backup_name)) 
        os.system("ln -s %s" % (replot_exe))
    else:
        os.system("ln -s %s" % (replot_exe))
    return

def edit_bash(my_bin):
    
    b_rc = "%s/.bashrc" % (os.path.expanduser("~"))
    b_pf = "%s/.bash_profile" % (os.path.expanduser("~"))
    
    if os.path.isfile(b_rc):
        bash_file = b_rc
    elif os.path.isfile(b_pf):
        bash_file = b_pf
    else:
        if os.path.isfile("/etc/skel/.bashrc"):
            shutil.copy2("/etc/skel/.bashrc", b_rc)
        bash_file = b_rc
    
    updated_path = "export PATH=%s:$PATH" % (my_bin)
    b_file = open(bash_file, "a")
    b_file.write("\n# Path appended for ALAscanApp\n")
    b_file.write(updated_path)
    b_file.close()
  
    print("We edited the file [%s]." % (bash_file))
    print("You should update the path by executing:\n")
    print(updated_path)
    print("\nAlternatively, you could close this terminal and open a new one.\n")
    return
    
def add_to_bin():
    
    my_bin = "%s/bin" % (os.path.expanduser("~"))
    
    if not os.path.isdir(my_bin):
        os.makedirs(my_bin, 0o755)
    
    if 'PATH' in os.environ and my_bin in os.getenv('PATH'):
        link_exes(my_bin)
    else:
        edit_bash(my_bin)
        link_exes(my_bin)
    return
    
def confirm_go_on(msg):
    
    go_on = ''
    
    while go_on != 'yes' and go_on != 'no':
        
        go_on = input("%s [yes|no]? [yes] " % msg)   
        if not go_on:
            go_on = 'yes'
        if go_on != 'yes' and go_on != 'no':
            print("Please press enter or type 'yes' to continue. Type 'no' to stop.")
        go_on = go_on.lower()

    if go_on == 'yes':
        return True
    else:
        return False

def begin_work():
    warn_message = """

  We are about to set up the alanine scanning app.

  We are assuming that you have installed a recent C++ compiler,
  GCC version 4.6 or greater, and if running this in a 
  Mac enviroment that you have set the CC and CXX variables.
  
  ie. export CC=gcc-X CXX=g++-X
  Where X is the version of GCC installed with brew.

  We are also assuming that you have python 3.5 or greater
  available in your enviroment.
  
  Preferibly you will have install Anaconda Python 3 
  
  To check GCC version type command
  g++ --version in Linux systems.
  g++-X --version in Mac systems.
  
  To check python version
  python --version

  If this sound double-dutch to you then ask 
  a system's Administrator to install it.
  
  The README file has more detailed information.

  """
  
    work_message = """
    
  So, we will continue.
  
  
  Jobs we are doing:
   - Unpacking the Alanine Scan App.
   - Compiling the C++ utilities.
   - Unpacking and compiling BUDE.
   - Creating/Updating the Alanine Scan App config file.
   
  We are dividing this in two parts in case you might like 
  to update the config file without re-compiling the C++ programs.   
  
    """
    
    path_msg = """
    We can now check if you have a 'bin' directory and is part of the path.
    
    If not, we could create and add it to the path as well as creating a link 
    to the ALAscanApp executables. 
    
    This will modify/create file '.bashrc' or '.bash_profile' within
    your home directory 
    """

    print(warn_message)

    if not confirm_go_on("Do you want to continue"):
        print("\nGood-Bye, come back when your are ready. :)\n")
        return
    
    print(work_message)
     
    print("We are going to unpack and compile programs.")
     
    if confirm_go_on("Do you want compile them"):
        unpack_app(app_tgz, tar_opt)
        do_cpp()
    else:
        print("\nWe skipped unpacking and compilation.")
 
    print("\n\nWe are going to create/update the ini file.")
     
    if confirm_go_on("Do you want create/update the ini file"):
        check_ini()
    else:
        print("\nWe skipped config ini file.")
      
    print(path_msg)

    if confirm_go_on("Do you want to add ALAscanApp to your PATH"):
        add_to_bin()
    else:
        print("OK, we will not touch your home directory. :)")          

    return


begin_work()

