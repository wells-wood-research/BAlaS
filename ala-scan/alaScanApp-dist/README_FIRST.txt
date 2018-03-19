If you are using a Mac system or a C++ compiler which is not GCC, you will need 
to export the CC and CXX environment variables. If using a Mac system you will 
also need to install GCC with Homebrew.

We also need python 3.5 or greater, preferably Anaconda. Please see the 
doc/README file to get more information on how to do it.

The doc directory has the README and UsingAlaScanApp in html, pdf, txt and odt 
format. These files explain how to install all requirements and how to use 
ALAscanApp.py.

Check points.

Mac:

Install Xcode, Homebrew, GCC.
Export CC and CXX environment variables.
Install Python Anaconda.

Linux:

Install GCC.
Export CC and CXX environment variables (if using different compiler).
Install Python Anaconda (unlikely your system has python 3.5 or greater).


If you have done all of that, run:

>./setup_utils.py

And follow the instructions. The script will output a healthy amount of 
information and will take some minutes depending on the speed of your machine. 
The previous command will unpack the ALAscanApp, compile the C++ programs and 
update/create the initialisation file ‘.alascanapp.ini’ in your home directory.


