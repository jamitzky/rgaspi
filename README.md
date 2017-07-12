# rgaspi
rgaspi: gaspi bindings in R

Authors: C. Bernau and F. Jamitzky

This project consists of three subprojects organized in separate folders:

- basiclayer:  
the R-C-interface to all necessary GPI2-functions 

- middlelayer: 
this layer consists of convenience functions, wrappers and 
            classes around the basic layer 
            
- toplayer:    
this layer can use the middle and basic layer to implement
            higher level aplications such as distributed matrices


Additionally, there is a separate directory 'tests' which contains basic tests
and examples.

*Before the shared library implementing basiclayer can be used, one has to enter the folder rgaspi/basic and
follow the compilation instructions as described in the file compile_notes.txt which can also be found in that 
directory.*


For the installation you also need an installed version of GPI2

http://www.gpi-site.com/gpi2/
