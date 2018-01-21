How to install seitosh
======================

Recommended steps to take
-------------------------
1. Read [src/README.compile](src/README.compile)
   which provides a thorough overview of requirements and 
   environment variables expected by Makefiles.
2. Read [contrib/pgplot/README.pgplot](contrib/pgplot/README.pgplot)
   to understand how to install the PGPLOT library, which is
   required by programs producing graphical output.
3. Once you feel ready (having installed all required compilers
   and external libaries, having set all expected environment
   variable) change to directory [src](src) and use the shell
   script [src/compile.sh](src/compile.sh) by issuing the
   command

        compile.sh install

Further resources of information
--------------------------------
* https://git.scc.kit.edu/Seitosh/Seitosh/wikis/home  
  is the wiki accompanying the repository
* https://git.scc.kit.edu/Seitosh/Seitosh/wikis/doc/PGPLOT  
  Instructions for the installation of PGPLOT

* http://gpitrsvn.gpi.uni-karlsruhe.de:8000/TFSoftware  
  is the predecessor of Seitosh
* http://gpitrsvn.gpi.uni-karlsruhe.de:8000/TFSoftware/wiki/trunk  
  provides documentation for source-code subdirectories; however,
  not all of them have been transferred to Seitosh and not all Seitosh
  directories are mentioned there
