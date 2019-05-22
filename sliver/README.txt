
              SLiVER 1.1
              May 2019

Symbolic LAbS VERifier

    * Package contents *

README.txt        this file

sliver.py         SLiVER command-line front-end

cex.py            SLiVER counterexample translation module

core/             CSeq core framework

labs/             LAbS parser and translator

flock.labs        a simple, parametric LAbS system.

(Other files)     Libraries used by CSeq/SLiVER

    * Installation *

To install SLiVER, please follow the steps below:

    1. install the dependencies:
        - Python 3.5 or higher
        - The Click package: https://click.palletsprojects.com.
          If the Python package installer (pip() is available, 
          Click may be installed with the command:
          $ pip install click
        - backends: CBMC, ESBMC, CSeq
         (none of the above tools is specifically required
          but at least one of them is needed for verification)

    2. create a directory, suppose this is called /workspace

    3. extract the entire package contents in /workspace
    
    4. set execution (+x) permissions for sliver.py

    5. make sure that the backend's binary is in the search path, or
       amend the command strings in, sect. Options and Parameters,
       accordingly.

    * Usage *

To try SLiVER, please use the following command:

    ./sliver.py --steps 12 --fair flock.labs birds=3 delta=22 grid=16

which should report that no property is violated.

The following command should instead report that a property is violated:

    ./sliver.py --steps 12 --fair flock.labs birds=3 delta=21 grid=16

Invoking the tool without options:

    ./sliver.py

will provide further usage directions.
