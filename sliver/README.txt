
              SLiVER 1.2
              June 2019

Symbolic LAbS VERifier

    * Package contents *

README.txt        this file

sliver.py         SLiVER command-line front-end

cex.py            SLiVER counterexample translation module

core/             CSeq core framework

labs/             LAbS parser and translator

*.labs            LAbS example specifications

cbmc54-custom     A slightly customized build of CBMC 5.4

(Other files)     Libraries used by CSeq/SLiVER

    * Installation *

To install SLiVER, please follow the steps below:

    1. install the dependencies:
      - Python 3.5 or higher
      - (Optional) The bundled CSeq backend requires Python 2.7 with the
        pycparser module (bundled in this distribution).

    2. create a directory, suppose this is called /workspace

    3. extract the entire package contents in /workspace
    
    4. set execution (+x) permissions for sliver.py and cbmc54-custom

    5. make sure that the backend's binary is in the search path, or
       amend the command strings in sliver.py accordingly.

    * Usage *

To try SLiVER, please use the following command:

    ./sliver.py --steps 12 --fair boids-aw.labs birds=3 delta=13 grid=10

which should report that no property is violated.

The following command should instead report that a property is violated:

    ./sliver.py --steps 18 --fair boids-aw.labs birds=4 delta=13 grid=10

Use the --backend=<cbmc|cseq|esbmc> option to select a different
verification backend. 
Please keep in mind that ESBMC support is still experimental, therefore:

  1. The esbmc executable is not provided as part of this package
  2. Our counterexample translation only supports CBMC and CSeq.

Invoking the tool without options:

    ./sliver.py

will provide further usage directions.
