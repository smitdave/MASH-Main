So far, I've found two ways to install OSMNX: the virtual environment route, and the "install everything and try not to break dependencies way". Here are the instructions for both approaches:

# Installing OSMNX
## 1) Virtual Environment

### Install Miniconda
* for Windows: I think there is an EXE file to do it
* for Mac: download the BASH installer, open the terminal and move to the directory where it is installed, then run:

```sh Miniconda2.sh```

### Create Virtual Environment

Once that is done, run the following command in the terminal (to create a python virtual environment):

```conda create --yes -c conda-forge -n OSMNX python=2.7 osmnx```

Now run:

```source activate OSMNX```

Start a python session:

```python```

and then:

``` python
import osmnx as ox
exit()
```

If that worked, you’ve installed OSMNX successfully. If it failed we can work it out.
Now close your environment session:

```source deactivate OSMNX```

___(source activate and deactivate OSMNX will need to be called in each terminal session)___

## 2) Hard Installation

```
sh Miniconda2.sh
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
conda install -c conda-forge osmnx
conda install -c conda-forge geopandas
brew install geos
INSTALL_WITH_INSTRUCTIONS_IN_TAR spatialindex
```

# Next Steps

Now try to run some examples from: https://github.com/gboeing/osmnx-examples/tree/master/notebooks
