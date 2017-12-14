# Networks Analysis Scripts

This is a temporary development folder that holds some network-analysis related routines for the second paper. Please take note that they require _PajaroLoco_ (https://bitbucket.org/chipdelmal/pajarolocopublic) installed.

Most of the ideas so far revolve around clustering geographically and analyzing the transitions between _sinks_ and _sources_ on the movement matrices. For more information see the related channel on *Slack: #mosypopdyn* or drop me a line.

For now, all the routines are coded in _Mathematica_ but they will be ported to *R* or *Python* in the future.

## Conda Environment

To install the *conda* environment containing: _iGraph_, _NetworkX_, and _TensorFlow_; run the following command in terminal (requires _Anaconda_):

```conda env create -f MASH_Networks.yml -n MASH_Networks```

To run the environment:

```source activate MASH_Networks```

To close the environment:

```source deactivate```

### Notes for my future self

To export environment to *YML* (after activating):

```conda env export > environment.yml```

To install *Miniconda*:

```sh Miniconda3.sh```