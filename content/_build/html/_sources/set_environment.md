Set the environment for jpbooks

```
conda create -n jpbooks
conda activate jpbooks
conda install conda-forge::jupyter-book
conda install conda-forge::sphinx
conda install conda-forge::matplotlib
conda install conda-forge::scipy
conda install conda-forge::ghp-import
conda install -c r r-irkernel
conda install -c conda-forge pkg-config # install pkg-config, otherwise you can not install some of the packages
```



build the book

```
# build the book
jupyter-book build ./content
# Upload to Github page
ghp-import -n -p -f ./content/_build/html

```