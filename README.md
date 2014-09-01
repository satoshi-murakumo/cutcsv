cutcsv
======

Pull out a specific column from a CSV file.


BUILD
-----

    yum install -y gcc glibc-static
    yum install -y zlib zlib-devel zlib-static
    yum install -y gmp gmp-devel gmp-static
    yum install -y freeglut freeglut-devel
    yum install -y wget
    wget http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-x86_64-unknown-linux-centos65.tar.bz2
    wget http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-install-1.20.0.3.tar.gz
    tar xvfj ghc-7.8.3-x86_64-unknown-linux-centos65.tar.bz2
    cd ghc-7.8.3
    ./configure
    make install
    cd
    tar zxvf cabal-install-1.20.0.3.tar.gz
    cd cabal-install-1.20.0.3
    ./bootstrap.sh

    # add path $HOME/.cabal/bin

    cabal update
    cabal install happy alex

    cd cutcsv
    cabal configure --ghc-options="-O2 -static -optl-static -optl-pthread"
    cabal build

