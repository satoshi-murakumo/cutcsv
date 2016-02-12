cutcsv
======

Pull out a specific column from a CSV file.


BUILD
-----

    # install stack
    # http://docs.haskellstack.org/en/stable/install_and_upgrade.html#centos-red-hat-amazon-linux

    sudo yum -y install glibc-static gmp-static zlib-static
    stack setup

    cd cutcsv
    stack build --ghc-options="-O2 -static -optl-static -optl-pthread"


