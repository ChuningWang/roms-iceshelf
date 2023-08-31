**<font size=4>ROMS + SeaIce + IceShelf code, used by Wang et al. (2023)</font>**

This repository contains a modified branch of the ROMS source code.
A dynamic sea ice module and a static ice shelf module are integrated into the original ocean model.

The sea ice module is inherited from Dr. Kate Hedstrom's code (https://github.com/kshedstrom/roms), with some modifications to fix bugs mostly related to the ice thermodynamics.
We also added some new features, e.g., parellel I/O, real-time zenith angle calculation, etc., to reduce runtime and improve thermodynamics calcultion.

The ice shelf model is inherited from Dr. Benjamin K. Galton-Fenzi's code (https://github.com/bkgf/ROMSIceShelf).
We added more features, e.g., parallel I/O, writing of diagnostic terms, and modified the thermodynamics using some existing code pieces from (https://github.com/ChuningWang/roms-iceplume).

The compiling and running of this branch is very similar to the official ROMS code, thus it should be straight forward for ROMS users. The configuration code for Wang et al. (2023) will be uploaded to a separate repository.

**<font size=4>Development Plan</font>**

**This branch will no longer be maintained!!!**

In Augest 2023, the Rutgers ROMS group finally brought the source code to [GitHub](https://github.com/myroms/roms), and a new sea ice branch is released with the source code. The new ice code is developed by Dr. Scott M. Durski, as described in [Durski & Kurapov (2019)](https://www.sciencedirect.com/science/article/abs/pii/S1463500318302191) and [Durski & Kurapov (2020)](https://www.sciencedirect.com/science/article/abs/pii/S1463500320301980). It contains several functional or performance improvements, e.g., modified ice thermodynamcis, improved surface albedo algorithm, melt ponds, expanded decision tree for surface ice state, etc. For my next project I plan to migrate to the ROMS official branch to include these new features.
