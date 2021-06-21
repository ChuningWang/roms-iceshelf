**<font size=6>ROMS-ICEPLUME IceShelf Branch</font>**

This branch contains a more advanced version of the ROMS ICESHELF code.

The original ICESHELF code is developed by Dr. Enrique Curchitser. ICESHELF predefines a static iceshelf, which provides pressure and frictional forces to the fluid below. Later the code is modified by Mike Dinniman, who added the basic thermodynamics between the shelf and water below. Dr. Benjamin K. Galton-Fenzi then added more features, i.e., writing of diagnostic terms, more thermodynamics, etc., and brought the code to [github](https://github.com/bkgf/ROMSIceShelf). Dr. Rupert Gladstone developed a coupling framkework FISOC which used the ICESHELF code developed by Benjamin K. Galton-Fenzi as one of the ocean components. The code of this branch is built on the shoulder of these predecessors.

The ICESHELF code I acquired is built on top of a rather old ROMS version (a 2015 release). The recent developments ROMS provides some new features that can improve the overall performance. This branch takes Ben and Rupert's ICESHELF code, and attempts to make it work with the most recent ROMS repo (2021 Jun release).
