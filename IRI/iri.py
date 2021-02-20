#!/usr/bin/env python
# -*- coding: utf-8 -*-

#       SUBROUTINE GETPI(PROF, NSAMP, DX, BASE, UNITSC, PI, XLEAD, XEXP,
#      &                 K1, K2, C, MU)
# C=======================================================================

# C <-> PROF   Real     On input, an array of profile height values.
# C                     On output, an array of filtered PI profile values.
# C <-> NSAMP  Integer  Number of data samples in array PROF. The filtered
# C                     profile has fewer points than the original.
# C --> DX     Real     Distance step between profile points (m).
# C --> BASE   Real     Distance covered by moving average (m).
# C                     Use 0.250 for unfiltered profile input, and 0.0
# C                     for pre-smoothed profiles (e.g. K.J. Law data).
# C --> UNITSC Real     Product of two scale factors: (1) meters per unit
# C                     of profile height, and (2) PI units of slope.
# C                     Ex: height is inches, slope will be in/mi.
# C                         UNITSC = (.0254 m/in)*(63360 in/mi) = 1069.34
# C <-- PI     Real     The average PI for the entire profile.
# C <-- XLEAD  Real     Initialization base length.
# C <-- XEXP   Real     Power weighting (1. = ARS, 2. = RMS).
# C <-- K1, K2, C, MU   Filter coefficients.


import piri
import numpy as np


class IRI:

    def __init__(self):
        pass

    def main(self):
        # prof = np.genfromtxt("prof.csv", delimiter=",", dtype='float32')
        prof = np.genfromtxt("prof.csv", delimiter=",")
        prof = prof[:, 0]
        # right = prof[:, 1]
        scl = .3048 # ft -> m 
        self.prof = np.asfortranarray(prof*scl)
        dx = 0.2069167  # ft
        nsamp = 25544
        dist = dx*nsamp
        base = 0.25
        unitsc = 63360. # in/mi
        #unitsc = 1.
        xlead = 11.
        xexp = 1
        k1 = 653.
        k2 = 63.3
        c = 6.
        mu = .15
        # seg = 528.05  # segment length in ft
        # step = int(seg/dx)
        # nstep = int(len(prof)/step)
        # prof = prof[0:nstep*step]
        # profs = np.split(prof, nstep)
        # dx *= scl  # m
        # for prof in profs:
        #    pi = piri.getpi(prof, dx, base, unitsc, xlead, xexp, k1, k2, c, mu)
        #    print(pi)
        # prof = prof[0:2557]
        dx *= scl
        self.prof1, nsamp, pi = piri.getpi(self.prof, dx, base, unitsc, xlead, xexp, k1, k2, c, mu, nsamp)
        # print(prof)
        print(pi)
        # print(nsamp)
        # print(prof)

if __name__ == "__main__":
    iri = IRI()
    iri.main()
