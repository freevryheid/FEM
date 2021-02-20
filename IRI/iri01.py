#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import piri
import matplotlib.pyplot as plt
plt.style.use('ggplot')


class Prof(object):

    def __init__(self, pdf, scl, dx):
        self.scl = scl
        self.pdf = pdf*scl
        self.x = dx
        self.dx = dx*scl
        self.n = len(pdf.index)
        self.d = self.n*dx
        self.nsx = int(self.d/528)

    def split(self):
        n = self.nsx*int(528/self.x)
        df = self.pdf[0:n]
        return np.split(df, self.nsx)


class IRI:

    def __init__(self):
        pdf = pd.read_csv('prof.csv')
        scl = 0.3048  # ft -> m
        dx = 0.2069167  # ft
        self.prof = Prof(pdf, scl, dx)

    def main(self):
        dfs = self.prof.split()
        iri = []
        for df in dfs:
            dfl = df.LEFT
            dfr = df.RIGHT
            prof, nsamp, liri = piri.getpi(dfl, self.prof.dx)
            prof, nsamp, riri = piri.getpi(dfr, self.prof.dx)
            liri *= 63360.
            riri *= 63360.
            airi = (liri+riri)/2
            # print("left iri: {0:6.2f} in/mi | right iri: {1:6.2f} in/mi | avg iri: {2:6.2f} in/mi".format(liri, riri, airi))
            iri.append({
                'LEFT': liri,
                'RIGHT': riri,
                'AVG': airi
            })
        iri = pd.DataFrame(iri, columns=['LEFT', 'RIGHT', 'AVG'])
        print(iri)
        print(iri.describe())
        iri.boxplot()
        plt.show()

if __name__ == "__main__":
    iri = IRI()
    iri.main()
