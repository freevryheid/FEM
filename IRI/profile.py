class Prof(object):

    def __init__(self, pdf, scl, dx):
        self.pdf = pdf
        self.scl = scl
        self.dxx = dx

    def left(self):
        return self.pdf.LEFT*self.scl

    def right(self):
        return self.pdf.RIGHT*self.scl

    def dx(self):
        return self.dxx*self.scl

    def n(self):
        return len(self.pdf)
