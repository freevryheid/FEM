class Prof(object):

    def __init__(self, pdf, scl, dx):
        self.scl = scl
        self.pdf = pdf*scl
        self.dx = dx*scl
        self.left = self.pdf.LEFT
        self.right = self.pdf.RIGHT
        self.n = len(pdf)
