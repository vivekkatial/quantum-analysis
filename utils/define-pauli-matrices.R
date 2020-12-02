###############################################################################
# Utility Script to generate all the differnet pauli matrices
#
# Created Date: Mon Sep  9 12:09:33 2019
# Author: Vivek Katial
###############################################################################


# Define Base Matrices ----------------------------------------------------

X = matrix(c(0,1,1,0),    nrow = 2, ncol = 2)
Y = matrix(c(0,-1i,1i,0), nrow = 2, ncol = 2)
Z = matrix(c(1,0,0,-1),   nrow = 2, ncol = 2)
I = matrix(c(1,0,0,1),    nrow = 2, ncol = 2)


