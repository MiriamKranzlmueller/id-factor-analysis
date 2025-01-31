import numpy as np
import itertools as it

def get_off_diago(A):
    n = A.nrows()
    utri_ind = np.triu_indices(int(n), k=1)
    off_diago = []
    for i in range(len(utri_ind[0])):
        row_ind = utri_ind[0][i]
        col_ind = utri_ind[1][i]
        off_diago.append(A[row_ind, col_ind])
    return(off_diago)

def get_ideal_random(adjMatrix):
    nrow = adjMatrix.shape[0]
    ncol = adjMatrix.shape[1]

    # Check which variables we need in ring
    variables = []
    for i in range(nrow):
        for j in range(ncol):
            if adjMatrix[i, j]:
                variables.append("l"+str(i+1)+str(j+1))

    # Define ring
    R = PolynomialRing(QQ, variables, order="deglex",implementation="singular")

    # Define matrix Lambda
    Lambda = matrix(R, nrow, ncol)
    count = 0
    for i in range(nrow):
        for j in range(ncol):
            if adjMatrix[i, j]:
                Lambda[i, j] = R.gens()[count]
                count = count + 1

    sample_space = [1,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97, 
                            101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199]
    sample_space = sample_space + [-x for x in sample_space]

    rLambda = Lambda.apply_map(lambda x : np.random.choice(sample_space) if x!=0 else x)
    rSigma = rLambda * rLambda.T

    generators = get_off_diago(rSigma - Lambda * Lambda.T)
    I = R.ideal(generators)
    return(I, R)

def plug_in(adjMatrix, R, point):
    nrow=adjMatrix.shape[0]
    ncol=adjMatrix.shape[1]
    L = matrix(R, nrow, ncol)
    for i in range(nrow):
        for j in range(ncol):
            if adjMatrix[i,j] != 0:
                L[i,j] = point["l"+str(i+1)+str(j+1)]
    return(L)

def is_equal_up_to_colsign(A, B):
    
    A = np.array(A)
    B = np.array(B)
   
    if A.shape != B.shape:
        raise ValueError("Matrices must have the same size.")
        
    nlat = A.shape[1]
        
    for perm in it.product([1,-1],repeat=nlat):
        if np.array_equal(A, np.matmul(B, np.diag(perm))):
            return(True)
    return(False)

def check_sign_identifiability(adjMatrix, it_cc=3, it_rr=50):
    
    # Check whether (complex) degree of sign-identifiability is equal to one
    res_cc = []
    for i in range(it_cc):
        I, R = get_ideal_random(adjMatrix)
        if I.dimension() == 0:
            V = I.variety(ring=CC)
            A = plug_in(adjMatrix, CC, V[0])
            is_id_cc = True
            for i in range(1,len(V)):
                B = plug_in(adjMatrix, CC, V[i])
                if not is_equal_up_to_colsign(A,B):
                    is_id_cc = False
                    break
        else:
            is_id_cc = False
        res_cc.append(is_id_cc)
    if any([not i for i in res_cc]):
        id_degree_1 = False
    else: 
        id_degree_1 = True
    
    # If the (complex) degree of sign-identifiability is larger than two, 
    # we verify generic sign-identifiability over the real numbers
    if id_degree_1:
        return(True)
    else:
        res_rr = []
        for i in range(it_rr):
            I, R = get_ideal_random(adjMatrix)
            if I.dimension() == 0:
                V = I.variety(ring=RR)
                A = plug_in(adjMatrix, R, V[0])
                is_id_rr = True
                for i in range(1,len(V)):
                    B = plug_in(adjMatrix, R, V[i])
                    if not is_equal_up_to_colsign(A,B):
                        is_id_rr = False
                        break
            else:
                is_id_rr = False
            res_rr.append(is_id_rr)
        if (sum([not x for x in res_rr]) >= 2):
            return(False)
        else: 
            # This should never appear
            print("The (complex) degree of sign-identifiability is >= 2  while generig sign-idnetifiability holds over the real numbers.")
            return(True)