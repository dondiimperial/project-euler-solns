def C(i,J):
    if len(J) == 0:
        return 0
    ret = 0
    
    print "i %s J %s " % (i, J)

    for j in range(1, i / J[0]):
        print " (J[0] * j) = %s",  (J[0] * j)
        ret +=  (J[0] * j) * C(i - J[0] * j, J[1:])
        


    return ret

print C(200, [1,2,5,10,20,50,100,200])
    


#print go([1,2],0)
#print go([1,2,5,10,20,50,100,200])

    
