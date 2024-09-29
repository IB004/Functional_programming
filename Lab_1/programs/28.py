def spiral_diag_sum(lenght):
    dist = 2
    x = 1
    res = 1
    while dist < lenght:
        for i in range(4):
            x += dist
            res += x
        dist += 2
    return res


print(spiral_diag_sum(1001))
        
        
    
