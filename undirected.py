import random
import numpy as np

f = open("undirected1000_15.txt", "x")

def print_vertex_un(vertex_num,out_rank,max_weight):
    a = np.zeros((vertex_num,vertex_num))

    for i in range(vertex_num*out_rank):
        k=int(random.uniform(0,vertex_num))
        j=int(random.uniform(0,vertex_num))
        while(a[k][j]>0):
            if k==j:
                k = int(random.uniform(0, vertex_num))
                j = int(random.uniform(0, vertex_num))
                continue
            k = int(random.uniform(0, vertex_num))
            j = int(random.uniform(0, vertex_num))
        weight = int(random.uniform(0, max_weight))
        a[k][j] = weight
        a[j][k] = weight
    for i in range(vertex_num):
        for j in range(vertex_num):
            if a[i][j]>0:
                f.write(str(i)+","+str(j)+","+str(int(a[i][j]))+"\n")

print_vertex_un(1000,15,10)