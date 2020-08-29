import random
f = open("output.txt", "x")

def print_vertex(vertex_num,out_rank,max_weight):
    for i in range(vertex_num):
        num_neigh=int(random.uniform(0,out_rank))
        chosen_vertex=random.sample(range(0, vertex_num), num_neigh)
        if i in chosen_vertex:
            chosen_vertex.remove(i)
        if len(chosen_vertex)>0:
            for j in chosen_vertex:
                f.write(str(i)+","+str(j)+","+str(int(random.uniform(1,max_weight)))+"\n")


print_vertex(10000,50,10)