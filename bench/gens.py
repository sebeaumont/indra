
# python to the rescue!
generators = "aBAb";
depthmax = 15 # torture for benchmarking!


################################################################################

def explore(l, d, p):
    if d < depthmax: 
        print(p,'--> ',end='')
        for i in range(l-1, l+2):
            k = i+len(generators) if i < 0 else i % len(generators)
            explore(k, d+1, p + generators[k])

            
################################################################################
            
def main():
    for i in range(len(generators)):
        print('')
        explore(i, 0, generators[i])
        

if __name__ == "__main__": main()
