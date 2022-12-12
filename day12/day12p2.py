#!/usr/bin/env python3
def load_data(filename):
    elevation = []
    startpos = (-1, -1)
    endpos = (-1, -1)
    lineno = 0
    with open(filename) as f:
        for line in f:
            elevation.append(list(line.rstrip()))
            if 'S' in line:
                startpos = (line.index('S'), lineno)
            if 'E' in line:
                endpos = (line.index('E'), lineno)
            lineno = lineno + 1
    return (elevation, startpos, endpos)

def BFS(elevation, startpos, endpos):
    traversal = [[99999 for i in range(len(elevation[0]))] for j in range(len(elevation))]
    (sx, sy) = startpos
    (ex, ey) = endpos
    traversal[sy][sx] = 0
    #print(list(neighbours(elevation,endpos)))
    Q = [startpos]
    while len(Q) > 0:
        pos = Q.pop(0)
        (x, y) = pos
        elev = elevation[y][x]
        trav = traversal[y][x]
        for n in neighbours(elevation, pos, elev, traversal, trav):
            (nx, ny) = n
            if elevation[ny][nx] == 'E':
                # for z in traversal:
                #     print('\t'.join(map(lambda xx: str(xx),z)))
                return traversal[y][x] + 1
            if elevation[ny][nx] == 'a':
                traversal[ny][nx] = 0
            else:
                traversal[ny][nx] = traversal[y][x] + 1
            Q.append(n)

def neighbours(el, pos, elev, tr, trav):
    (x, y) = pos
    if not_visited(el, (x-1, y), elev, tr, trav):
        yield (x-1, y)
    if not_visited(el, (x, y-1), elev, tr, trav):
        yield (x, y-1)
    if not_visited(el, (x+1, y), elev, tr, trav):
        yield (x+1, y)
    if not_visited(el, (x, y+1), elev, tr, trav):
        yield (x, y+1)

def not_visited(el, pos, elev, tr, trav):
    (x, y) = pos
    if elev == 'S':
        elev = 'a'
    if (x < 0) or (x >= len(el[0])) or (y < 0) or (y >= len(el)):
        return False
    return (el[y][x] == 'E') \
        or ((trav+1) < tr[y][x]) and (ord(el[y][x])-ord(elev)) <= 1

def main():
    (elevation, startpos, endpos) = load_data('input_long.txt')
    print(BFS(elevation, startpos, endpos))
    # for x in elevation:
    #     print('\t'.join(map(lambda xx: str(xx),x)))

if __name__ == "__main__":
    main()