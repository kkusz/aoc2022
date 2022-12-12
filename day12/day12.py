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
    (sx, sy) = startpos
    (ex, ey) = endpos
    elev = elevation[sy][sx]
    elevation[sy][sx] = 0
    #print(list(neighbours(elevation,endpos)))
    Q = [(startpos, elev)]
    while len(Q) > 0:
        (pos,elev) = Q.pop(0)
        (x, y) = pos
        for n in neighbours(elevation, pos, elev):
            (nx, ny) = n
            if elevation[ny][nx] == 'E':
                return elevation[y][x] + 1
            elev = elevation[ny][nx]
            elevation[ny][nx] = elevation[y][x] + 1
            Q.append((n,elev))

def neighbours(el, pos, elev):
    (x, y) = pos
    if not_visited(el, (x-1, y), elev):
        yield (x-1, y)
    if not_visited(el, (x, y-1), elev):
        yield (x, y-1)
    if not_visited(el, (x+1, y), elev):
        yield (x+1, y)
    if not_visited(el, (x, y+1), elev):
        yield (x, y+1)

def not_visited(el, pos, elev):
    (x, y) = pos
    if elev == 'S':
        elev = 'a'
    if (x < 0) or (x >= len(el[0])) or (y < 0) or (y >= len(el)):
        return False
    return (el[y][x] == 'E') or (type(el[y][x]) is str) and (ord(el[y][x])-ord(elev)) <= 1

def main():
    (elevation, startpos, endpos) = load_data('input_long.txt')
    print(BFS(elevation, startpos, endpos))
    # for x in elevation:
    #     print(x)

if __name__ == "__main__":
    main()