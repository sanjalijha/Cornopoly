brown = [["Baker Hall", 60, 50, [2, 10, 30, 90, 160, 250], 30],
         ["Lincoln Hall", 60, 50, [4, 20, 60, 180, 320, 450], 30]]
light_blue = [[
    "Physical Sciences Building", 100, 50, [6, 30, 90, 270, 400, 550], 50
], ["Rockerfeller Hall", 100, 50, [6, 30, 90, 270, 400, 550], 50],
    ["Klarman Hall", 120, 50, [8, 40, 100, 300, 450, 600], 60]]
pink = [[
    "Martha Von Rensselaer Hall", 140, 100, [10, 50, 150, 450, 625, 750], 70
], ["Bailey Hall", 140, 100, [10, 50, 150, 450, 625, 750], 70],
    ["Kennedy Hall", 160, 100, [12, 60, 180, 500, 700, 900], 80]]
orange = [["Day Hall", 180, 100, [14, 70, 200, 550, 750, 950], 90],
          ["Sage Hall", 180, 100, [14, 70, 200, 550, 750, 950], 90],
          [
              "Willard Straight Hall", 200, 100, [16, 80, 220, 600, 800, 1000],
              100
]]
red = [["Annabel Taylor Hall", 220, 150, [18, 90, 250, 700, 875, 1050], 110],
       ["Hollister Hall", 220, 150, [18, 90, 250, 700, 875, 1050], 110],
       ["Carpenter Hall", 240, 150, [20, 100, 300, 750, 925, 1100], 120]]
yellow = [["Uris Hall", 260, 150, [22, 110, 330, 800, 975, 1150], 130],
          ["Sage Hall", 260, 150, [22, 110, 330, 800, 975, 1150], 130],
          ["Statler Hall", 280, 150, [24, 120, 360, 850, 1025, 1200], 140]]
green = [["Phillips Hall", 300, 200, [26, 130, 390, 900, 1100, 1275], 150],
         ["Duffield Hall", 300, 200, [26, 130, 390, 900, 1100, 1275], 150],
         ["Rhodes Hall", 320, 200, [28, 150, 450, 1000, 1300, 1400], 160]]
blue = [["Upson Hall", 350, 200, [35, 175, 500, 1100, 1300, 1500], 175],
        ["Gates Hall", 400, 200, [50, 200, 600, 1400, 1700, 2000], 200]]

trains = [["Helen Newman Bus Stop", 200, 0, [25, 50, 100, 200], 100],
          ["Carpenter Hall Bus Stop", 200, 0, [25, 50, 100, 200], 100],
          ["Statler Hotel Bus Stop", 200, 0, [25, 50, 100, 200], 100],
          ["Schwartz Bus Stop", 200, 0, [25, 50, 100, 200], 100]]

utilities = [["Cornell Health Services", 150, 0, [], 75],
             ["Cornell IT Services", 150, 0, [], 75]]

all_colors = []
for b in brown:
    b.append("brown")
    all_colors.append(b)
for lb in light_blue:
    lb.append("light blue")
    all_colors.append(lb)
for p in pink:
    p.append("pink")
    all_colors.append(p)
for o in orange:
    o.append("orange")
    all_colors.append(o)
for r in red:
    r.append("red")
    all_colors.append(r)
for y in yellow:
    y.append("yellow")
    all_colors.append(y)
for g in green:
    g.append("green")
    all_colors.append(g)
for b in blue:
    b.append("blue")
    all_colors.append(b)
tiles = [-1 for i in range(40)]


def generator():
    tiles[0] = (0, "GO")
    tiles[10] = (10, "JAIL")
    tiles[20] = (20, "PARKING")
    tiles[30] = (30, "GO TO JAIL")
    tiles[2] = (2, "CORNELL CARD")
    tiles[4] = (4, "INCOME TAX")
    tiles[7] = (7, "CORNELL CARD")
    tiles[17] = (17, "CORNELL CARD")
    tiles[22] = (22, "CORNELL CARD")
    tiles[33] = (33, "CORNELL CARD")
    tiles[36] = (36, "CORNELL CARD")
    tiles[38] = (38, "SUPER TAX")
    j = 0
    k = 0
    l = 0
    print("{")
    print("\"tiles\":")
    print("[")
    for i in range(40):
        if i == 28 or i == 12 and k < len(utilities) - 1:
            tiles[i] = utilities[k]
            print("{")
            print("\"id\":" + str(i) + ",")
            print("\"name\":" + "\"" + str(tiles[i][0]) + "\"" + ",")
            print("\"price\":" + str(tiles[i][1]) + ",")
            print("\"cost for lounge\":" + str(tiles[i][2]) + ",")
            print("\"rents\":" + str(tiles[i][3]) + ",")
            print("\"color\":" "\"" + "util" + "\"" + ",")
            print("\"mortgage\":" + str(tiles[i][4]))
            print("},")
            k += 1
        elif i != 0 and i in [5, 15, 25, 35]:
            tiles[i] = trains[l]
            print("{")
            print("\"id\":" + str(i) + ",")
            print("\"name\":" + "\"" + str(tiles[i][0]) + "\"" + ",")
            print("\"price\":" + str(tiles[i][1]) + ",")
            print("\"cost for lounge\":" + str(tiles[i][2]) + ",")
            print("\"rents\":" + str(tiles[i][3]) + ",")
            print("\"color\":" "\"" + "train" + "\"" + ",")
            print("\"mortgage\":" + str(tiles[i][4]))
            print("},")
            l += 1
        elif tiles[i] != -1:
            print("{")
            print("\"id\":" + str(tiles[i][0]) + ",")
            print("\"name\":" + "\"" + str(tiles[i][1]) + "\"" + ",")
            print("\"price\":" + str(0) + ",")
            print("\"cost for lounge\":" + str(0) + ",")
            print("\"rents\":" + str([]) + ",")
            name = tiles[i][1]
            if name == "CORNELL CARD":
                print("\"color\":" + "\"" + str("Card") + "\"" + ",")
            elif name == "GO TO JAIL":
                print("\"color\":" + "\"" + str("Jail") + "\"" + ",")
            elif name == "JAIL":
                print("\"color\":" + "\"" + str("Non-Jail") + "\"" + ",")
            elif name == "PARKING":
                print("\"color\":" + "\"" + str("Free Parking") + "\"" + ",")
            elif name == "GO":
                print("\"color\":" + "\"" + str("Go") + "\"" + ",")
            elif name == "INCOME TAX":
                print("\"color\":" + "\"" + str("Income Tax") + "\"" + ",")
            elif name == "SUPER TAX":
                print("\"color\":" + "\"" + str("Super Tax") + "\"" + ",")
            else:
                print("\"color\":" + "\"" + str("SPC") + "\"" + ",")
            print("\"mortgage\":" + str(0))
            print("},")
        elif tiles[i] == -1:
            tiles[i] = all_colors[j]
            print("{")
            print("\"id\":" + str(i) + ",")
            print("\"name\":" + "\"" + str(tiles[i][0]) + "\"" + ",")
            print("\"price\":" + str(tiles[i][1]) + ",")
            print("\"cost for lounge\":" + str(tiles[i][2]) + ",")
            print("\"rents\":" + str(tiles[i][3]) + ",")
            print("\"color\":" "\"" + str(tiles[i][-1]) + "\"" + ",")
            print("\"mortgage\":" + str(tiles[i][4]))
            print("},")
            j += 1
    print("],")
    print("\"start tile\": 0")
    print("}")


generator()
