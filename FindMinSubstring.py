import timeit
from itertools import product, combinations, chain

def FindMinSubstring(strArr):

    fullstring, substring = strArr

    positions = {}
    doubles = {}

    for char in substring:
        positions[char] = [i for i, x in enumerate(fullstring) if char == x] #indices for all characters
        if substring.count(char) > 1:
            doubles[char] = list(x for x in combinations(positions[char], substring.count(char)))
            del positions[char]

    length = len(fullstring)

    if doubles:
        for group in product(*positions.values()):  # for i in itertools.product(*positions): lets me create Cartesian products of all the values in 'positions'
            if len(doubles) > 1:
                for group2 in product(*doubles.values()):
                    newgroup = sorted(list(group) + list(chain.from_iterable(group2)))
                    if (newgroup[-1] - newgroup[0]) < length:
                        length = newgroup[-1] - newgroup[0]
                        record = (newgroup[0], newgroup[-1])

            elif len(doubles) == 1:
                for multi in doubles:
                    for tup in doubles[multi]:
                        newgroup = sorted(group + tup)
                        if (newgroup[-1] - newgroup[0]) < length:
                            length = newgroup[-1] - newgroup[0]
                            record = (newgroup[0], newgroup[-1])

    if not doubles:
        for group in product(*positions.values()):  # lets me create Cartesian products of all the values in 'positions'
            group = sorted(group)  # this is additional step I need to take with long strings, else the function breaks
            if (group[-1] - group[0]) < length:
                length = group[-1] - group[0]
                record = (group[0], group[-1])

    return fullstring[record[0]:record[1]+1] #remember to add +1 when extracting strings


def Tester():

    testnormal = {"without repeats": ["aaffhkksemckelloea", "fhea"],
    "1x2 repeat": ["aaffhkksemckelloea", "fheae"],
    "2x2 repeat": ["aaffhkksemckelloea", "fheaea"],
    "1x3 repeat": ["aaffhkksemckelloea", "fheaee"],
    "1x3 repeat + 1x2 repeat": ["aaffhkksemckelloea", "fheaeea"],
    "2x3 repeat": ["aaffhkksemckelloea", "fheaeeaa"]
    }

    for test in testnormal:
        print(f"Condition '{test}' took {timeit.timeit(lambda: FindMinSubstring(testnormal[test]), number=1000)} seconds to complete. Smallest substring is '{FindMinSubstring(testnormal[test])}'")

Tester()
