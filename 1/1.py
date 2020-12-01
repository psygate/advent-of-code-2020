def part1(values: list, tgt_value=2020):
    values = sorted(values)

    for l_value in values:
        for r_value in values:
            if r_value + l_value > tgt_value:
                break
            elif r_value + l_value == tgt_value:
                print(f"{r_value} * {l_value} = {r_value * l_value}")


def part2(values: list, tgt_value=2020):
    values = sorted(values)

    for l_value in values:
        for r_value in values:
            if r_value + l_value > tgt_value:
                break
            for c_value in values:
                if r_value + l_value + c_value > tgt_value:
                    break
                elif r_value + l_value + c_value == tgt_value:
                    print(f"{r_value} * {l_value} * {c_value} = {r_value * l_value * c_value}")


def main():
    with open("input", "r") as f:
        values = [int(x) for x in f.readlines()]

    part1(values)
    part2(values)


if __name__ == '__main__':
    main()
