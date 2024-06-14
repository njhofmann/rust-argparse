import argparse as ap
import sys

def parse_from_nargs(nargs):
    parser = ap.ArgumentParser()
    for (i, arg) in enumerate(nargs):
        match arg:
            case 'v':
                arg = '*'
            case 'e':
                arg = 1
            case 'o':  # one or more
                arg = '+'
            case other: # zero or one
                arg = '?'
        parser.add_argument(str(i), nargs=arg)
    return parser.parse_args(['a', 'b', 'c', 'd', 'e'])

if __name__ == '__main__':
    print(parse_from_nargs(sys.argv[1]))