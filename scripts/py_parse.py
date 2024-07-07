import argparse as ap
import sys

def parse_from_nargs(nargs):
    has_flag = False
    parser = ap.ArgumentParser()
    is_flag = False
    for (i, arg) in enumerate(nargs):
        match arg:
            case 'v':
                arg = '*'
            case 'z':
                arg = '?'
            case 'o':  # one or more
                arg = '+'
            case 'f':
                has_flag = True
                is_flag = True
                arg = None
            case other: # zero or one
                arg = int(other)

        if arg:
            if is_flag:
                parser.add_argument(f'--{i}', nargs=arg)
                is_flag = False
            else:
                parser.add_argument(str(i), nargs=arg)
    
    if has_flag or sys.argv[2:]:
        return parser.parse_args(sys.argv[2:])
    return parser.parse_args(['a', 'b', 'c', 'd', 'e'])

if __name__ == '__main__':
    print(parse_from_nargs(sys.argv[1]))