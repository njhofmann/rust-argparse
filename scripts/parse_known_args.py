import argparse as ap
import sys

def parse_from_nargs(nargs):
    has_flag = False
    parser = ap.ArgumentParser()
    is_flag = False
    idx = 0
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
                print(parser.add_argument(f'--{idx}', nargs=arg))
                is_flag = False
            else:
                print(parser.add_argument(str(idx), nargs=arg))

        if not is_flag:
            idx += 1
    
    return parser.format_usage(), parser.parse_args(sys.argv[2:])


if __name__ == '__main__':
    print(parse_from_nargs(sys.argv[1]))