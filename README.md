A (near) complete implementation of Python's [argparse](https://docs.python.org/3/library/argparse.html) library (as of 3.12.4) in Rust. 

### Unsupported Behavior

- deliberatly not supported as design decision
    - convert_arg_line_to_args as it required manual overriding in python
    - error on nonsensical argument pairings
    - ArgumentParser.exit_on_error
        - raise errors to user in rust like fashion
    - won't support specific positional argument splitting 
    - intermixed args
- literally can't support 
