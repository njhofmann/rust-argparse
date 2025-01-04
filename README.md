A (near) complete implementation of Python's [argparse](https://docs.python.org/3/library/argparse.html) library (as of 3.12.4) in Rust. 

### Behavior Notes
- if non-required flag argument not given a default value, default to empty vector
    - suppress its creation with ...

### Unsupported Behavior

- convert_arg_line_to_args as it required manual overriding in python
- error on nonsensical argument pairings
- `ArgumentParser::new(exit_on_error=...)`
    - user must handle errors in Rust like fashion with "?" & ".unwrap()"
    - might add checked versions of methods at some poin
- won't support specific positional argument splitting 
- `ArgumentParser.add_argument(type=...)`
    - internally ArgumentParser treats everything as a String for simplicity
    - use type sytem with `ArgumentParser.add_argument<T>(choices=..., default=...)` & `Namespace.get::<T>(...)`
- `ArgumentParser.parse_intermixed_args(...)` && `ArgumentParser.parse_known_intermixed_args(...)`
    - maybe at some point, but such a different style of parsing doesn't seem worth the time
- `ArgumentParser.set_defaults(...)`, `ArgumentParser.get_default(...)`, `ArgumentParser::new(argument_default=...)`
    - doesn't mix well with how library opts to handles argument types
        - ex: could set parser default as a string that isn't compatible with a custom enum
    - do support suppression of attribute creation with `ArgumentParser::new(suppress_missing_attributes=...)` to convert argument values back & forth between desired types
- `ArgumentParser.convert_arg_line_to_args(...)`
    - intended as method user's override, which isn't supported in this lib
- `ArgumentParser.register(...)`
    - don't support custom actions right now, maybe someday
    - "custom types" doesn't really make sense with how library handles types 
- `ArgumentParser.exit(...)` & `ArgumentParser.error(...)`
    - don't really see the point of these with how library is structured
- `ArgumentParser.add_argument(const=...)` can't be `None` when action is `append_const` or `store_const`
- literally can't support 
