- test improvement
    - remove setup from retrieval 
    - split usage display, coverage w/ more metavar
- posn arg work
    - mult posn arguments w same dest / name
    - support actions for posn args
- arg parser + arg builder methods
- major work items
    - custom actions
- setup proper crate lib structure
    - integration tests run w/ cargo test
     check library release requirements
    - only make necessary methods pub
    - keep private
        - argument name
    - write documentation
    - how to do boolean flags
    - test can't fetch flag w/ abbre
    - prefix chargs
        - mixed gives error
    - how to release library guide

- will not support / deliberatly not supported
    - convert_arg_line_to_args as it required manual overriding in python
    - error on nonsensical argument pairings
    - ArgumentParser.exit_on_error
        - raise errors to user in rust like fashion
    - won't support specific positional argument splitting 
    - intermixed args

- maybe but probably not
    - ArgumentParser.parse_args(namespace=)

- note
    - can't have non-required positional arguments
    - clone retrieval so can be used multiple times
    - can't have non-store action positional argument 
    - default value only allowed for store action

- long term
    - redo all some / none checks
    - return refs?
    - Option<&T> --> &Option<T>
    - return refs like index
    - iter over collections
    - input args are &str