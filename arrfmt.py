import numpy as np
import ast


def python_to_j(input_str):
    # Parse the input string as a Python literal
    data = ast.literal_eval(input_str)
    ndim = np.array(data).ndim
    it = _sep()
    seps = [next(it) for _ in range(ndim)]
    seps.reverse()
    return concatenate_strings(np.array(data).astype(str), seps)


def j_to_python(j_array):
    """
    Convert J console output to Python array representation.
    """
    sep_stack = []
    it = _sep()
    while True:
        sep = next(it)
        if sep not in j_array:
            break
        sep_stack.append(sep)
    python_array = split_string_reverse_order(j_array, sep_stack)
    return repr(np.array(python_array).astype(int).tolist())


def _sep():
    yield ' '
    i = 1
    while True:
        yield "\n" * i
        i += 1


def concatenate_strings(x, seps):
    # Define a helper function to concatenate strings with a specific separator
    def concatenate_with_sep(arr, sep):
        return sep.join(arr)

    # Get the number of dimensions of the input array
    ndim = x.ndim

    # Check that the number of separators matches the number of dimensions
    if len(seps) != ndim:
        raise ValueError(f"The number of separators ({len(seps)}) must match the number of dimensions of the array ({ndim})")

    # Iteratively concatenate the strings along each axis
    for i in range(ndim-1, -1, -1):
        # Get the corresponding separator
        sep = seps[i]

        # Concatenate the strings along the current axis
        x = np.apply_along_axis(concatenate_with_sep, i, x, sep)

    return x


def split_string_reverse_order(s, separators):
    # Base case: if there are no more separators, return the string as is.
    if not separators:
        return s

    # Take the last separator in the list
    separator = separators[-1]

    # Split the string using the current separator
    split_s = s.split(separator)

    # Recursively split each substring using the remaining separators
    split_s = [split_string_reverse_order(sub_s, separators[:-1]) for sub_s in split_s]

    return split_s
