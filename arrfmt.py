from typing import List, Any, Iterator
import numpy as np
import ast


def python_to_j(input_str: str) -> str:
    # Parse the input string as a Python literal
    data = ast.literal_eval(input_str)
    ndim = np.array(data).ndim
    it = _sep()
    seps = [next(it) for _ in range(ndim)]
    seps.reverse()
    return concatenate_strings(np.array(data).astype(str), seps)


def j_to_python(j_array: str) -> str:
    """
    Convert J console output to Python array representation.
    """
    sep_stack: List[str] = []
    it = _sep()
    while True:
        sep = next(it)
        if sep not in j_array:
            break
        sep_stack.append(sep)
    python_array = split_string_reverse_order(j_array, sep_stack)
    return repr(np.array(python_array).astype(int).tolist())


def _sep() -> Iterator[str]:
    yield ' '
    i: int = 1
    while True:
        yield "\n" * i
        i += 1


def concatenate_strings(x: np.ndarray[str, Any], seps: List[str]) -> str:
    # Define a helper function to concatenate strings with a specific separator
    def concatenate_with_sep(arr: np.ndarray[str, Any], sep: str) -> str:
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
    # if x is of type np.ndarray, convert it to a string
    ret: str = ""
    if isinstance(x, str):
        ret = x
    if isinstance(x, np.ndarray):
        ret = x.item()
    return ret


def split_string_reverse_order(s: str, separators: List[str]) -> Any:
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
