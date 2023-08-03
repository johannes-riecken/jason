import unittest
from formatter import j_to_python, python_to_j
import numpy as np


class JToPythonTestCase(unittest.TestCase):
    def testZeroDim(self):
        self.assertEqual("1", j_to_python("1"), '0-dimensional (scalar)')
        self.assertEqual("[1, 2, 3]", j_to_python("1 2 3"), '1-dimensional (vector)')
        self.assertEqual("[[1, 2, 3], [4, 5, 6]]", j_to_python("1 2 3\n4 5 6"), '2-dimensional (matrix)')
        self.assertEqual("[[[1, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]]]", j_to_python("1 2 3\n4 5 6\n\n7 8 9\n10 11 12"), "3-dimensional")


class PythonToJTestCase(unittest.TestCase):
    def testZeroDim(self):
        self.assertEqual(python_to_j(str(np.array(5).tolist())), "5")
        self.assertEqual(python_to_j(str(np.array([1, 2, 3]).tolist())), "1 2 3")
        self.assertEqual(python_to_j(str(np.array([[1, 2], [3, 4]]).tolist())), "1 2\n3 4")
        self.assertEqual(python_to_j(str(np.array([[[1, 2], [3, 4]], [[5, 6], [7, 8]]]).tolist())), "1 2\n3 4\n\n5 6\n7 8")
