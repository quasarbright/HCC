import unittest
import os
import subprocess

class IntegrationTests(unittest.TestCase):
    def test_files(self):
        files = os.listdir("./tests")
        filenames = [file[:-2] for file in files if file[-2:] == ".c"]
        for filename in filenames:
            path = "./tests/"+filename
            out_path = path+".out"
            err_path = path+".err"
            is_out = os.path.isfile(out_path)
            is_err = os.path.isfile(err_path)
            if is_out or is_err:
                result = subprocess.run(["hcc",path], capture_output=True)
                if is_out:
                    with open(out_path) as out:
                        expected = out.read()
                        actual = result.stdout.decode("utf-8")
                        self.assertEqual(expected, actual, msg=path)
                if is_err:
                    with open(out_path) as err:
                        expected = err.read()
                        actual = result.stderr.decode("utf-8")
                        self.assertIn(expected, actual, msg=path)

if __name__ == "__main__":
    subprocess.run(["stack", "build"])
    unittest.main()