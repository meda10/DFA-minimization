import os
import subprocess
import sys
import filecmp
import shlex


class bcolors:
    SUCCESS = '\033[0;32m'
    NORMAL = '\033[0m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'


def print_results(tests):
    for test in tests:
        name = tests[test][0]
        if tests[test][1]:
            print("{}[SUCCESS]{} {} ".format(bcolors.SUCCESS, bcolors.NORMAL, name))
        else:
            print("{}[FAILED]{} {} ".format(bcolors.FAIL, bcolors.NORMAL, name))


def compare_results(output, expected):
    try:
        return filecmp.cmp(output, expected)
    except FileNotFoundError as e:
        print(e)
        sys.exit(1)


def run_test(path_to_test_dir, program, cwd, args):
    # path_to_test_dir = os.path.join(cwd, 'test')
    # test_files = os.path.join(path_to_test_dir, 'in')
    test_files = find_test(os.path.join(path_to_test_dir, 'in'))

    args.insert(0, program)
    args[0] = './' + args[0]

    if not os.path.exists(os.path.join(path_to_test_dir, 'out')):
        os.makedirs(os.path.join(path_to_test_dir, 'out'))
    if not os.path.exists(os.path.join(path_to_test_dir, 'err')):
        os.makedirs(os.path.join(path_to_test_dir, 'err'))

    for test in test_files:
        name = test_files[test][0]

        input_file_path = os.path.join(path_to_test_dir, "in/" + name + '.in')
        output_file_path = os.path.join(path_to_test_dir, "out/" + name + '.out')
        expected_output_file_path = os.path.join(path_to_test_dir, "valid/" + name + '.out')
        error_file_path = os.path.join(path_to_test_dir, "err/" + name + '.err')

        error_out = None
        try:
            input_file = open(input_file_path, 'r')
            output_file = open(output_file_path, 'w+')
            error_file = open(error_file_path, "w+")
        except Exception as e:
            print(e)
            sys.exit(1)

        process = subprocess.Popen(args, shell=False, cwd=cwd, stdin=input_file, stdout=output_file, stderr=error_file)
        outs, err = process.communicate()
        # error = process.returncode

        process.kill()
        process.terminate()

        input_file.close()
        output_file.close()
        error_file.close()

        # if error != 0:
        #     error_file = open(error_file_path, "w+")
        #     error_file.write("{}".format(error))
        #     error_file.close()
        #     if error_out is not None:
        #         print(error_out)
        #     # return 0

        test_files[test][1] = compare_results(output_file_path, expected_output_file_path)
    print_results(test_files)


def find_test(test_dir):
    test_dic = {}
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file[-2:] == 'in':
                test_dic[os.path.join(root, file)] = [file[:-3], False]
    return test_dic


if __name__ == '__main__':

    project_root = os.getcwd()
    path_to_program_from_root = 'dka-2-mka'

    print("----- Argument -t -----")
    path_to_test_dir = os.path.join(project_root, '../Done/test/arg_t')
    run_test(path_to_test_dir, path_to_program_from_root, project_root, shlex.split("-t"))

    print("----- Argument -i -----")
    path_to_test_dir = os.path.join(project_root, '../Done/test/arg_i')
    run_test(path_to_test_dir, path_to_program_from_root, project_root, shlex.split("-i"))
