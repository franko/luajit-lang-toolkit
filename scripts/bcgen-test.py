import os
import re
import subprocess

test_dir = "tests"
luajit_exec = "luajit"

log = open("bcgen-log.txt", "w")

for dirpath, dirnames, filenames in os.walk(test_dir):
    for filename in filenames:
        m = re.match(r'([^.]+)\.lua$', filename)
        if m:
            fullname = os.path.join(dirpath, filename)
            test_name = m.group(1)
            out_tst = None
            run_error = False
            try:
                out_tst = subprocess.check_output([luajit_exec, "run.lua", fullname])
            except subprocess.CalledProcessError:
                run_error = True
            out_ref = subprocess.check_output([luajit_exec, fullname])
            msg = None
            if run_error:
                msg = "* %-24s\tFail to run.\n" % test_name
            elif out_tst == out_ref:
                if out_tst in ["", "\n", "\r\n"] or not out_tst:
                    msg = "- %-24s\tPass / no output expected.\n" % test_name
                else:
                    msg = "- %-24s\tPass.\n" % test_name
            else:
                msg = "* %-24s\tDiffer:\n" % test_name
                msg = msg + ("*** Reference ***\n%s\n" % out_ref)
                msg = msg + ("*** Test Program ***\n%s\n" % out_tst)
            log.write(msg)

log.close()
