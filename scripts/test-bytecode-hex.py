import os
import re
import sys
import subprocess
import StringIO
from glob import glob

test_dir = "tests"
luajit_exec = "luajit"
luajit_x = "./src/luajit-x"
diff_exec = "diff"

def lua_files(test_dir):
	for dirpath, dirnames, filenames in os.walk(test_dir):
		for filename in sorted(filenames):
			m = re.match(r'([^.]+)\.lua$', filename)
			if m:
				yield dirpath, m.group(1)

def do_process(cmd, dst):
	src = subprocess.Popen(cmd, stdout = subprocess.PIPE).stdout
	for line in src:
		dst.write(re.sub(r'\x0d', '', line))

def do_process_output(cmd):
	sf = StringIO.StringIO()
	do_process(cmd, sf)
	s = sf.getvalue()
	sf.close()
	return s

def source_fullname_ref(fullname):
    fullname_alias = re.sub(r'\.lua$', '.alias.lua', fullname)
    return fullname_alias if os.path.isfile(fullname_alias) else fullname

def expected_bytecode(name, fullname):
	subprocess.check_call([luajit_exec, "-bg", source_fullname_ref(fullname), ".out.raw"])
	s = do_process_output([luajit_x, "-bx", ".out.raw"])
	yield s, "luajit"
	expect_dir = os.path.join("tests", "expect_hex")
	for expect_filename in glob(os.path.join(expect_dir, "*.txt")):
		efilename = os.path.basename(expect_filename)
		m = re.match(r'([^.]+)\.(expect\d+)\.txt$', efilename)
		if m and m.group(1) == name:
			ef = open(expect_filename, "rb")
			sf = StringIO.StringIO()
			parse(ef, sf)
			s = sf.getvalue()
			ef.close()
			sf.close()
			yield s, m.group(2)

def write_diff(a, b, a_name, b_name):
	fna = "tests/log/%s.txt" % a_name
	fnb = "tests/log/%s.%s.txt" % (a_name, b_name)
	af = open(fna, "wb")
	bf = open(fnb, "wb")
	af.write(a)
	bf.write(b)
	af.close()
	bf.close()

	diff_output = subprocess.Popen([diff_exec, "-U", "4", fna, fnb], stdout=subprocess.PIPE).communicate()[0]
	diff_file = open("tests/log/%s.%s.diff" % (a_name, b_name), "w")
	diff_file.write(diff_output)
	diff_file.close()

def compare_to_ref(name, fullname, output_test):
	for s, source in expected_bytecode(name, fullname):
		if s == output_test:
			return "pass", source
		else:
			write_diff(output_test, s, name, source)
	return "fail", None

if not os.path.isdir("tests/log"):
	try:
		print "Creating directory tests/log..."
		os.mkdir("tests/log")
	except:
		print "Error creating directory tests/log."
		sys.exit(1)

try:
	subprocess.check_call([luajit_exec, "-e", ""])
except:
	print "Error calling luajit."
	print "Please make sure that luajit executable is in the current PATH."
	sys.exit(1)

for filename in glob("tests/log/*"):
	os.remove(filename)

for dirpath, name in lua_files(test_dir):
	fullname = os.path.join(dirpath, name + ".lua")

	output_test = do_process_output([luajit_x, "-bx", fullname])
	msg, source = compare_to_ref(name, fullname, output_test)

	led = " " if msg == "pass" else "*"
	msg_ext = "%s / %s" % (msg, source) if source and source != "luajit" else msg

	print("%s %-24s%s" % (led, name, msg_ext))
