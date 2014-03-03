import os
import re
import subprocess
import StringIO
from glob import glob

test_dir = "tests"
luajit_exec = "luajit"
diff_exec = "diff"

def lua_files(test_dir):
	for dirpath, dirnames, filenames in os.walk(test_dir):
		for filename in filenames:
			m = re.match(r'([^.]+)\.lua$', filename)
			if m:
				yield dirpath, m.group(1)

class LabelSource:
	def __init__(self):
		self.index = 1
		self.defs = {}
	def get(self, lab):
		if not lab in self.defs:
			self.defs[lab] = "X%03d" % self.index
			self.index += 1
		return self.defs[lab]

def proto_lines(bcfile):
	for line in bcfile:
		if re.match(r'\s*$', line): break
		if re.match(r'\d+ (  |=>) FNEW', line):
			line = re.sub(r'"([^"]+)":(\d+)', r'\1:\2', line)
		yield line

def normalize(source, outfile):
	labels = LabelSource()
	for line in source:
		rline = None
		m = re.match(r'(\d{4}) (  |=>) (.*)', line)
		lab, ref, rem = m.groups()
		mr = re.match(r'([A-Z0-9]+\s+)(\d+) => (\d+)(.*)', rem)
		if mr:
			ins, reg, jmp, xrem = mr.groups()
			jmp = labels.get(jmp)
			rem = "%s%s => %s%s" % (ins, reg, jmp, xrem)
		if ref == '=>':
			lab = labels.get(lab)
		else:
			lab = "    "
		rline = "%4s %s %s\n" % (lab, ref, rem)

		outfile.write(rline)

def parse(bcfile, outfile):
	for line in bcfile:
		m = re.match(r'-- BYTECODE -- ([^:]+):(\d+-\d+)', line)
		if m:
			name, lines = m.groups()
			name = re.sub(r'"([^"]+)"', r'\1', name)
			outfile.write('\n-- BYTECODE -- %s:%s\n' % (name, lines))
			normalize(proto_lines(bcfile), outfile)

def do_process(cmd, dst):
	src = subprocess.Popen(cmd, stdout = subprocess.PIPE).stdout
	parse(src, dst)

def do_process_output(cmd):
	sf = StringIO.StringIO()
	do_process(cmd, sf)
	s = sf.getvalue()
	sf.close()
	return s

def expected_bytecode(name, fullname):
	s = do_process_output([luajit_exec, "-bl", fullname])
	yield s, "luajit"
	for expect_filename in glob("tests/expect/*.txt"):
		m = re.match(r'tests/expect/([^/]+)\.(expect\d+)\.txt$', expect_filename)
		if m and m.group(1) == name:
			ef = open(expect_filename, "r")
			sf = StringIO.StringIO()
			parse(ef, sf)
			s = sf.getvalue()
			ef.close()
			sf.close()
			yield s, m.group(2)

def write_diff(a, b, a_name, b_name):
	fna = ".out-%s.txt" % a_name
	fnb = ".out-%s.txt" % b_name
	af = open(fna, "w")
	bf = open(fnb, "w")
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

for dirpath, name in lua_files(test_dir):
	fullname = os.path.join(dirpath, name + ".lua")

	output_test = do_process_output([luajit_exec, "run.lua", "-b", fullname])
	msg, source = compare_to_ref(name, fullname, output_test)

	led = " " if msg == "pass" else "*"
	msg_ext = "%s / %s" % (msg, source) if source and source != "luajit" else msg

	print("%s %-24s%s" % (led, name, msg_ext))
