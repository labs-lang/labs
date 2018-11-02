""" CSeq C Sequentialization Framework
	parallel backend feeder module:
	spanws multiple processes to invoke the backend using different options

	written by Omar Inverso.
"""
VERSION = 'labs-feeder_parallel-2018.10.23'
# VERSION = 'labs-feeder_parallel-2018.05.31'
#VERSION = 'feeder_parallel-2018.05.25'
#VERSION = 'feeder_parallel-2018.04.22'
#VERSION = 'feeder-2015.07.16'     # CSeq 1.0 Release - ASE2015
"""

Prerequisites:
	Input correctly instrumented for the specified backend.

TODO:
	- handle keyboard interrupts silently
	- when the backend is not available, there should be an exception.

Changelog:
	2018.05.31  forked from feeder_parallel for sequentialised programs (feeder_parallel-2018.05.25)
"""

import os, sys, getopt, time, signal, subprocess, shlex

from collections import Counter
#from multiprocessing import Process, Lock, Array
import multiprocessing
import pycparser.c_parser, pycparser.c_ast, pycparser.c_generator
import core.module, core.parser, core.utils
from core.module import ModuleError
from utils import findpropositionalvar, findpropositionalvarsize, get_bin

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

				  Options and Parameters below.

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Name of the executable file to run, by backend.
backendFilename = {}
backendFilename['esbmc'] = 'esbmc'
backendFilename['cbmc-assumptions'] = './cbmc-simulator'
backendFilename['llbmc'] = 'llbmc'
backendFilename['blitz'] = 'blitz'
backendFilename['satabs'] = 'satabs'
# backendFilename['2ls'] = 'summarizer'
# backendFilename['smack'] = 'smack-verify.py'
backendFilename['klee'] = 'klee'
backendFilename['cpachecker'] = 'cpa.sh'
# backendFilename['spin'] = 'spin'

# Command-line parameters, by backend.
cmdoptions = {}
cmdoptions['esbmc'] = ' --no-slice --no-bounds-check --no-div-by-zero-check --no-pointer-check --unwind 1 --no-unwinding-assertions '
cmdoptions['cbmc-assumptions'] = '  '  ###cmdoptions['cbmc'] = ' --bounds-check '
###cmdoptions['cbmc'] = '  --unwind 1 --no-unwinding-assertions '
cmdoptions['llbmc'] = ' -no-max-function-call-depth-checks -no-memory-free-checks -no-shift-checks -no-memcpy-disjoint-checks -no-memory-access-checks -no-memory-allocation-checks --max-loop-iterations=1 --no-max-loop-iterations-checks --ignore-missing-function-bodies -no-overflow-checks -no-div-by-zero-checks'
cmdoptions['blitz'] = '  --terminate-on-firstbug '
cmdoptions['satabs'] = ' '
# cmdoptions['2ls'] = ' '
# cmdoptions['smack'] = ' --unroll 1 '
cmdoptions['klee'] = ' -exit-on-error '
cmdoptions['cpachecker'] = ' -preprocess -heap 15000M -timelimit 86400 -noout -predicateAnalysis '

# Command-line parameters, by backend - when no sequentialisation is performed.
cmdoptionsNOSEQ = {}
cmdoptionsNOSEQ['esbmc'] = ' --no-slice --no-bounds-check --no-div-by-zero-check --no-pointer-check '
cmdoptionsNOSEQ['cbmc-assumptions'] = '  '
cmdoptionsNOSEQ['llbmc'] = ' -no-max-function-call-depth-checks -no-memory-free-checks -no-shift-checks -no-memcpy-disjoint-checks -no-memory-access-checks -no-memory-allocation-checks --ignore-missing-function-bodies -no-overflow-checks -no-div-by-zero-checks '
# cmdoptionsNOSEQ['blitz'] = '  --terminate-on-firstbug '   # No support concurrency
cmdoptionsNOSEQ['satabs'] = ' '
# cmdoptionsNOSEQ['2ls'] = ' '     # no concurrency support
# cmdoptionsNOSEQ['smack'] = ' '
cmdoptionsNOSEQ['klee'] = ' '
# cmdoptionsNOSEQ['cpachecker'] = ' -preprocess -heap 15000M -timelimit 86400 -noout -predicateAnalysis '  # No support concurrency



class labs_feeder_parallel(core.module.BasicModule):
	verbose = False

	def init(self):
		self.addInputParam('backend', 'backend (blitz, cbmc, esbmc, llbmc, cpachecker, satabs, klee)', 'b', 'cbmc-assumptions', False)
		self.addInputParam('time', 'analysis time limit (in seconds)', 't', '3600000', False)
		self.addInputParam('depth', 'limit search depth', 'd', '0', False)   # depth parameter for the competition
		self.addInputParam('extrargs', 'extra arguments to use for parallel analysis (one per core)', 'x', [], False)
		self.addInputParam('simulate', '0 for verification mode; otherwise # of traces to generate', 't', '0', optional=True)  # TODO
		self.addInputParam('info', 'LAbS system information', 'i', None, False)
		self.addOutputParam('exitcode')

	def loadfromstring(self, string, env):
		extrargs = []
		simulate = 0
		cores = None


		if self.getInputParamValue('extrargs') is not None:
			extrargs = self.getInputParamValue('extrargs')
			cores = len(extrargs)

		if cores == 0 or cores is None: cores = 1

		if self.getInputParamValue('show') is not None:
			self.output = string
			return

		if self.getInputParamValue('simulate') is not None:
			simulate = int(self.getInputParamValue('simulate'))
			cores = 16


		if cores > 1: print "Parallel analysis using %s cores" % cores
		else: print "No parallel analysis"

		depth = int(self.getInputParamValue('depth'))
		timelimit = self.getInputParamValue('time')
		backend = self.getInputParamValue('backend')
		backendparams = self.getInputParamValue('backendparams')
		witness = self.getInputParamValue('witness')
		info = self.getInputParamValue('info')

		''' Run the verification tool on the input file '''
		seqfile = core.utils.rreplace(env.inputfile, '/', '/_cs_', 1) if '/' in env.inputfile else '_cs_' + env.inputfile
		logfile = seqfile + '.' + backend + '.log' if witness is None else witness

		core.utils.saveFile(seqfile, string)

		if backend == 'esbmc':
			cmd = backendFilename[backend] + cmdoptions[backend] + seqfile
			if depth != 0:
				cmd += ' --depth %s ' % str(depth)
		elif backend == 'cbmc-assumptions':
			cmd = backendFilename[backend] + cmdoptions[backend] + seqfile
			if depth != 0:
				cmd += ' --depth %s ' % str(depth)
		elif backend == 'llbmc':
			# llbmc and clang need to be matched
			clangpath = '' if self.getInputParamValue('llvm') is None else  self.getInputParamValue('llvm')
			clangexe = clangpath +'clang'
			cmd = "%s -c -g -I. -emit-llvm %s -o %s.bc 2> %s " % (clangexe, seqfile, seqfile[:-2], logfile)
			p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
			out, err = p.communicate()
			core.utils.saveFile('clang_stdout.log', out)
			core.utils.saveFile('clang_stderr.log', err)
			cmd = backendFilename[backend] + ' ' + cmdoptions[backend] + ' ' + seqfile[:-2] + '.bc'
		elif backend == 'blitz':
			cmd = backendFilename[backend] + cmdoptions[backend] + seqfile
			if depth != 0:
				cmd += ' --depth %s ' % str(depth)
		elif backend == 'satabs':
			cmd = backendFilename[backend] + cmdoptions[backend] + seqfile
		elif backend == '2ls':
			cmd = backendFilename[backend] + cmdoptions[backend] + seqfile
		elif backend == 'klee':   # klee needs llvm-gcc version 2.9
			clangpath = '' if self.getInputParamValue('llvm') is None else  self.getInputParamValue('llvm')
			clangexe = clangpath + 'llvm-gcc'
			cmd = "%s -c -g -emit-llvm %s -o %s.bc " % (clangexe, seqfile, seqfile[:-2])
			p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
			out, err = p.communicate()
			core.utils.saveFile('clang_stdout.log', out)
			core.utils.saveFile('clang_stderr.log', err)
			cmd = backendFilename[backend] + ' ' + cmdoptions[backend] + ' ' + seqfile[:-2] + '.bc'
		elif backend == 'cpachecker':
			cmd = backendFilename[backend] + cmdoptions[backend] + seqfile
		elif backend == 'smack':
			cmd = backendFilename[backend] + cmdoptions[backend] + seqfile


		#try:
		if simulate == 0:
			processes = []
			lock = multiprocessing.Lock()
			#cores = 4 # multiprocessing.cpu_count()

			# thread-safe shared vectors of
			# process identifiers, process exit codes, and spawned processes' identifiers
			# every process fills in its own entries
			pool = multiprocessing.Array('i', [-1 for i in range(cores)])  # -1=not started, 0=terminated
			code = multiprocessing.Array('i', [-1 for i in range(cores)])  # return codes
			boh = multiprocessing.Array('i', [-1 for i in range(cores)])  # child processes PIDs

			pipea,pipeb = multiprocessing.Pipe()

			starttime = time.time()

			if cores == 1:
				args = "" if len(extrargs) == 0 else extrargs[0]
				p = multiprocessing.Process(target=self.feed, args=(0,cores, cmd+' '+args,timelimit,backend,logfile, pool,code,boh, lock,pipeb, starttime))
				processes.append(p)
				p.start()
			else:
				for k in range(0,cores):
					p = multiprocessing.Process(target=self.feed, args=(k,cores, cmd+' '+extrargs[k],timelimit,backend,logfile, pool,code,boh, lock,pipeb, starttime))
					processes.append(p)
					p.start()

			# in any case,
			# only one of the processes' output will make it to this point: either
			# the first process that finds an error trace (or crashes), or
			# the last one to terminate (or time out) without finding anything.
			self.output = pipea.recv()
			#print self.output
			core.utils.saveFile(logfile,self.output)

			# wait for all processes to terminate,
			# if one of them finds the error will terminate all the others
			for p in processes:
				p.join()
		else:
			## Simulation mode ##
			p = multiprocessing.Pool(cores)
			with open(seqfile+".map") as f:
				self.lines = f.readlines()

			results = [p.apply_async(feed_sim, (self, cmd, info, timelimit, i, seqfile)) 
				for i in range(simulate)]
			
			done = [r.get() for r in results]
			p.close()

			pass_count, fail_count = Counter(), Counter()

			for result in done:
				for prop, success in result.items():
					if success:
						pass_count[prop] += 1
					else:
						fail_count[prop] += 1

			self.output = "PASS: {}\nFAIL: {}".format(str(pass_count), str(fail_count))
			

	''' Single-process Analysis.

		- on finding a counterexample (at the moment detected by exitcode=10) terminates all other processes.
		- on crashing (exitcode=6) terminates all other processes.
		- on successfully terminating without crashing or counterexamples (exitcode=0) do nothing.

	'''
	def feed(self,id,cores, cmd,timeout,backend,logfile, pool,code,boh, l,pipe, starttime):
		pool[id] = os.getpid()  # store this process' pid into the shared array

		tids = ','.join(str(x) for x in pool)
		codes = ','.join(str(x) for x in code)
		subs = ','.join(str(x) for x in boh)

		#print("---> [%s] START +%0.2fs pid:[%s] cmd:[%s] pool:[%s] code:[%s] sub:[%s] <---" %(id,time.time()-starttime,os.getpid(),cmd,tids,codes,subs))
		print("---> [%s] START +%0.2fs pid:[%s] cmd:[%s] <---" %(id,time.time()-starttime,os.getpid(),cmd))
		

		p = core.utils.CommandPid(cmd)
		newpid = p.spawn()   # store stdout, stderr, process' return value
		boh[id] = newpid     # store the identifier for the spawned backend 
		###########print ("SPAWNED %s" % newpid)

		out,err,cod = p.wait(int(timeout))

		if cod is None: cod = -2
		else: cod = int(cod)
	

		code[id] = cod


		####if 'warning' in err: self.warn('warnings on stderr from the backend')

		# for CBMC, code=0 means SAFE, code=10 means UNSAFE, code=6 means error.
		tids = ','.join(str(x) for x in pool)
		codes = ','.join(str(x) for x in code)
		subs = ','.join(str(x) for x in boh)

		pool[id] = 0

		# no error trace found or analysis timeout:
		# if this is the last process to terminate,
		# save the log;
		# if there are still other processes running,
		# no action taken.
		if code[id] == 0 or code[id] == -9:
			s = ''
			if code[id] == 0: s = 'PASS'
			if code[id] ==  -9: s = 'SKIP'
			#print("---> [%s] %s  +%0.2fs pid:[%s] cmd:[%s] pool:[%s] code:[%s] sub:[%s] <---" %(id,s,time.time()-starttime,os.getpid(),cmd,tids,codes,subs))
			print("---> [%s] %s  +%0.2fs pid:[%s] cmd:[%s] <---" %(id,s,time.time()-starttime,os.getpid(),cmd))

			l.acquire()

			lastone = True   # is this the only process still running?

			for k in range(0,cores):
				if pool[k]!=0 and k!=id:
					lastone = False

			# last process to terminate, and no error traces found
			if lastone:
				self.setOutputParam('exitcode', code[id])

				# dump backend's output to file
				######core.utils.saveFile(logfile,out)
				pipe.send(out)
				pipe.close()
	
			l.release()

		# error trace found or error from the backend:
		# stop all other processes immediately
		if not(code[id] == 0 or code[id] == -9):
			#print("---> [%s] FAIL  +%0.2fs pid:[%s] cmd:[%s] pool:[%s] code:[%s] sub:[%s] <---" %(id,time.time()-starttime,os.getpid(),cmd,tids,codes,subs))
			print("---> [%s] FAIL  +%0.2fs pid:[%s] cmd:[%s] <---" %(id,time.time()-starttime,os.getpid(),cmd))

			l.acquire()

			for k in range(0,cores):
				if boh[k] != 0:
					try:  # might have terminated meanwhile
						os.kill(boh[k],signal.SIGTERM)
						os.kill(boh[k],signal.SIGKILL)
						boh[k] = 0
					except:
						pass

			# dump error trace to file
			######core.utils.saveFile(logfile,out)
			pipe.send(out)
			pipe.close()

			self.setOutputParam('exitcode', code[id])

			if code[id] not in (6,10): self.warn('unknown return value (%s) from the backend' %code[id])

			l.release()

def feed_sim(mod, cmd, info, timeout, number, basename):
	"""Single-process simulation 
	"""
	place = {
		"I": 		'c init::1::_I!0@1#1 ',
		"Lvalue": 	'c init::1::_Lvalue!0@1#1 ',
		"E": 		'c init::1::_E!0@1#1 '
	}
	bitwidths = {
		"char": 8,
		"short": 16,
		"int": 32
	}

	assumes = []
	for (typ, pos, offset, value) in info.instrument():
		bitw = bitwidths[typ]

		startvar = findpropositionalvar(place[pos],mod.lines,offset*bitw).bind(mod)
		assumes.extend(
			"%i=%s" % x
			for x in
			zip(range(startvar,startvar+bitw), get_bin(int(value),bitw)[::-1])
		)
	
	cmd += " --assume " + ",".join(assumes) 
	p = core.utils.CommandPid(cmd)
	newpid = p.spawn()
	out,err,cod = p.wait(int(timeout))
	if cod == 0:
		# Something's wrong, simulations should always fail
		raise "A simulation incorrectly ended without generating a trace"

	properties_log = [l for l in out.splitlines() if l.startswith(">>>")]
	properties_log = [l.split(" ") for l in properties_log]
	properties = {}
	for name, result in properties_log:
		if name[3:] not in properties:
			properties[name[3:]] = (True if "satisfied" in result else False)
		else:
			# Only update the dictionary if we have a violation
			if "violated" in result:
				properties[name[3:]] = False	

	logfile = "{}.{}.log".format(basename, number)
	core.utils.saveFile(logfile, out)

	return properties
