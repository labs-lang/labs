""" CSeq C Sequentialization Framework
	parallel backend feeder module:
	spanws multiple processes to invoke the backend using different options

	written by Omar Inverso.
"""
VERSION = 'feeder_parallel-2019.09.19'
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
	2019.09.19  cluster mode bug fix when --from == --to
	2018.05.25  sub-processes now terminated by directly killing backend
	2018.04.22  forked from latest stable feeder version
	2015.10.20  fix for backend and witness file
	2015.07.19  fix for backend llbmc and klee (Truc)
	2015.07.03  1st version, codebase inherited from cseq-feeder.py (feeder-2015.07.02)
"""

import os, sys, getopt, time, signal, subprocess, shlex
from threading import Timer, Thread
#from multiprocessing import Process, Lock, Array
import multiprocessing
import pycparser.c_parser, pycparser.c_ast, pycparser.c_generator
import core.module, core.parser, core.utils
from core.module import ModuleError

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

				  Options and Parameters below.

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Name of the executable file to run, by backend.
backendFilename = {}
backendFilename['cbmc'] = './cbmc2'

cmdoptions = {}
cmdoptions['cbmc'] = '  '  ###cmdoptions['cbmc'] = ' --bounds-check '

verificationOK = {}
verificationOK['cbmc'] = 'VERIFICATION SUCCESSFUL'

verificationKO = {}
verificationKO['cbmc'] = 'VERIFICATION FAILED'



class feeder_parallel(core.module.BasicModule):
	verbose = False

	def init(self):
		#self.addInputParam('backend', 'backend (blitz, cbmc, esbmc, llbmc, cpachecker, satabs, klee)', 'b', 'cbmc', False)
		self.addInputParam('time', 'analysis time limit (in seconds)', 't', '3600000', False)
		self.addInputParam('extrargs', 'extra arguments to use for parallel analysis (one per core)', 'x', [], False)
		self.addInputParam('from', 'first scheduling partition to analyse (default=0)', 'f', default=0, optional=True)
		self.addInputParam('to', 'last scheduling partition to analyse (default=cores-1)', 't', default=0, optional=True)
		self.addInputParam('contexts', 'execution contexts', 'r', None, False)
		self.addInputParam('unwind', 'loop unwind bound', 'u', '1', False)

		self.addOutputParam('exitcode')


	def loadfromstring(self,string,env):
		extrargs = []
		contexts = int(self.getInputParamValue('contexts'))
		unwind =  int(self.getInputParamValue('unwind'))

		#if self.getInputParamValue('extrargs') is not None:
		extrargs = self.getInputParamValue('extrargs')

		cores = len(extrargs)
		if cores == 0 or cores is None: cores = 1

		timelimit = self.getInputParamValue('time')
		backend = 'cbmc'

		coresstart = coresstop = None

		if self.getInputParamValue('from') is not None:
			coresstart = int(self.getInputParamValue('from'))
		else:
			coresstart = 0

		if self.getInputParamValue('to') is not None:
			coresstop = int(self.getInputParamValue('to'))
		else:
			coresstop = cores-1

		if coresstop-coresstart+1 > 1:
			print "Parallel analysis using %s cores, partitions [%s...%s], %s overall partitions" % (coresstop-coresstart+1,coresstart,coresstop,cores)
		else: print "No parallel analysis"

		if coresstop-coresstart+1 > multiprocessing.cpu_count():
			self.warn("exceeding the CPU count: spawning %s separate backend processes on a host with %s CPUs" % (coresstop-coresstart+1,multiprocessing.cpu_count()))


		''' Run the verification tool on the input file '''
		seqfile = core.utils.rreplace(env.inputfile, '/', '/_cs_', 1) if '/' in env.inputfile else '_cs_' + env.inputfile
		core.utils.saveFile(seqfile,string)
		cmd = backendFilename[backend] + cmdoptions[backend] + seqfile

		#try:
		if 1:
			processes = []
			lock = multiprocessing.Lock()

			# thread-safe shared vectors of
			# process identifiers, process exit codes, and spawned processes' identifiers
			# every process fills in its own entries
			pool = multiprocessing.Array('i', [-1 for i in range(cores)])  # -1=not started, 0=terminated
			code = multiprocessing.Array('i', [-1 for i in range(cores)])  # return codes
			boh = multiprocessing.Array('i', [-1 for i in range(cores)])  # child processes PIDs (i.e., PID of the spawned backend)

			pipea,pipeb = multiprocessing.Pipe()

			starttime = time.time()

			logfile = 'logs/'+seqfile+'.u%sc%s.c%s.from%sto%s' %(unwind,contexts,cores,coresstart,coresstop)

			for id in range(coresstart,coresstop+1):
				newlogfile = logfile +'.partition%s.log' % id
				#args = '' if coresstart==coresstop else extrargs[id]
				args = extrargs[id] if len(extrargs) else ''
				p = multiprocessing.Process(target=self.feed, args=(id,coresstart,coresstop, cmd+' '+args,timelimit,backend,newlogfile, pool,code,boh, lock,pipeb, starttime))
				processes.append(p)
				p.start()

			# wait for all processes to terminate,
			# if one of them finds the error will terminate all the others
			for p in processes:
				p.join()

			# in any case,
			# only one of the processes' output will make it to this point: either
			# the first process that finds an error trace (or crashes), or
			# the last one to terminate (or time out) without finding anything.
			self.output = pipea.recv()
			core.utils.saveFile(logfile+'.log',self.output)


	''' Single-process Analysis.

		- on finding a counterexample (at the moment detected by exitcode=10) terminates all other processes.
		- on crashing (exitcode=6) terminates all other processes.
		- on successfully terminating without crashing or counterexamples (exitcode=0) do nothing.

	'''
	def feed(self,   id,corestart,corestop,   cmd,timeout,backend,logfile,   pool,code,boh,   l,pipe, starttime):
		pool[id] = os.getpid()  # store this process' pid into the shared array

		print("---> [%s] START +%0.2fs pid:[%s] cmd:[%s] log:[%s] <---" %(id,time.time()-starttime,os.getpid(),cmd,logfile))

		p = core.utils.CommandPid(cmd)
		newpid = p.spawn()   # store stdout, stderr, process' return value
		boh[id] = newpid     # store the identifier for the spawned backend process

		print("---> [%s] SPAWN +%0.2fs pid:[%s] spawning pid %s <---" %(id,time.time()-starttime,os.getpid(),boh[id]))

		out,err,cod = p.wait(int(timeout))

		if cod is None: cod = -2
		else: cod = int(cod)
	
		code[id] = cod

		# for CBMC, code=0 means SAFE, code=10 means UNSAFE, code=6 means error.
		#tids = ','.join(str(x) for x in pool)
		#codes = ','.join(str(x) for x in code)
		#subs = ','.join(str(x) for x in boh)

		pool[id] = 0

		# no error trace found or analysis timeout:
		# if this is the last process to terminate,
		# update the exit code, send the output to the pipe and close it;
		# if there are still other processes running,
		# no action is taken apart from saving the logfile.
		#if not verificationOK[backend] in out.splitlines()[-1] and not verificationKO[backend] in out.splitlines()[-1]:
		if code[id] == 0 or code[id] == -9:
			s = ''

			if code[id] == 0: s = 'PASS'
			if code[id] ==  -9: s = 'SKIP'

			print("---> [%s] %s  +%0.2fs pid:[%s] cmd:[%s] <---" %(id,s,time.time()-starttime,os.getpid(),cmd))

			# save the log in any case
			core.utils.saveFile(logfile,out)

			l.acquire()

			lastone = True   # is this the only process still running?

			for k in range(corestart,corestop):
				if pool[k]!=0 and k!=id:
					lastone = False

			# last process to terminate, and no error traces found
			if lastone:
				self.setOutputParam('exitcode', code[id])

				# dump backend's output to file
				####core.utils.saveFile(logfile,out)
				pipe.send(out)
				pipe.close()
	
			l.release()

		# error trace found or error from the backend:
		# stop all other processes immediately
		if not(code[id] == 0 or code[id] == -9):
			#print("---> [%s] FAIL  +%0.2fs pid:[%s] cmd:[%s] pool:[%s] code:[%s] sub:[%s] <---" %(id,time.time()-starttime,os.getpid(),cmd,tids,codes,subs))
			print("---> [%s] FAIL  +%0.2fs pid:[%s] cmd:[%s]  <---" %(id,time.time()-starttime,os.getpid(),cmd))
			#print("---> [%s] FAIL  +%0.2fs pid:[%s] pool:[%s] <---" %(id,time.time()-starttime,os.getpid(),tids))
			#print("---> [%s] FAIL  +%0.2fs pid:[%s] code:[%s] <---" %(id,time.time()-starttime,os.getpid(),codes))
			#print("---> [%s] FAIL  +%0.2fs pid:[%s] sub:[%s]  <---" %(id,time.time()-starttime,os.getpid(),subs))

			l.acquire()

			for k in range(corestart,corestop+1):
				if boh[k] != 0 and boh[k] != -1:
					print "---> [%s] KILL  +%0.2fs pid:[%s] killing backend process %s spawned from pid %s" % (id,time.time()-starttime,os.getpid(),boh[k],pool[k])

					try:  # might have terminated meanwhile
						os.kill(boh[k],signal.SIGTERM)
						os.kill(boh[k],signal.SIGKILL)
						boh[k] = 0
						print "---> [%s] KILL success" % id
					except:
						print "---> [%s] KILL fail" % id
						pass

			# dump error trace to file
			core.utils.saveFile(logfile,out)
			pipe.send(out)
			pipe.close()

			self.setOutputParam('exitcode', code[id])

			l.release()

