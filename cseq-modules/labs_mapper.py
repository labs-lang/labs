""" CSeq C Sequentialization Framework
	mapper module

	maps from variables in the input program to propositional variables in the propositional formula (based on our forked version of CBMC)

	written by Omar Inverso.
"""
VERSION = 'labs_mapper-2020.05.19'
#VERSION = 'labs_mapper-2018.10.22'  # forked from mapper-2018.05.24 for sequentialised programs
#VERSION = 'mapper-2018.05.24'
#VERSION = 'mapper-2018.04.21'
#VERSION = 'feeder-2015.07.16'     # CSeq 1.0 Release - ASE2015
"""

Prerequisites:
	Input parsable by the backend.

Note: when the map lookup is insuccessfully there may be three cases:
	1. you are not using the modified CBMC version that generates an extended map at the end of the DIMACS fie
	2. the input program is very simple and thus trivially verifies safe, so the DIMACS is not generated
	3. the variable or function name is wrong.

TODO:
	- add explicit timeout parameter (now hardcoded) for generating the propositional formula
	- when the backend is not available, there should be an exception.

Changelog:
	2020.05.19	adjusted for SLiVER-1.9 (Luca Di Stefano)
	2018.05.31  forked from mapper for sequentialised programs
	2018.04.21  forked from latest stable feeder module
"""

import os, sys, getopt, time, signal, subprocess, shlex
import math
from threading import Timer
import pycparser.c_parser, pycparser.c_ast, pycparser.c_generator
import core.module, core.parser, core.utils
from core.module import ModuleError


from info import Info
from utils import findpropositionalvar, findpropositionalvarsize, get_bin

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

				  Options and Parameters below.

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
backendFilename = {}
backendFilename['cbmc-assumptions'] = '../cbmc/cbmc-simulator'

cmdLineOptions = {}
cmdLineOptions['cbmc-assumptions'] = ' ';


class labs_mapper(core.module.BasicModule):

	def init(self):
		self.addInputParam('backend', 'backend', 'b', default='cbmc-assumptions', optional=False)
		self.addInputParam('split', '....', 'v', 'choice', optional=False)
		self.addInputParam('steps', 'number of system evolutions', 's', '1', optional=False)
		self.addInputParam('cores', 'number of cores (power of two) for parallel analysis (0 = auto)', 'c', '1', optional=False)
		self.addInputParam('simulate', '0 for verification mode; otherwise # of traces to generate', 't', '0', optional=True)  # TODO
		self.addInputParam('info', 'LAbS system info', 'i', '', optional=True)

		self.addOutputParam('extrargs')
		self.addOutputParam('info')
		self.addOutputParam('simulate')

	def loadfromstring(self, string, env):
		contexts = 0
		cmdline = ''

		contexts = int(self.getInputParamValue('steps'))
		backend = self.getInputParamValue('backend')
		spliton = self.getInputParamValue('split')
		cores = int(self.getInputParamValue('cores'))  #TODO: check cores is power of two


		simulate = self.getInputParamValue('simulate')

		if backend != 'cbmc-assumptions': self.error("backend (%s) not supported" % backend)

		seqfile = core.utils.rreplace(env.inputfile, '/', '/_cs_', 1) if '/' in env.inputfile else '_cs_' + env.inputfile
		core.utils.saveFile(seqfile, string) 

		''' Run the verification tool on the input file '''
		if backend == 'cbmc-assumptions':
			cmdline = backendFilename[backend] + " " + seqfile + " --dimacs | grep \"^c \""    #--outfile " + seqfile+".dimacs"

		command = core.utils.Command(cmdline)
		out, err, code = command.run(timeout=int(7200))[0:3]   # store stdout, stderr, process' return value
		if code != 0:
			raise IOError(err)

		lines = out.splitlines()
		processed_info = Info.parse(self.getInputParamValue('info'))

		core.utils.saveFile(seqfile+".map", out)
		self.setOutputParam('info', processed_info)
		self.output = string

		def verificationmode(cores):
			extrargs = []
			log = int(math.log(float(cores),2.0))

			varset = []

			# TODO change fn_name according to 'spliton'
			# fn_name = 'init'
			# splitonfull = 'c %s::1::%s!0@1#1 ' % (fn_name, spliton)
			splitonfull = 'c %s#2 ' % (spliton)

			if contexts == 1:
				cores = 1

			try:
				choice_bitwidth = (findpropositionalvarsize(splitonfull,lines).bind(self))/contexts
			except:
				self.error("DIMACS lookup failed for the given symbol %s" % spliton)

			# split on least significant digits of the symbolic variables that represent the context-switch points
			if cores >= 2: varset.append(findpropositionalvar(splitonfull,lines,0).bind(self))
			if cores >= 4: varset.append(findpropositionalvar(splitonfull,lines,1*choice_bitwidth).bind(self))
			if cores >= 8: varset.append(findpropositionalvar(splitonfull,lines,2*choice_bitwidth).bind(self))
			if cores >= 16: varset.append(findpropositionalvar(splitonfull,lines,3*choice_bitwidth).bind(self))

			if cores > 1:
				for k in range(0,cores): 
					extrarg = " --assume "

					boh = '%s' % get_bin(k,log)

					i=0
					for v in varset:					
						extrarg += "%s=%s%s" %(v, 0 if boh[i]=='0' else 1, ',' if i<len(varset)-1 else '')
						i+=1

					extrargs.append(extrarg)	

			#if 'warning' in err: self.warn('warnings on stderr from the backend')

			# to parallel feeder
			self.setOutputParam('extrargs', extrargs)

		if not simulate or int(simulate) == 0:
			verificationmode(cores)
		else:
			self.setOutputParam('simulate', int(simulate))

