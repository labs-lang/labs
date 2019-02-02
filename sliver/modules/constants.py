""" CSeq
	Concurrency-safe Constant Propagation module

	written by Omar Inverso.
"""
VERSION = 'constants-0.0-2018.10.28'  #
#VERSION = 'constants-0.0-2018.05.26'  #
#VERSION = 'constants-0.0-2017.07.21'  # started from scratch
#VERSION = 'constants-0.0-2015.11.07'  # CSeq-1.0-svcomp2016
#VERSION = 'constants-0.0-2014.12.24'  # CSeq-1.0beta
#VERSION = 'constants-0.0-2014.12.09'
##VERSION = 'constants-0.0-2014.10.26'    # CSeq-Lazy-0.6: newseq-0.6a, newseq-0.6c, SVCOMP15
###VERSION = 'constants-0.0-2014.10.15'
####VERSION = 'constants-0.0-2014.03.14' (CSeq-Lazy-0.4)
#####VERSION = 'constants-0.0-2014.02.25' (Cseq-Lazy-0.2)

"""
	Transformation 1 (binary operations, including nested expressions):
	e.g. 4 + 3*2  --->  10

	Transformation 2:
	Simple workaround for expressions that contains global (and thus potentially shared) variables

Limitations:
	- only works on integer constants
	- transformation 2 uses int for temporary variables
	- transformation 2 only considers binary operations on the RHS

TODO:
	(urgent) need full code review
	(urgent) need to move transformation 2 (non-atomicity of binary operations)
	         to the sequentialisation stage.

Changelog:
	2018.10.28  borrowed visit_binaryop from ICCPS18's basic constantfolding module
	2018.05.26  added translation for integer division (when possible) and multiplication
	2015.10.22  add fix for ldv-races category in SVCOMP16 (Truc)
	2014.12.09  further code refactory to match the new organisation of the CSeq framework
	2014.10.26  removed dead/commented-out/obsolete code
	2014.10.15  removed visit() and moved visit call-stack handling to module class (module.py)
	2014.03.14  further code refactory to match  module.Module  class interface
	2014.02.25  switched to  module.Module  base class for modules

"""
import pycparser.c_parser, pycparser.c_ast, pycparser.c_generator
import core.common, core.module, core.parser, core.utils
import re


class MyParser(pycparser.c_generator.CGenerator):
	def __init__(self):
		self.__considervar = ''
		self.__hasConsidervar = False

	def setConsidervar(self, string):
		self.__considervar = string

	def getHasConsidervar(self):
		return self.__hasConsidervar

	def setHasConsidervar(self, value):
		self.__hasConsidervar = value

	def visit(self, node):
		method = 'visit_' + node.__class__.__name__
		ret = getattr(self, method, self.generic_visit)(node)
		if ret == self.__considervar:
			self.__hasConsidervar = True
		return ret


class constants(core.module.Translator):
	__temp_var_no = -1
	__globalMemoryAccessed = False
	__currentFunction = ''
	__atomicSection = False

	# TODO: this needs proper implementation
	# Use another parser to avoid breaking line mapping system
	__myparser = MyParser()


	def visit_FuncDef(self, n):
		self.__currentFunction = n.decl.name
		decl = self.visit(n.decl)

		self.__atomicSection = False
		if n.decl.name.startswith("__CSEQ_atomic_"):
			self.__atomicSection = True

		self.indent_level = 0

		body = self.visit(n.body)

		self.__currentFunction = ''

		if n.param_decls:
			knrdecls = ';\n'.join(self.visit(p) for p in n.param_decls)
			return decl + '\n' + knrdecls + ';\n' + body + '\n'
		else:
			return decl + '\n' + body + '\n'


	def visit_Assignment(self, n):
		oldGlobalMemoryAccessed = self.__globalMemoryAccessed
		globalAccess = False
		self.__globalMemoryAccessed = False
		lval_str = self.visit(n.lvalue)

		self.__myparser.setConsidervar(lval_str)
		self.__myparser.setHasConsidervar(False)
		self.__myparser._parenthesize_if(n.rvalue, lambda n: isinstance(n, pycparser.c_ast.Assignment))

		rval_str = self._parenthesize_if(n.rvalue, lambda n: isinstance(n, pycparser.c_ast.Assignment))
		globalAccess = self.__globalMemoryAccessed

		ret = ''

		if (not self.__atomicSection and
				globalAccess and
				type(n.rvalue) == pycparser.c_ast.BinaryOp and
				self.__myparser.getHasConsidervar() and
				self.__getType(self.__currentFunction, lval_str) != "UNKNOWN"
			):
			# Declare a temporary variable for this statement
			self.__temp_var_no += 1
			ret = '; %s __cs_temporary_%s = 0; __cs_temporary_%s = %s; ' % (self.__getType(self.__currentFunction, lval_str), self.__temp_var_no, self.__temp_var_no, lval_str)
			newrval_str = " " + rval_str + " "
			newrval_str = newrval_str.replace(" %s " % lval_str, ' __cs_temporary_%s ' % self.__temp_var_no, 1).strip()
			ret += '%s %s %s' % (lval_str, n.op, newrval_str)
		else:
			ret = '%s %s %s' % (lval_str, n.op, rval_str)

		return ret


	def visit_ID(self, n):
		# If this ID corresponds either to a global variable,
		# or to a pointer...
		#
		if ((self.__isGlobal(self.__currentFunction, n.name) or self.__isPointer(self.__currentFunction, n.name)) and not n.name.startswith('__cs_thread_local_')):
			self.__globalMemoryAccessed = True
		return n.name


	def visit_FuncCall(self, n):
		fref = self._parenthesize_unless_simple(n.name)

		if fref == "__CSEQ_atomic_begin": self.__atomicSection = True
		elif fref == "__CSEQ_atomic_end": self.__atomicSection = False

		return fref + '(' + self.visit(n.args) + ')'


	def visit_BinaryOp(self, n):
		lval_str = self._parenthesize_if(n.left, lambda d: not self._is_simple_node(d))
		rval_str = self._parenthesize_if(n.right, lambda d: not self._is_simple_node(d))

		# remove brackets enclosing constants (e.g. (1234) -> 1234)
		if lval_str.startswith('(') and lval_str.endswith(')') and self._isInteger(lval_str[1:-1]):
			lval_str = lval_str[1:-1] 

		if rval_str.startswith('(') and rval_str.endswith(')') and self._isInteger(rval_str[1:-1]):
			rval_str = rval_str[1:-1] 

		if self._isInteger(lval_str) and self._isInteger(rval_str):
			if n.op == '-': return str(int(lval_str) - int(rval_str))
			if n.op == '+': return str(int(lval_str) + int(rval_str))
			if n.op == '*': return str(int(lval_str) * int(rval_str))
			if n.op == '/' and (int(lval_str) % int(rval_str) == 0): return str(int(lval_str) / int(rval_str))

		return '%s %s %s' % (lval_str, n.op, rval_str)


	def _isInteger(self, s):
		if s[0] in ('-', '+'): return s[1:].isdigit()
		else: return s.isdigit()

	# Checks whether variable  v  from function  f  is a pointer.
	#
	def __isPointer(self, f, v):
		if v in self.Parser.varNames[f] and self.Parser.varType[f,v].endswith('*'): return True
		elif v in self.Parser.varNames[''] and self.Parser.varType['',v].endswith('*'): return True
		else: return False

	# Checks whether variable  v  from function  f  is global.
	#
	def __isGlobal(self, f, v):
		if (v in self.Parser.varNames[''] and v not in self.Parser.varNames[f]): return True
		else: return False

	def __getType(self, f, v):
		if (v in self.Parser.varNames[f]):
			return self.Parser.varType[f, v]
		elif '.' in v:  # This variable is a field
		    # TODO: this needs proper implementation, quick hack for ldv-races
		    return 'int'
		else:
			return "UNKNOWN"
