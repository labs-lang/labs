""" CSeq C Sequentialization Framework
	parsing module

    written by Omar Inverso, University of Southampton,
    built on top of pycparser by Eli Bendersky (BSD license),
    which embeds PLY, by David M. Beazley.
"""
VERSION = 'parser-0.0-2018.05.26'
#VERSION = 'parser-0.0-2018.01.24'
#VERSION = 'parser-0.0-2015.06.26'
#VERSION = 'parser-0.0-2015.01.07'   
#VERSION = 'parser-0.0-2014.10.29'   # CSeq-1.0beta
#VERSION = 'parser-0.0-2014.10.29'   #newseq-0.6c, SVCOMP15
#VERSION = 'parser-0.0-2014.10.27'   newseq-0.6a
#VERSION = 'parser-0.0-2014.03.16'
#VERSION = 'parser-0.0-2014.03.09'
#VERSION = 'parser-0.0-2014.02.19'
"""
Generate symbol-table and a few other data structures.
(this module is used at the beginning of a Translator module
 for building basic data structs
 capturing information on the input code)

things to handle here:
	- typedef expansion
	- add extraction of any extra information about the code needed in later modules.

Prerequisites:
	- input must be preprocessed (i.e., no # directives)
	- no linemarkers (pycparser does not handle them)
	- no anonymous structs (use merger.py first)
	- no expressions such as A->b,
	  should be (*A).b instead (see workaround no. 4)

Changelog:
	2018.05.26  not longer storing the entire body of functions (self.funcBody)
	2018.05.26  added new data structs for extra reasoning, exp. useful for inlining (funcExitPointsCnt,lastStmtNode,lastFuncStmtNode)
	2018.01.24  disabled new symbol table bookkeeping (not needed for now)
	2018.01.24  removed funcBlock[] (inefficient and useless)
    2018.01.24  integrated changes (need to check them) from SVCOMP'16 version (see below):
    		--start merged changes to check--
    	2016.11.21  add safe check for node.coord before extracting linenumber
    	2016.08.16  add self.varInAssignment and self.varNoNeedInit to track local variables
			--end merged changes to check--
    2016.08.16  add self.varInAssignment and self.varNoNeedInit to track local variables
	2015.06.23  re-implemented 3rd parameter extraction to  pthread_create()  call (fName)
	2015.01.07  bugfix: calculating when variables are referenced and referenced not working with arrays
	2014.10.29  more information on threads (threadindex map)
	2014.10.27  improved symbol table about variables' details (exact line(s) where they are referenced, dereferenced, and where they occur)
	2014.03.16  amended the mechanism to calculate the list of functions (varNames)
	2014.03.16  introduced self.reset() for resetting all the data structs
	2014.03.09  anonymous structs no longer supported (they are assigned a name in merger.py)
	2014.03.04  symbol table: removed unused variables names in nested parameter declarations (e.g. parameters of a parameter, for example of a function)
	2014.02.25  bugfix: varNames
	2014.02.19  added  self.nodecoords[] to store the nodes' coords

"""

import pycparser.c_parser, pycparser.c_ast, pycparser.c_generator
import utils, common


class Parser(pycparser.c_generator.CGenerator):
	__sourcecode = ''
	__stack = []


	""" Uses the same visitor pattern as c_ast.NodeVisitor, but modified to
		return a value from each visit method, using string accumulation in 
		generic_visit.
	"""
	def __init__(self):
		self.reset()


	def reset(self):
		###### NEW PARSING TABLE START #####
		#self.symbols = []              # _all_ the symbols (function names, variables, struct names, .... ) TODO this needs to be checked & finished properly
		#self.blocks = {}
		#self.__symbolsstack = []
		#self.__symbolscount = 0
		#self.blocks.append('0')
		##self.symbolsparents[n.name]# the symbol's full parents stack 
		##self.__symbolsblock[n.name] = # the symbol's full block/compound stack (e.g. 0:1:20:2 - block 0: is the global scope)
		###### NEW PARSING TABLE END #####

		self.currentFunct = ''         # name of the function being parsed ('' = none)
		self.visitingField = False     # needed to differentiate between variables and fields during the visit

		self.funcName = ['']           # all functions names (consider '' to be a special function to model global scope)
		self.funcParams = {}
		self.threadName = []           # all threads names (i.e. functions used as argument to pthread_create())
		self.funcReferenced = []       # all functions whose id are used in anything else than just function calls (e.g. they are used as pointer-to-functions..)
		self.funcCallCnt = {}          # number of function calls for each function name
		self.threadCallCnt = {}        # number of times a function is used to generate a thread (by calling pthread_create())

		self.threadCount = 0           # pthread create()s found so far
		self.threadIndex = {}          # index of the thread = value of threadcount when the pthread_create to that thread was discovered

		self.lastStmt = ''             # last statement generated (as a string)
		self.lastStmtNode = ''         # last statement generated (as an AST node)
		self.lastFuncStmt = {}         # last statement for each function
		self.lastFuncStmtNode = {}     # last statement for each function (as an AST node)

		###self.funcBlock = {}            # the entire function definition (= declaration+body) for each function
		self.funcBlockIn = {}          # input parameters for each function
		self.funcBlockOut = {}         # return value for each function 
		###self.funcBody = {}             # function body-only
		self.funcDecl = {}             # function declarations, only for functions declared and defined in different statements, or not defined at all.
		self.funcASTNode = {}          # the AST node for the function definitions, by function
		self.funcIsVoid = {}
		self.funcExitPointsCnt = {}    # Number of return statements in the function
		self.funcLabels = {}           # set of labels defined in a given function, by function

		self.currentVarAssign = ''     # name of the current variable used as lvalue in an assignment statement

		self.currentStruct = ''        # note! this 
		self.structName = []

		self.mainParametersDecl = ''     # parameters of the main() function to be transferred to thread 0.


		""" The following are indexed either using
			(struct, variable)  for struct fields, or
			(function, variable) local variables and function parameters, or
			('', variable) for global variables.	

			See _generate_decl(self, n) below for details.

			TODO: to make the parsing more robust,
			      the stack of blocks enclosing a variable's scope
			      should rather be used for indexing (each block should have a unique ID)  
		"""
		self.varNames = {}           # names of all the variables (global, local, parameters) + variables in structs
		self.varNames[''] = []       # initialisation for global var names ('' is the global scope)

		self.varType = {}            # int, char, ....
		self.varTypeUnExpanded = {}  # same as .varType, but with unexpanded typedefs
		self.varArity = {}           # 0 for scalar, k for k-dimensional arrays
		self.varSize = {}            # [] for scalars, [n1,...,k] for k-dimensional arrays
		self.varKind = {}            # g, l, p, s
		self.varMallocd = {}         # used only for pointers
		self.varID = {}              # unique IDs for variables, as they are found while parsing, starting with 0
		self.varCount = 0            #
		self.extraGlovalVarCount = 0 # Count global variables introduced in the above data structures, but not from original input. 
		self.varInitExpr = {}        # set when a declared variable has an init expression
		self.varPtrToFunct = {}

		# patch for handling pointer-to-function etc. (TODO move with the new symbol table)
		#self.__varIsPointer = {} 
		#self.__varIsArray = {} 
		#self.__varIsFunction = {} 

		# list of line no. where an occurrence, referencing, or dereferencing  happened
		self.varOccurrence = {}      # any occurrence (does not include the very first time a variable occurs, i.e. on the left hand side of its own declaration)
		self.varReferenced = {}      # &var
		self.varDeReferenced = {}    # *var

		# Handling of typedefs.
		# We put in the first variable below the last part of a typedef statement, 
		# and in the second variable its correspondent expansion.
		#
		# Anonymous typedefs are no exception, as they are assigned an internal name to be used as described.
		#
		self.typedefs = []          # contains last the part of a typedef stmt, e.g. typedef struct struct_name {.... } last_part;
		self.typedefExpansion = {}  # e.g. typedefs['field'] = struct { int a; int b; ... }  from the original typedef statement
	
		# Statements start with indentation of self.indent_level spaces, using
		# the _make_indent method
		#
		self.indent_level = 0		# to keep track of the depth of the block {}
		self.INDENT_SPACING = '   '	# ....

		# = True while parsing the subtree for a function declaration
		self.parsingFuncDecl = False

		# = True while parsing the subtree for a struct (or union) declaration
		self.parsingStruct = False

		# this will set to True after parsing the first function definition (not declaration!)
		self.firstFunctionDefinitionDone = False

		# set to True while parsing void functions
		self.parsingVoidFunction = False

		# set to True while parsing typedef blocks 
		self.parsingTypedef = False

		#
		self.currentInputCoord = ''
		self.currentInputLineNumber = -1
		self.currentOutputLineNumber = -1

		# coords for each node in the AST
		# (when the input is loaded from a string rather than a file,
		#  the coords only contain the line number)
		self.nodecoords = {} 

		self.lines = []

		self.varInAssignment = {}
		self.varNoNeedInit = {}


	def getversion(self):
		return PARSER_VERSION


	'''
	def load(self, filename):
		self.__inputfilename = filename
		# Load the input source file to build the AST, then generate the symbol table
		self.ast = pycparser.parse_file(self.__inputfilename, use_cpp=True, cpp_args=r'-Iinclude -E -C ')
		self.__sourcecode = self.visit(self.ast)
		self.ast.show()
	'''


	def loadfromstring(self, string):
		#self.ast = pycparser.parse_file(self.__inputfilename, use_cpp=True, cpp_args=r'-Iinclude -E -C ')
		self.ast = pycparser.c_parser.CParser().parse(string)
		self.__sourcecode = self.visit(self.ast)		
		###self.ast.show()

		'''
		from pycparserext.ext_c_parser import GnuCParser
		p = GnuCParser()
		self.ast = p.parse(string)
		#self.ast.show()
		'''


	def show(self):
		#print utils.strip(self.__sourcecode)
		print self.__sourcecode


	def save(self, filename):
		outfile = open(filename,"w")
		outfile.write(self.__sourcecode)
		outfile.close()


	def string(self):
		return(self.visit(self.ast))


	def printsymbols(self):
		print "list of functions:"

		for f in self.funcName:
			if f == '': continue
			s = "   %s" % f
			if f in self.funcReferenced: s += '   referenced'
			print s,

			if f != '':
				print "  call count %s" % self.funcCallCnt[f]
			else:
				print ''

		print ''

		print "list of thread functions:"

		for f in self.threadName:
			print "   %s  call count %s" % (f, self.threadCallCnt[f])

		print ''

		print "parameters for main():\n   "+ '(no params)' if not self.mainParametersDecl else self.mainParametersDecl
		print ''
       
		# List of all variables
		print "Variables:"
		for f in self.funcName:
			if not f == '':	print "   " + f
			else: print "   (global)"
			
			for v in self.varNames[f]:
				# detailed var description
				s = "      "
				s += "id" + str(self.varID[f,v]) + "  "
				s += "'" + str(v) + "'  "
				s += "\n         "				
				s += "type '" + self.varType[f, v] + "'  "
				s += "kind '" + self.varKind[f, v] + "'  "
				s += "arity '" + str(self.varArity[f, v]) + "'  "
				s += "\n         "				
				s += "size '" + str(self.varSize[f, v]) + "'  "
				#s += "mallocd '" + str(self.varMallocd[f,v]) + "'  "
				s += "\n         "				
				s += "ref '" + str(self.varReferenced[f, v]) + "'  "
				s += "\n         "				
				s += "deref '" + str(self.varDeReferenced[f, v]) + "'  "
				s += "\n         "				
				s += "occurs '" + str(self.varOccurrence[f, v]) + "'  "

				if (f,v) in self.varPtrToFunct: s += "ptr-to-f '" + str(self.varPtrToFunct[f,v]) + "'"
				
				print s

		print '\n'

		# List of all fields
		print "Fields:"
		for f in self.structName:
			if not f == '':	print "   " + str(f)
			else: print "   (global)"
			
			for v in self.varNames[f]:
				# detailed var description
				s = "      "
				s += "id" + str(self.varID[f,v]) + "  "
				s += "'" + v + "'  "
				s += "type '" + self.varType[f, v] + "'  "
				s += "kind '" + self.varKind[f, v] + "'  "
				s += "arity '" + str(self.varArity[f, v]) + "'  "
				s += "size '" + str(self.varSize[f, v]) + "'  "
				#s += "mallocd '" + str(self.varMallocd[f,v]) + "'"
				
				print s

		print '\n'

		print "Typedefs:"
		for x in self.typedefs:
			print x + " -> " + self.typedefExpansion[x]

		print '\n'

		print "Pointer variables:"
		for f in self.funcName:
			if not f == '':	print "   " + f
			else: print "   (global)"
			
			for v in self.varNames[f]:
				if self.varType[f,v].endswith('*'):
					s = "       "
					s += "var '" + v + "'   "
					s += "type '" + self.varType[f, v] + "'   "
					s += "kind '" + self.varKind[f, v] + "'   "
					s += "arity '" + str(self.varArity[f, v]) + "'   "
					s += "size '" + str(self.varSize[f, v]) + "'   "
				
					print s

		print '\n'

		'''
		print "Function blocks:"
		for f in self.funcName:
			if f != '':
				print "function '%s' ----------------------------------:" % f
				print self.funcBlock[f] + '\n'
				print self.funcBlockIn[f] +'\n'
				print self.funcBlockOut[f] +'\n'

		print "Last statement, by function:"
		for f in self.funcName:
			if f != '':
				print "function: %s   stmt: %s" % (f, self.lastFuncStmt[f])

		print '\n'
		'''

		print "All symbols (new symbol table - work in progress):"
		for s in self.symbols:
			##print "   %s    [%s]" % (s, self.symbolshierarchy[s, blockstack])
			print "   %s  " % str(s)


	def shownodes(self):
		from optparse import OptionParser
		import inspect
		print [entry for entry in dir(pycparser.c_generator.CGenerator) if not entry[0].startswith('_')]


	def _make_indent(self, delta=0):
		return (self.indent_level+delta) * self.INDENT_SPACING


	def _getCurrentCoords(self, item):
		### return '/*cseq:coords '+ str(item.coord) +' */\n'
		return ''


	def visit(self, node):
		method = 'visit_' + node.__class__.__name__

		# Extracts node coords where possible.
		if hasattr(node, 'coord'):
			self.nodecoords[node] = str(node.coord)
			#########print "-> attr %s [%s]\n" % (self.nodecoords[node], str(node))
		else:
			self.nodecoords[node] = None
		
		# This is to update the current coord (= filename+line number) of the input file being parsed,
		# considering that:
		#
		# - on the same line of input, there may be more AST nodes (shouldn't enter duplicates)
		# - compo\d statement and empty statements have line number 0 (shouldn't update the current line)
		# - the same line of input may correspond to many lines of output
		#
		lineCoords = ''

		####if not isinstance(node, pycparser.c_ast.FileAST) and str(node) != 'None' and self.indent_level == 0:
		if not isinstance(node, pycparser.c_ast.FileAST) and str(node) != 'None' and self.indent_level == 0 and node.coord is not None:
			##print "       VISITING %s - %s" % (str(node), node.coord)

			linenumber = str(node.coord)
			linenumber = linenumber[linenumber.rfind(':')+1:]

			self.currentInputLineNumber = int(linenumber)

			# Each line of the output is annotated when 
			# either it is coming from a new input line number
			# or the input line has generated many output lines, 
			# in which case the annotation needs to be repeated at each line..
			#
			if linenumber not in self.lines and linenumber != '0':
				### print "      adding line number %s" % linenumber
				self.lines.append(linenumber)
				lineCoords = self._getCurrentCoords(node)

		self.__stack.append(node.__class__.__name__)
		retval = lineCoords + getattr(self, method, self.generic_visit)(node) 
		
		##print str(self.__stack) + '   prev:' + str(self.__stack[len(self.__stack)-2])
		self.__stack.pop()

		return retval


	def generic_visit(self, node):
		#~ print('generic:', type(node))
		if node is None:
			return ''
		else:
			return ''.join(self.visit(c) for c in node.children())


	def visit_Label(self, n):
		self.funcLabels[self.currentFunct].append(n.name)

		return n.name + ':\n' + self._generate_stmt(n.stmt)


	def visit_FuncCall(self, n):
		fref = self._parenthesize_unless_simple(n.name)
		args = self.visit(n.args)

		# When a thread is created, extract its function name
		# based on the 3rd parameter in the pthread_create() call:
		#
		# pthread_create(&id, NULL, f, &arg);
		#                          ^^^
		#
		if fref == 'pthread_create' or fref == common.changeID['pthread_create']:   # may be used after thread_* renaming
			fName = self.visit(n.args.exprs[2])
			fName = fName[1:] if fName.startswith('&') else fName

			if fName not in self.threadCallCnt:
				self.threadName.append(fName)
				self.threadCallCnt[fName] = 1;
				self.threadCount = self.threadCount + 1
				self.threadIndex[fName] = self.threadCount
			else:
				self.threadCallCnt[fName] += 1;

		# Counts function calls
		if fref not in self.funcCallCnt: self.funcCallCnt[fref] = 0;
		else: self.funcCallCnt[fref] += 1;

		return fref + '(' + args + ')'


	def visit_Typedef(self, n):
		s = ''
		if n.storage: s += ' '.join(n.storage) + ' '

		self.parsingTypedef = True
		typestring = self._generate_type(n.type)
		self.parsingTypedef = False

		# Typical  typedef struct  statement...
		# TODO this should be reimplemented using AST visits.
		if  typestring.startswith("struct "):
			## print "     adding typedef name '" +s+ "'"
			## print "     adding typedef type '" +typestring+ "'"
			leftPart = typestring[:typestring.find('{')]
			if leftPart.endswith(' '): leftPart = leftPart[:-1]
			if leftPart.endswith('\n'): leftPart = leftPart[:-1]
			if leftPart.endswith(' '): leftPart = leftPart[:-1]
			rightPart = typestring[typestring.rfind('} ')+2:]
			## print "      LEFT: '" + leftPart + "'"
			## print "      RIGHT: '" + rightPart + "'"
			self.typedefs.append(rightPart)
			self.typedefExpansion[rightPart] = leftPart

		s += typestring
		return s


	# Note: function definition = function declaration + body.
	# This method is not called when parsing simple declarations of function (i.e., function prototypes).
	#
	def visit_FuncDef(self, n):
		##self.funcDefined.append(n.decl.name)
		if n.decl.name not in self.funcCallCnt: self.funcCallCnt[n.decl.name] = 0;

		self.currentFunct = n.decl.name

		if n.decl.name not in self.funcName:
			self.funcName.append(n.decl.name)
			self.funcParams[self.currentFunct] = []
			self.varNames[self.currentFunct] = []
			self.funcLabels[self.currentFunct] = []
			self.varInAssignment[self.currentFunct] = {}
			self.varNoNeedInit[self.currentFunct] = {}
			self.funcExitPointsCnt[self.currentFunct] = 0

		self.funcASTNode[n.decl.name] = n

		# Note: the function definition is in two parts:
		#       one is 'decl' and the other is 'body'
		decl = self.visit(n.decl)

		if decl.startswith("void") and not decl.startswith("void *"):
			self.parsingVoidFunction = True
			self.funcIsVoid[self.currentFunct] = True
		else:
			self.parsingVoidFunction = False
			self.funcIsVoid[self.currentFunct] = False

		body = self.visit(n.body)
		funcBlock = decl + '\n' + body + '\n'

		# Store all the function block
		#self.funcBlock[self.currentFunct] = funcBlock
		#self.funcBody[self.currentFunct] = body

		# Store the return type of the function
		returnType = decl[:decl.find(self.currentFunct+'(')]
		returnType = returnType[:-1] if returnType.endswith(' ') else returnType
		self.funcBlockOut[self.currentFunct] = returnType

		# Store the function input parameter list
		self.funcBlockIn[self.currentFunct] = decl[decl.find(self.currentFunct+'(')+len(self.currentFunct)+1:decl.rfind(')')]

		self.lastFuncStmt[self.currentFunct] = self.lastStmt
		self.lastFuncStmtNode[self.currentFunct] = self.lastStmtNode

		#print "VISITING  %s  LAST STMT %s\n\n" % (self.currentFunct, self.lastStmt)
		self.currentFunct = ''

		return funcBlock


	def visit_Compound(self, n):
		s = self._make_indent() + '{\n'
		self.indent_level += 1
		##if n.block_items:
		##	s += ''.join(self._generate_stmt(stmt) for stmt in n.block_items)

		if n.block_items:
			for stmt in n.block_items:
				newStmt = self._getCurrentCoords(stmt) + self._generate_stmt(stmt)
				s += newStmt
				self.lastStmt = newStmt
				self.lastStmtNode = stmt

				##### s += self._generate_stmt(stmt)

		self.indent_level -= 1
		s += self._make_indent() + '}\n'
		
		return s


	def visit_ID(self, n):
		#####print "VISITING ID:%s   STACK:<%s>   COORDS:%s   FIELD?:%s" % (n.name, str(self.__stack), self.nodecoords[n], self.visitingField )

		if not self.visitingField:
			if self.nodecoords[n]:
				if n.name in self.varNames[self.currentFunct]:
					self.varOccurrence[self.currentFunct,n.name].append(int(self.nodecoords[n][1:]))
				elif n.name in self.varNames['']:
					self.varOccurrence['',n.name].append(int(self.nodecoords[n][1:]))

		# Detecting pointer-to-function references (i.e.: when a function name is used for anything else than a function call)
		#
		if n.name in self.funcName:
			
			prev = str(self.__stack[len(self.__stack)-2])
			#print "visiting function : %s (prev:%s)" % (n.name, prev)
			#print str(self.__stack) + '   prev:' + str(self.__stack[len(self.__stack)-2])

			##if prev != 'FuncCall':
			if 'FuncCall' not in self.__stack:
				self.funcReferenced.append(n.name)
				# TODO: inline function from pointer (n.name)

		if (self.currentFunct != '' and n.name in self.varInAssignment[self.currentFunct]):
			self.varInAssignment[self.currentFunct][n.name] += 1

		return n.name


	def visit_Case(self, n):
		s = 'case ' + self.visit(n.expr) + ':\n'
		self.indent_level += 2
		for stmt in n.stmts:
			s += self._generate_stmt(stmt)
		self.indent_level -= 2
		return s


	def visit_Default(self, n):
		s = 'default:\n'
		self.indent_level += 2
		for stmt in n.stmts:
			s += self._generate_stmt(stmt)
		self.indent_level -= 2
		return s


	def visit_Decl(self, n, no_type=False):
		#if n.name not in self.symbols and n.name is not None:
		if n.name is not None:
			# new symbol table (TODO)
			#self.__symbolsstack.append(n.name)
			#self.symbols.append( (self.__symbolscount,n.name) )
			#self.__symbolscount += 1
			##self.symbolsparents[n.name] = self....  # the symbol's full parents stack 
			##self.symbolsblock[n.name] = self.....   # the symbol's full block/compound stack (e.g. 0:1:20:2 - block 0: is the global scope)
			#### self.symbolshierarchy[n.name, <currentblock/currentscope>]
			'''
			print "NEW SYMBOL"
			print "     %s" % n.name
			print "    [%s]" % self.__symbolsstack[-1]
			print "    [%s]" % self.__symbolsstack
			print "    is a pointer? %s" % ('c_ast.PtrDecl' in str(n.type))
			print "    is a pointer? %s" % (n.children())
			'''
			#### generate_type(n.type)

		# no_type is used when a Decl is part of a DeclList, where the type is
		# explicitly only for the first delaration in a list.

		#
		s = n.name if no_type else self._generate_decl(n)

		if self.currentFunct != '':   # Inside a function
			self.varInAssignment[self.currentFunct][n.name] = 0   # Just a declaration

		if n.bitsize: s += ' : ' + self.visit(n.bitsize)

		if n.init:
			if isinstance(n.init, pycparser.c_ast.InitList):
				s += ' = {' + self.visit(n.init) + '}'
			elif isinstance(n.init, pycparser.c_ast.ExprList):
				s += ' = (' + self.visit(n.init) + ')'
			else:
				s += ' = ' + self.visit(n.init)

		if 'FuncDecl' in str(n.type) and self.currentFunct == '':
			#print "function %s   decl %s   currentFunct %s\n\n" % (n.name, s, self.currentFunct)
			self.funcDecl[n.name] = s

		#if n.name is not None and len(self.__symbolsstack) > 0:
		#	self.__symbolsstack.pop()

		return s


	def visit_Struct(self, n):
		'''
		# Assign a name to anonymous structs
		if n.name == None:
			n.name = '__CS_anonstruct_' + str(self.currentAnonStructsCount)
			self.currentAnonStructsCount += 1
		'''

		# This method is called more than once on the same struct,
		# the following is done only on the first time.
		#
		if n.name not in self.structName:
			self.currentStruct = n.name
			self.structName.append(n.name)
			self.varNames[self.currentStruct] = []

		oldParsingStruct = self.parsingStruct
		self.parsingStruct = True
		s = self._generate_struct_union(n, 'struct')
		self.parsingStruct = oldParsingStruct

		# Parsing  typedef struct
		'''
		if self.parsingTypedef: 
			print "    PARSING TYPEDEF STRUCT!!!!!!"
			print "       s: " + s 
			print "    name: " + str(n.name) + '\n\n\n'
		'''

		return s


	def visit_StructRef(self, n):
		sref = self._parenthesize_unless_simple(n.name)

		oldVisitingField = self.visitingField
		self.visitingField = True

		field = self.visit(n.field)

		self.visitingField = oldVisitingField

		return sref + n.type + field


	def visit_UnaryOp(self, n):
		operand = self._parenthesize_unless_simple(n.expr)
		oper = operand[:operand.find('[')] if '[' in operand else operand # could be an array: remove indexes

		if n.op == 'p++': return '%s++' % operand
		elif n.op == 'p--': return '%s--' % operand
		elif n.op == 'sizeof': return 'sizeof(%s)' % self.visit(n.expr)
		elif n.op == '*':
			#print "DEREFERENCING %s (line:%s)" % (operand, self.nodecoords[n]);
			if self.nodecoords[n]:
				if oper in self.varNames[self.currentFunct]:
					self.varDeReferenced[self.currentFunct,oper].append(int(self.nodecoords[n][1:]))
				elif oper in self.varNames['']:
					self.varDeReferenced['',oper].append(int(self.nodecoords[n][1:]))

			return '%s%s' % (n.op, operand)
		elif n.op == '&':
			#print "REFERENCING %s / %s (line:%s)" % (operand, oper, self.nodecoords[n]);
			if self.nodecoords[n]:
				if oper in self.varNames[self.currentFunct]:  # local variable
					self.varReferenced[self.currentFunct,oper].append(int(self.nodecoords[n][1:]))
				elif oper in self.varNames['']:               # global variable
					self.varReferenced['',oper].append(int(self.nodecoords[n][1:]))

			return '%s%s' % (n.op, operand)
		else: return '%s%s' % (n.op, operand)


	def visit_Union(self, n):
		oldParsingStruct = self.parsingStruct
		self.parsingStruct = True
		s = self._generate_struct_union(n, 'union')
		self.parsingStruct = oldParsingStruct
		
		return s


	def visit_Assignment(self, n):
		rval_str = self._parenthesize_if(n.rvalue, lambda n: isinstance(n, pycparser.c_ast.Assignment))
		lval_str = self.visit(n.lvalue)

		if (self.currentFunct != '' and lval_str in self.varInAssignment[self.currentFunct] and
				self.varInAssignment[self.currentFunct][lval_str] == 1):
			if self.currentFunct in self.varNoNeedInit:
				self.varNoNeedInit[self.currentFunct][lval_str] = True

		return '%s %s %s' % (lval_str, n.op, rval_str)


	def visit_Return(self,n):
		self.funcExitPointsCnt[self.currentFunct] += 1

		s = 'return'
		if n.expr: s += ' ' + self.visit(n.expr)

		return s + ';'


	def _generate_decl(self, n):
		#print "DECL %s, %s, %s parsingstruct:%s" % (n.name, self.currentFunct, self.currentStruct, self.parsingStruct)

		""" Generation from a Decl node.
		"""
		s = ''
		
		# use flags to keep track through recursive calls of what is being parsed (START)
		if 'FuncDecl' in str(n.type):
			oldParsingFuncDecl = self.parsingFuncDecl
			self.parsingFuncDecl = True
			###self.varNames[self.currentFunct] = []

		if 'Struct' in str(n.type): self.parsingStruct = True

		if n.funcspec: s = ' '.join(n.funcspec) + ' '
		if n.storage: s += ' '.join(n.storage) + ' '
	
		s += self._generate_type(n.type)

		# use flags to keep track through recursive calls of what is being parsed (END)
		if 'FuncDecl' in str(n.type):
			### self.parsingFuncDecl = False
			self.parsingFuncDecl = oldParsingFuncDecl

		if 'Struct' in str(n.type): self.parsingStruct = False


		""" Handling of struct declarations.
		"""
		if 'Struct' in str(n.type):
			# new structure declaration
			1
			if self.parsingStruct:
				# new field in a structure
				1

		""" Handling of variable declarations.

			Each variable has the following info associated with it:

				name, type, kind, arity, size
	
			name
		    	varName[''] = list of global variables
		    	varName['x'] = list local variables in function or struct 'x'
				              (incl. input params for functions)

			type
				the type as declared in the original code (e.g. 'unsigned int', 'char *', ...)

			kind
				'g'  for global variables
				'l'  for local variables
				'p'  for function input parameters
				'f'  for struct fields

			arity
				0  for scalar variables
				k  for for k-dimensional arrays

			size
				[] for scalar variables
				[size1, ..., sizek] for k-dimensional arrays

		"""
		# Whatever is not a function or a struct here, is a variable declaration
		# TODO: see what else should be excluded to only have var declarations after the if
		#
		if 'FuncDecl' not in str(n.type) and 'Struct' not in str(n.type) and 'Union' not in str(n.type):
			# Variable name (for variables) or field name (for structs fields).
			#
			# At this point,
			# any variable declaration can occur (a) in the global scope,
			# or (b) in a function or (c) in a struct.
			#
			# Each of those will have a name, apart from the global scope,
			# for which name we use the empty string. We call it Context. 
			#
			# For example,
			#   if a variable V is declared in a function F, its Context is F.
			#   if a variable V is declared in a struct S, its Context is S.
			#   if a variable V is declared globally, its Context is ''.
			#
			# We use the context to index the arrays:
			# (1) to index the array name
			# (2) to index the arrays type, kind, arity, size,
			# in this case with the tuple (context, variable)
			#
			if not self.parsingStruct: variableContext = self.currentFunct
			else: variableContext = self.currentStruct

			#self.varPtrToFunct[variableContext,n.name] = None
			##print "appending var  [%s]\n          from context [%s]\n          type [%s]\n          string [%s]\n\n\n" % (n.name, variableContext, n.type, s)  

			if (not n.name in self.varNames[variableContext] and    # avoid duplicates
				self.__stack.count('ParamList') <= 1):              # do not consider nested declarations (example: int f1(int (f2(int a, int b)))
				### print "***** appending var  [%s]\n          from context [%s]\n          type [%s]\n          string [%s]" % (n.name, variableContext, n.type, s)  
				### print "***** " + str(self.__stack) + '   prev:' + str(self.__stack[len(self.__stack)-2]) 
				### print "***** PARAMLIST COUNT " + str(self.__stack.count('ParamList')) + '\n\n'

				# new variable or field discovered
				self.varNames[variableContext].append(n.name)       # 
				self.varID[variableContext,n.name] = self.varCount  # associate each new variable with a unique IDs
				self.varReferenced[variableContext,n.name] = []
				self.varDeReferenced[variableContext,n.name] = []
				self.varOccurrence[variableContext,n.name] = []

				if n.init: self.varInitExpr[variableContext,n.name] = True
				else: self.varInitExpr[variableContext,n.name] = False

				self.varCount += 1

			# Variable or field type
			if s.count('[') > 0: s2 = s[ :s.find('[')]		
			else: s2 = s

			if n.name:
				if s2.endswith(n.name): s2 = s2[:-len(n.name)]
				else: s2 = s2.replace(n.name, '')
			if s2.endswith(' '): s2 = s2[:-1]

			# Typedef expansion:
			# when some type of variable is found for which there is an entry in the list of typedefs,
			# expands directly in the symbol table the corresponding string.
			#
			self.varTypeUnExpanded[variableContext,n.name] = s2

			for x in self.typedefs:
				## print "found %s ---> %s" % (x, self.typedefExpansion[x])

				if s2 == x: s2 = self.typedefExpansion[x]
				if s2.startswith(x+ ' '): s2 = s2.replace(x+' ', self.typedefExpansion[x]+' ', 1)

				s2 = s2.replace(' '+x+' ', ' '+self.typedefExpansion[x]+' ')

			s2 = s2.replace('\n', '')
			s2 = s2.replace('\t', ' ')
			self.varType[variableContext,n.name] = s2.rstrip()
			### self.varType[variableContext,n.name] = n.type
			###print "variable added %s %s" % (n.name, n.type)
			###print str(self.__stack) + '   prev:' + str(self.__stack[len(self.__stack)-2]) + '\n\n'

			# Variable kind
			if self.parsingFuncDecl:    # parameter (from a function declaration)					
				self.varKind[variableContext, n.name] = 'p'
				if variableContext != '': self.funcParams[variableContext].append(n.name)

				## print "FOUND MAIN PARAMETER %s, DECL %s\n\n" % (n.name, s)
				if variableContext == 'main':
					varDecl = s;
					varDecl = varDecl.replace(' '+n.name, ' __CS_main_arg_'+n.name)
					varDecl = varDecl.replace(' *'+n.name, ' *__CS_main_arg_'+n.name)
					varDecl = varDecl.replace(' *__CS_main_arg_'+n.name+'[]', ' **__CS_main_arg_'+n.name)

					self.mainParametersDecl += '\t' + varDecl + ';\n'   ### TODO

			elif self.parsingStruct:    # field in a struct (this are not really variables, but we handle them using the same data structures)
				self.varKind[variableContext, n.name] = 'f'
			elif self.indent_level < 1:  # global variable
				self.varKind[variableContext, n.name] = 'g'
			else:                       # local variable
				self.varKind[variableContext, n.name] = 'l'

			# Variable arity (scalars have arity 0)
			self.varArity[variableContext,n.name] = int(s.count('['))   # TODO this needs to be properly calculated

			# Variable size(s) (for scalars that is an empty array)
			self.varSize[variableContext,n.name] = []

			tmp_s = s
			i = self.varArity[variableContext,n.name]

			while i > 0:
				tmp = tmp_s[tmp_s.find("[")+1:tmp_s.find("]")]
				if tmp == '': ithSize = -1              # Unbounded array. This is equivalent to declare a pointer. TODO
				###elif not tmp.isdigit(): ithSize = -2    # Array size given by a complex expression. This is like dynamically allocated blocks. TODO
				elif not tmp.isdigit(): ithSize = tmp   # Array size given by a complex expression. This is like dynamically allocated blocks. TODO
				else: ithSize = int(tmp)                # Array size given by a constant expression

				self.varSize[variableContext,n.name].append(ithSize)
				tmp_s = tmp_s[tmp_s.find("]")+1:]
				i = i-1

			# This is used later on to see if a pointer is ever associated with a malloc()
			self.varMallocd[variableContext,n.name] = False
	
		return s


	def _generate_type(self, n, modifiers=[]):
		""" Recursive generation from a type node. n is the type node.
			modifiers collects the PtrDecl, ArrayDecl and FuncDecl modifiers
			encountered on the way down to a TypeDecl, to allow proper
			generation from it.
		"""
		typ = type(n)
		#~ print(n, modifiers)

		retstr = ''

		'''
		if hasattr(n, 'name'):
			print "GENERATING TYPE FOR %s,..." % n.name
		else:
			print "GENERATING TYPE....(%s)" % n
		'''

		if typ == pycparser.c_ast.TypeDecl:
			s = ''
			if n.quals: s += ' '.join(n.quals) + ' '
			s += self.visit(n.type)

			nstr = n.declname if n.declname else ''
			# Resolve modifiers.
			# Wrap in parens to distinguish pointer to array and pointer to
			# function syntax.
			#
			for i, modifier in enumerate(modifiers):
				if isinstance(modifier, pycparser.c_ast.ArrayDecl):
					if (i != 0 and isinstance(modifiers[i - 1], pycparser.c_ast.PtrDecl)):                   # array-of-pointers
						### print "   CASE I ARRAY-OF-POINTERS[%s]" % self.__symbolsstack[-1]
						nstr = '(' + nstr + ')'
					nstr += '[' + self.visit(modifier.dim) + ']'
				elif isinstance(modifier, pycparser.c_ast.FuncDecl):
					if (i != 0 and isinstance(modifiers[i - 1], pycparser.c_ast.PtrDecl)):
						### print "   CASE II: POINTER-TO-FUNCTION DECLARATION [%s]" % self.__symbolsstack[-1]   # pointer-to-function
						nstr = '(' + nstr + ')'

						if not self.parsingTypedef:
							if not self.parsingStruct: variableContext = self.currentFunct
							else: variableContext = self.currentStruct

							## print "          [%s,%s]" % (variableContext,self.__symbolsstack[-1])
							#self.varPtrToFunct[variableContext,self.__symbolsstack[-1]] = True

					nstr += '(' + self.visit(modifier.args) + ')'


				elif isinstance(modifier, pycparser.c_ast.PtrDecl):
					### print "   CASE III: POINTER DECLARATION [%s]" % self.__symbolsstack[-1]                  # pointer
					if modifier.quals:
						nstr = '* %s %s' % (' '.join(modifier.quals), nstr)
					else:
						nstr = '*' + nstr
			if nstr: s += ' ' + nstr

			retstr = s

			# When generating nested parameter declarations,
			# strip the names of the parameters (e.g. names of parameters in a pointer to function).
			#
			if self.__stack.count('ParamList') > 1 and n.declname:
				retstr = retstr[:retstr.rfind(str(n.declname))-1]
				if retstr[-1] == ' ': retstr = retstr[:-1]

		elif typ == pycparser.c_ast.Decl:
			retstr = self._generate_decl(n.type)
		elif typ == pycparser.c_ast.Typename:
			retstr = self._generate_type(n.type)
		elif typ == pycparser.c_ast.IdentifierType:
			retstr = ' '.join(n.names) + ' '
		elif typ in (pycparser.c_ast.ArrayDecl, pycparser.c_ast.PtrDecl, pycparser.c_ast.FuncDecl):
			retstr = self._generate_type(n.type, modifiers + [n])
		else:
			retstr = self.visit(n)

		return retstr


	

