class AlgebraicTree:

	def __init__(self, expression):
		self.raw_expression = expression
		self.parsed_op, t = self.parser([c for c in self.raw_expression], [], None)

	def parser(self, expr, acc, lastOp):
		if expr == []:
			if lastOp != None:
				acc = acc + [int(lastOp)]
			return acc, []
		head = expr.pop(0)
		if head == "+" or head == "-" or head == "*" or head == "/":
			if lastOp != None:
				acc = acc + [int(lastOp)]
			return self.parser(expr, (acc+[head]), None)
		elif head == "(":
			if lastOp != None:
				acc = acc + [int(lastOp)]
			parenthesis, rest = self.parser(expr, [], None)
			return self.parser(rest, acc+[parenthesis], None)
		elif head == ")":
			if lastOp != None:
				acc = acc + [int(lastOp)]
			return acc, expr
		else:
			if lastOp != None:
				head = lastOp + head
			return self.parser(expr, acc, head)
