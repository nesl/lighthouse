import os
import re
import sys

verbose = False

class FunctionNode:
    "Node in a callgraph for a function"

    def __init__(self, func_file, func_name):
        "Create node for func_name located in func_file"
        self.func_file = func_file
        self.func_name = func_name
        self.called_funcs = []
        self.proto_string = ""

    def add_callee(self, callee):
        "Add node for callee to list of functions called"
        self.called_funcs.append(callee)

    def get_callees(self):
        "Return node list of called functions"
        return self.called_funcs

    def get_name(self):
        "Return human readable name of function"
        return self.func_file + ":" + self.func_name

    def __repr__(self):
        if verbose:
            return self.get_name() + " (" + self.proto_string + ")"\
                    " " + str(self.called_funcs)
        else:
            return self.get_name() + " (" + self.proto_string + ")"


class CallGraph:
    "Abstraction of the call graph of a program"

    def __init__(self, filename=""):
        self.nodes = {}
        self.loop_check = []
        if filename != "":
            self.parse_callgraph(filename)

    def parse_callgraph(self, filename):
        """Generate a dictionary of FunctionNodes corresponding to the data
        from a graphviz digraph"""
        
        call = re.compile(".*->.*;")
        callgraph = open(filename, "r")
        for line in callgraph:
            hit = call.match(line)
            if not hit: continue
            
            caller = hit.group().split('"')[1]
            callee = hit.group().split('"')[3]
            if not callee in self.nodes:
                func_file, func_name = callee.split(":")
                self.nodes[callee] = FunctionNode(func_file, func_name)
            if not caller in self.nodes:
                func_file, func_name = caller.split(":")
                self.nodes[caller] = FunctionNode(func_file, func_name)
            self.nodes[caller].add_callee(self.nodes[callee])

    def topological_sort(self, root):
        """Generate a topological sort of the call graph starting from leaf
        nodes and ending with root specified by root"""

        root_node = self.nodes[root]

        if len(root_node.called_funcs) == 0:
            root_node.proto_string = "Leaf"
        elif root_node in self.loop_check:
            i = 1
            while self.loop_check[-i] != root_node:
                self.loop_check[-i].proto_string = "Strong"
                i = i + 1
            self.loop_check[-i].proto_string = "Strong"

        else:
            self.loop_check.append(root_node)
            for new_node in root_node.called_funcs:
                if new_node.proto_string == "":
                    self.topological_sort(new_node.get_name())
            self.loop_check.pop()
            # If node is not strongly connected...
            if root_node.proto_string == "":
                root_node.proto_string = "Visited"

    def __repr__(self):
        return "\n".join([str(value) for (key, value) in self.nodes.iteritems()])

            


if __name__ == "__main__":
    
    if len(sys.argv) == 1:
        cg = CallGraph("test.dot") 
        print cg
        print
        cg.topological_sort(":c")
        print cg
        print
        cg.topological_sort(":b")
        print cg
        print
    else:
        cg = CallGraph(sys.argv[1])
        for (key, _) in cg.nodes.iteritems():
            cg.topological_sort(key)
        print cg


