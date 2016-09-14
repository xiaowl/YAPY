class Trie:
    def __init__(self, val=None):
        self.val = val
        self.term = False
        self.leaves = {} 

    def add(self, seq):
        if not seq:
            return

        v, t = seq[0], seq[1:]
        n = self.leaves.get(v)
        if not n:
            n = Trie(v)
            self.leaves[v] = n
        n.term = n.term or not t
        n.add(t)

    def exist(self, seq):
        if not seq:
            return True
        v, t = seq[0], seq[1:]
        n = self.leaves[v]
        if n:
            return n.exist(t)
        return False


    def dump(self):
        if not self.leaves:
            return (self.val, self.term), []
        return (self.val, self.term), [node.dump() for node in self.leaves.values()]

def dump(state, depth, ignore=False):
    expr = []
    root, suffix = state[0], state[1]
    if not ignore:
        expr.append('("%s"' % (root[0]))
        if suffix:
            expr.append('\n')
    if suffix:
        expr.append(' ' * depth * 2)
        if not ignore:
            expr.append('(')
        for i, s in enumerate(suffix):
            expr.extend(dump(s, depth + 1))
            expr.append(' ' * depth * 2)
        expr = expr[:-2]
        if not ignore:
            expr.append(')')
    expr.append(")")
    expr.append("\n")
    return expr

import sys

pinyins = [line.strip() for line in open(sys.argv[1]).readlines()]

t = Trie()
for pinyin in pinyins:
    t.add(pinyin)

for pinyin in pinyins:
    if not t.exist(pinyin):
        print pinyin, "WTF"

_def = """(setq YAPY-pinyin-state
  '(
%s
)
"""
print _def % ''.join(dump(t.dump(), 2, True))

