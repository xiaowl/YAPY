import string
import random

random.seed(20160829)

def gen_index(n, prefix=2, points=2):
    index = []
    code = []
    c = 0
    while True:
        for i in range(points):
            for j in range(prefix):
                code.append(random.choice(string.ascii_lowercase))
            index.append("".join(code))
            code = []
        yield index
        index = []
        c += 1
        if c == n:
            break


defvar_open = '''
;; This index file is built by indexer.py automatically, DO NOT EDIT MANUALLY.
;; The index is a multiple-level prefix index, rules:
;; First sort by the first index code's first character, if equal, try second index code's first character
;; If all match, then try each code's second character.
;; Thus, this index makes matching pinyin codes effectively
;; - (a b)
;; - (ax b)
;; - (ax bx)
;; WARNING:
;; It's not good at (and might work incorrectly) if searching:
;; - (a bx)

(defvar yp-index
  '('''
defvar_close = '''  )
)'''

def index_cmp(a, b, c, w):
    '''
    A customized sorter which build indexes to make search
    - a b*
    - ax b*
    effectively.
    Limitation:
    - a bx
    '''
    for j in range(w):
        for i in range(c):
            ai, bi = a[i], b[i]
            if ai[j] == bi[j]:
                continue
            return ai[j] < bi[j] and -1 or 1
    return -1 

if __name__ == "__main__":
    import sys
    indexes = list(gen_index(int(sys.argv[1])))
    indexes.sort(cmp=lambda a, b: index_cmp(a, b, 2, 2))
    print defvar_open
    for index in indexes:
        print '    ("%s" "%s")' % tuple(index)
    print defvar_close
