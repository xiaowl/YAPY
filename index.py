'''
Index a dict. Using a Leveled alphabet order.

Leveled alphabet order:
(It's a coined term, as I don't know if there is any other term to describe this)
Give a list of pinyin codes:
feng-sha
fei-shuang
fei-huang
fei-teng

their order is defined as:
set i(code index), j(char index) to 0, 0
for j from 0 to LEAD:
    for i from 0 to CODE:
        compare the j char from i code
        if equal then continue
        else return the compared order

So after sort, the order of previous example will be
fei-huang
fei-shuang
feng-sha
fei-teng

Note that fei-teng is after feng-sha, instead of before it using traditional alphabet way.
This makes fuzzy matching "f-s" more efficiency as pinyin with same prefixes are grouped together.

By default, the CODE and LEAD will both be 2, which means
we only index first 2 char from each code in the first 2 code from a pinyin

According to the rules of pinyin, this will effectively covers ~10000 combinations
which makes the final index can easily be bi-searched to find any combination within
~10 steps.
'''

import re
import sys

_PY = re.compile(r"^[a-z-]+$")
def cleanup(dt, limit=8):
    # only keep [a-z-]+
    lines, char = [], 'a'

    for line in dt:
        line = line.strip()
        if line[0] in "#;":
            continue

        data = line.split(" ", 1)
        if len(data) == 1:
            continue

        pinyin, words = data
        if not _PY.match(pinyin):
            continue

        codes = pinyin.split("-")
        if len(codes) > limit:
            continue
        if pinyin[0] == char:
            lines.append((codes, words))
        else:
            yield lines
            lines = [(codes, words)]
            char = pinyin[0]
    yield lines


def leveled_sort(left, right, code=2, lead=2):
    def main_sort():
        for i in range(lead):
            for j in range(code):
                A, B = None, None
                if j < len(left):
                    A = left[j]
                if j < len(right):
                    B = right[j]

                if not (A and B):
                    break

                a, b = None, None
                if i < len(A):
                    a = A[i]
                if i < len(B):
                    b = B[i]

                if a > b:
                    return 1
                if a < b:
                    return -1

        return 0

    def sub_sort():
        if len(left) < len(right):
            return -1
        if len(left) > len(right):
            return 1
        for a, b in zip(left, right):
            if a < b:
                return -1
            if a > b:
                return 1
        return 0

    r = main_sort()
    if r == 0:
        return sub_sort()
    return r





def build_index(dt, code=2, lead=2):
    wcount = 0
    for chunk in cleanup(dt):
        indexes = [[("",) * code, -1, -1]]
        result = []
        chunk.sort(lambda a, b:leveled_sort(a[0], b[0], code, lead))
        for codes, words in chunk:
            pinyin = "-".join(codes)
            size = len(pinyin) + len(words.decode("utf8")) + 2 # '-' and '\n'
            result.append((pinyin, words))
            index = tuple(c[:lead] for c in codes[:code])
            if len(index) < code:
                index += ("",) * (code - len(index))
            if index != indexes[-1][0]:
                indexes.append([index, wcount, wcount + size])
            else:
                indexes[-1][-1] += size
            wcount += size
        yield result[0][0][0], indexes[1:], result



_index_header = '''
;;; A dict index for YAPY (Yet Another PinYin) IME for Emacs
;;; See the index.py for more details.
'''

_index_defvar_start = '''
(setq YAPY-global-index '('''
_index_defvar_end = '''))
'''
def main():
    import sys
    indx_out = open("gen_dict.el", "w")
    dict_out = open("gen_dict.txt", "w")
    print >>indx_out, _index_header
    print >>indx_out, _index_defvar_start
    for label, indexes, dit in build_index(open(sys.argv[1]).readlines()):
        print "Indexing...", label, len(dit), "lines"
        for (index, start, end) in indexes:
            print >>indx_out, '  ("%s" "%s" %d %d)' % (index[0], index[1], start, end)
        for codes, words in dit:
            print >>dict_out, codes, words

    print >>indx_out, _index_defvar_end
    indx_out.close()
    dict_out.close()

if __name__ == "__main__":
    main()


