import argparse
import numpy as np
import subprocess as sp
from os.path import join, dirname
from os import makedirs


span = ord('Z')-ord('1')+1
alph = np.arange(0, span, dtype=int)
alph_str = ''.join(chr(c + ord('1')) for c in alph)
error_alph = np.arange(ord('[')-ord('1'), ord('}')-ord('1')+1, dtype=int)


def revert(chrs):
    return ''.join(chr(c + ord('1')) for c in chrs)


def make_LR():
    P = np.array(alph)
    np.random.shuffle(P)    
    return P


def make_T():
    Tsh = make_LR()
    T = np.zeros(span, dtype=int)
    for i in range(0, span, 2):
        T[Tsh[i]] = Tsh[i+1]
        T[Tsh[i+1]] = Tsh[i]
        
    if np.random.rand(1) < 0.05:
        np.random.shuffle(T)
    
    return T


def make_K():
    return np.random.choice(alph, 2)


def make_In(mean):
    std = 0.2
    lo = np.rint((1-std)*mean)
    hi = np.rint((1+std)*mean)
    ilen = int(np.rint(np.clip(np.random.normal(mean, std*mean), lo, hi)))
    return np.random.choice(alph, ilen)


def mess(P):
    rolls = np.random.rand(3)

    if rolls[0] < 0.02:
        total = int(np.ceil(0.2*len(P)))
        errors = np.random.choice(error_alph, size=total)
        placements = np.random.choice(np.arange(0, len(P)), size=total)

        P[placements] = errors

    if rolls[1] < 0.02:
        lo = np.floor(0.8*len(P))
        hi = np.ceil(1.2*len(P))
        newlen = int(np.rint(np.clip(np.random.normal(len(P), 0.2*len(P)), lo, hi)))
        P.resize(newlen, refcheck=False)

    if rolls[2] < 0.02:
        total = int(np.ceil(0.2*len(P)))
        from_idx = np.random.choice(np.arange(0, len(P)), size=total)
        to_idx = np.random.choice(np.arange(0, len(P)), size=total)

        P[to_idx] = P[from_idx]


def gen_sample(mean):
    L = make_LR()
    mess(L)

    R = make_LR()
    mess(R)

    T = make_T()
    mess(T)

    K = make_K()
    mess(K)

    In = make_In(mean)
    mess(In)

    return [revert(chrs) for chrs in [L, R, T, K]], revert(In)


def run(prog, args, text):
    process = sp.Popen([prog, *args], stdin=sp.PIPE, stdout=sp.PIPE, text=True)
    encrypted = process.communicate(input=text)
    return process.returncode, encrypted if isinstance(encrypted, str) else encrypted[0]


def all_equal(lst):
    return len(lst) == 0 or lst.count(lst[0]) == len(lst)


def verify_LR(LR):
    return ''.join(sorted(LR)) == alph_str


def verify_T(T):
    if not ''.join(sorted(T)) == alph_str:
        return False

    Tsh = [ord(c) - ord('1') for c in T]
    return all(Tsh[Tsh[i]] == i for i in range(len(Tsh)))


def verify_K(K):
    return all(ord('1') <= ord(c) <= ord('Z') for c in K)


def verify_In(In):
    return all(ord('1') <= ord(c) <= ord('Z') for c in In)


def verify(L, R, T, K, In):
    log = []
    if not verify_LR(L):
        log.append('L')
    if not verify_LR(R):
        log.append('R')
    if not verify_T(T):
        log.append('T')
    if not verify_K(K):
        log.append('K')
    if not verify_In(In):
        log.append('In')

    return len(log) == 0, ' '.join(log)


def make_parser():
    desc = 'Generate samples for DCL programs.'
    parser = argparse.ArgumentParser('gen', description=desc)

    parser.add_argument('-n', '--nsamples', type=int, default=100,
        help='number of samples for the test')
    parser.add_argument('-l', '--inputlen', type=int, default=100,
        help='mean input length for the test (normal, std=0.2, '
             'cutoff at +-0.2)')

    subparsers = parser.add_subparsers(dest='mode')

    imm_parser = subparsers.add_parser('imm', help='immediate benchmark mode')
    imm_parser.add_argument('progs', nargs='*',
        help='paths to tested programs')

    fs_parser = subparsers.add_parser('fs', help='test-file mode')
    fs_parser.add_argument('outdir',
        help='output directory for tests')
    fs_parser.add_argument('-s', '--schema', default='{}',
        help='test-naming schema in Python format (blanks are replaced with 0,1..)')

    return parser


def main():
    parser = make_parser()
    args = parser.parse_args()

    for sample in range(args.nsamples):
        arg, In = gen_sample(args.inputlen)
        if args.mode == 'imm':
            reslist = [run(prog, arg, In) for prog in args.progs]

            if not all_equal(reslist):
                validity, log = verify(*arg, In)
                print('Sample #{}:\n'
                    'Args: {}\n'
                    'Input: {}\n'
                    'Validity: {} [{}]'.format(sample, arg, In, validity, log))
                for i, prog in enumerate(args.progs):
                    res, out = reslist[i]
                    print('\tProgram {}:\n'
                        '\tReturn code: {}\n'
                        '\tOutput: {}'.format(prog, res, out))
        else:
            name = join(args.outdir, args.schema.format(sample))
            makedirs(dirname(name), exist_ok=True)
            open(name + '.key', 'w').write('{} {} {} {}'.format(*arg))
            open(name + '.a', 'w').write(In)


if __name__ == '__main__':
    main()
