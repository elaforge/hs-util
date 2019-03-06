#!/usr/bin/env python
from __future__ import print_function
import sys, os, subprocess, glob, re

# For some reason ghc doesn't append versions to these binaries.
# 'fix' will put versions on them.
unversioned = ['hsc2hs', 'hpc']
binaries = ['runghc', 'ghc', 'ghci', 'ghc-pkg', 'haddock-ghc'] + unversioned

def main():
    prefix = os.environ.get('prefix', '/usr/local')
    versions = get_versions()
    try:
        mode = modes[sys.argv[1]]
        version = sys.argv[2]
    except (IndexError, KeyError) as exc:
        print('usage: %s %s version' % (sys.argv[0], '|'.join(modes)))
        print('versions:', ' '.join(versions))
        return 1
    if version not in versions:
        print('unknown version:', version)
        print('valid versions:', ' '.join(versions))

    ver = with_version(version)
    ghc = join(prefix, 'bin', ver('ghc'))
    if not os.path.exists(ghc):
        print('%s does\'t exist, is the version right?' % (ghc,))
        return 1
    return mode(prefix, ver)

def fix(prefix, ver):
    os.chdir(join(prefix, 'bin'))
    cmds = []
    for f in unversioned:
        if not os.path.islink(f):
            cmds.append(['mv', f, ver(f)])
            cmds.append(['ln', '-s', ver(f), f])
    ask(cmds)
    return 0

ln = ['ln', '-shf']

def set_version(prefix, ver):
    os.chdir(join(prefix, 'bin'))
    for f in unversioned:
        if not os.path.islink(f):
            print(f, 'is unversioned')
            return 1
    cmds = [ln + [ver(f), f] for f in binaries]
    # ver('haddock-ghc') is already linked to 'haddock-ghc'.
    cmds.append(ln + ['haddock-ghc', 'haddock'])
    cabal = join(os.environ['HOME'], '.cabal')
    cmds.append(ln + [
        '/usr/local/share/doc/%s/html' %(ver('ghc'),),
        join(cabal, 'ghc-html')
    ])
    [doc] = [
        fn for fn in os.listdir(join(cabal, 'share/doc'))
        if fn.endswith(ver('ghc'))
    ]
    cmds.append(ln + [doc, join(cabal, 'share/doc/current')])
    ask(cmds)

def rm(prefix, ver):
    rms = []
    bin = join(prefix, 'bin')
    lib = join(prefix, 'lib')
    rms = [
        subprocess.check_output([ver('ghc'), '--print-libdir']).strip(),
        join(lib, ver('ghc')),
        join('/usr/local/share', ver('ghc')),
    ]

    rms.extend(join(bin, ver(f)) for f in binaries)
    cabal = concat([
        glob.glob(join(os.environ['HOME'], '.cabal', sub, ver('*-ghc')))
        for sub in ['lib', 'share', 'share/doc']
    ])
    rms.extend(cabal)
    packages = glob.glob(join(os.environ['HOME'], '.ghc', ver('*')))
    rms.extend(packages)
    run(['du', '-hsc'] + rms, fail_ok=True)
    if raw_input('type "rm -rf" to remove: ') == 'rm -rf':
        for f in rms:
            run(['rm', '-rf', f])
    else:
        print('not removing')

modes = {'fix': fix, 'set': set_version, 'rm': rm}

join = os.path.join

def ask(cmds):
    if not cmds:
        print('no cmds to run')
        return
    for c in cmds:
        print(' '.join(c))
    if raw_input('ok? ') == 'y':
        for c in cmds:
            run(c)

def run(cmd, fail_ok=False):
    code = subprocess.call(cmd)
    if not fail_ok and code != 0:
        raise Exception('%s returned %s' % (' '.join(cmd), code))

def get_versions():
    matches = filter(None,
        (re.match(r'ghc-([0-9].*)', fn)
            for fn in os.listdir('/usr/local/bin')))
    return set(m.group(1) for m in matches)

def with_version(version):
    return lambda f: f + '-' + version

def concat(xs):
    out = []
    for x in xs:
        out.extend(x)
    return out

try:
    raw_input
except NameError:
    raw_input = input # python3 renamed raw_input to input


if __name__ == '__main__':
    sys.exit(main())
