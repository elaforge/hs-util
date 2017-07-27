#!/usr/bin/env python
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
    except (IndexError, KeyError), exc:
        print 'usage: %s %s version' % (sys.argv[0], '|'.join(modes))
        print 'versions: ', ' '.join(versions)
        return 1
    if version not in versions:
        print 'unknown version:', version
        print 'valid versions: ', ' '.join(versions)

    ver = with_version(version)
    ghc = join(prefix, 'bin', ver('ghc'))
    if not os.path.exists(ghc):
        print '%s does\'t exist, is the version right?' % (ghc,)
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

def set_version(prefix, ver):
    os.chdir(join(prefix, 'bin'))
    for f in unversioned:
        if not os.path.islink(f):
            print f, 'is unversioned'
            return 1
    cmds = [['ln', '-sf', ver(f), f] for f in binaries]
    cmds.append(['ln', '-sf', 'haddock-ghc', 'haddock'])
    ask(cmds)

def rm(prefix, ver):
    rms = []
    bin = join(prefix, 'bin')
    lib = join(prefix, 'lib')
    rms = [
        subprocess.check_output([ver('ghc'), '--print-libdir']).strip(),
        join(lib, ver('ghc')),
    ]

    rms.extend(join(bin, ver(f)) for f in binaries)
    cabal = [
        glob.glob(join(os.environ['HOME'], '.cabal', sub, ver('*-ghc')))
        for sub in ['lib', 'share', 'share/doc']
    ]
    rms.extend(c for c in cabal)
    run(['du', '-hsc'] + rms, fail_ok=True)
    if raw_input('type "rm -rf" to remove: ') == 'rm -rf':
        for f in rms:
            run(['rm', '-rf', f])

modes = {'fix': fix, 'set': set_version, 'rm': rm}


join = os.path.join

def ask(cmds):
    if not cmds:
        print 'no cmds to run'
        return
    for c in cmds:
        print ' '.join(c)
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


if __name__ == '__main__':
    sys.exit(main())
