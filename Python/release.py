####################################################################
##
## release.py
##
##   A cross-platform parser library.
##
##   Web:     imparse.org
##
##

####################################################################
## Assembly of module for release.

rstr = '## RELEASE CONTENT BEGINS HERE #####################################'

imparse = ""
imparse += open('working/imparse.py', 'r').read()
imparse += open('working/genGrammar.py', 'r').read().split(rstr)[1]
imparse += open('working/generate.py', 'r').read().split(rstr)[1]
open('release/imparse/imparse.py', 'w').write(imparse)

#eof
